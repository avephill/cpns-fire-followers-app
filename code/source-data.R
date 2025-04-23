library(duckdb)
library(tidyverse)
library(sf)
library(dbplyr)


dcon <- dbConnect(duckdb(dbdir = "data/working-db.db"))
dcon |> dbExecute("install spatial; load spatial;")

# Rename lower case
read_csv("data/observations-523544.csv") |>
  rename_with(tolower) |>
  write_csv("data/observations.csv")

dcon |> dbExecute("
CREATE OR REPLACE TABLE gbif AS
SELECT *, ST_Point(longitude, latitude) AS geom
FROM read_csv('data/observations.csv');")

dcon |> tbl("gbif")

# Read FRAP into duckdb
frap <- st_read("data/firep21_2.gpkg")
frap2020 <- frap |>
  filter(YEAR_ == 2020) |>
  st_transform(4326) |>
  mutate(geom_wkt = st_as_text(geom))

# frap2020 |> st_geometry_type() |> table()

frap2020 |>
  as_tibble() |>
  select(-geom) |>
  write_csv("data/frap.csv")

dcon |> dbExecute("
CREATE OR REPLACE TABLE frap AS
SELECT *, ST_GeomFromText(geom_wkt) AS geom
FROM read_csv('data/frap.csv');")

# Create spatial indices
dbExecute(dcon, "CREATE INDEX gbif_geom_idx ON gbif USING RTREE (geom);")
dbExecute(dcon, "CREATE INDEX frap_geom_idx ON frap USING RTREE (geom);")

dcon |> dbExecute("
CREATE OR REPLACE TABLE gbif_frap AS
SELECT
  g.*,
  f.* EXCLUDE (geom, geom_wkt)
FROM gbif AS g
LEFT JOIN frap AS f
  ON ST_Intersects(f.geom, g.geom);
")

# dcon |>
#   tbl("gbif_frap") |>
#   distinct(FIRE_NAME) |>
#   collect()
# dcon |>
#   tbl("gbif_frap") |>
#   colnames()

dcon |> dbGetQuery("DESCRIBE gbif_frap")
dcon |> dbDisconnect(shutdown = T)

# --- Move to shiny -----------------------------------------------------------
# Connect to a new shiny database
scon <- dbConnect(duckdb(dbdir = "data/shiny.db"))
scon |> dbExecute("INSTALL spatial; LOAD spatial;")

# Attach the working database instead of copying it
scon |> dbExecute("ATTACH '/home/ahill/Projects/fire-followers/data/working-db.db' AS workingdb;")
scon |> tbl("workingdb.frap")
# Select sample fires
# select_fires <- c("GLASS", "CZU LIGHTNING COMPLEX", "BOBCAT", "CREEK", "DOLAN")

# Move FRAP over
scon |> dbExecute(
  sprintf("CREATE TABLE shiny.frap AS
(
SELECT * FROM workingdb.frap
-- WHERE FIRE_NAME IN ('%s')
WHERE GIS_ACRES > 5000
)
", paste(select_fires, collapse = "','"))
)

# Copy GBIF points over
scon |> dbExecute(
  sprintf("CREATE OR REPLACE TABLE shiny.gbif_frap AS
(
SELECT * FROM workingdb.gbif_frap
-- WHERE FIRE_NAME IN ('%s')
WHERE GIS_ACRES > 5000
)
", paste(select_fires, collapse = "','"))
)

# Verify the new tables in shiny.db
scon |> dbExecute("DETACH workingdb")
scon |> dbListTables()

# Add MTBS to local ---------------------------------------
library(terra)
mtbs <- rast("~/Data/Environment/MTBS/MTBS_1984-2021.tif")

mtbs2020 <- mtbs[["mtbs_CA_2020"]]

writeRaster(mtbs2020, "fire-followers-app/data/mtbs_2020.tif")

plot(mtbs2020)

# Add MTBS to gbif_frap table in database ---------------------------------------
library(terra)

# Extract the points from the gbif_frap table
gbif_points <- scon |>
  tbl("gbif_frap") |>
  select(id, longitude, latitude) |>
  collect()

# Create SpatVector from points
points_sf <- st_as_sf(gbif_points, coords = c("longitude", "latitude"), crs = 4326)
points_vect <- vect(points_sf)

# Load the MTBS raster
mtbs2020 <- rast("data/mtbs_2020.tif")

# Extract values from raster at each point
severity_values <- terra::extract(mtbs2020, points_vect)

# Add the fire severity values to the points dataframe
gbif_points$mtbs_fire_severity <- severity_values[, 2] # column 2 should have the values

# Update the database with the new column
# First, create a temporary table with the ID and severity value
dbWriteTable(scon, "temp_severity",
  gbif_points |> select(id, mtbs_fire_severity),
  overwrite = TRUE
)

# Then, update the main table with these values
scon |> dbExecute("
ALTER TABLE gbif_frap ADD COLUMN IF NOT EXISTS mtbs_fire_severity INTEGER;
UPDATE gbif_frap
SET mtbs_fire_severity = t.mtbs_fire_severity
FROM temp_severity t
WHERE gbif_frap.id = t.id;
")

# Clean up temporary table
scon |> dbExecute("DROP TABLE IF EXISTS temp_severity;")

# Verify the new column exists
scon |>
  tbl("gbif_frap") |>
  filter(mtbs_fire_severity == 1, 2) |>
  select(mtbs_fire_severity, scientific_name)

# Disconnect from the database
scon |> dbDisconnect(shutdown = TRUE)

# Huggingface db ---------------------------------------
# Connect to a new shiny database

hcon <- dbConnect(duckdb(dbdir = "data/huggingface.db"))
hcon |> dbExecute("INSTALL spatial; LOAD spatial;")


hcon |> dbExecute("ATTACH 'fire-followers-app/data/shiny.db' AS shinydb;")

hcon |>
  tbl("shinydb.gbif_frap") |>
  count(geoprivacy)

# Move FRAP over
hcon |> dbExecute(
  "CREATE TABLE huggingface.frap AS
(
SELECT * FROM shinydb.frap
WHERE GIS_ACRES > 5000
)"
)


# Copy GBIF points over
hcon |> dbExecute(
  "CREATE OR REPLACE TABLE huggingface.gbif_frap AS
(
SELECT * FROM shinydb.gbif_frap
WHERE GIS_ACRES > 5000
AND geoprivacy != 'obscured'
)"
)

hcon |> dbDisconnect(shutdown = TRUE)
