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
scon <- dbConnect(duckdb(dbdir = "shiny/data/shiny.db"))
scon |> dbExecute("INSTALL spatial; LOAD spatial;")

# Attach the working database instead of copying it
scon |> dbExecute("ATTACH '/home/ahill/Projects/fire-followers/data/working-db.db' AS workingdb;")

# Select sample fires
select_fires <- c("GLASS", "CZU LIGHTNING COMPLEX", "BOBCAT", "CREEK", "DOLAN")

# Move FRAP over
scon |> dbExecute(
  sprintf("CREATE TABLE shiny.frap AS
(SELECT * FROM workingdb.frap
WHERE FIRE_NAME IN ('%s'))
", paste(select_fires, collapse = "','"))
)

# Copy GBIF points over
scon |> dbExecute(
  sprintf("CREATE OR REPLACE TABLE shiny.gbif_frap AS
(SELECT * FROM workingdb.gbif_frap
WHERE FIRE_NAME IN ('%s'))
", paste(select_fires, collapse = "','"))
)

# Verify the new tables in shiny.db
scon |> dbExecute("DETACH workingdb")
scon |> dbListTables()

# Disconnect from the database
scon |> dbDisconnect(shutdown = TRUE)
