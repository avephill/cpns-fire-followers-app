# app.R

library(shiny)
library(DBI)
library(duckdb)
library(dplyr)
library(dbplyr)
library(sf)
library(leaflet)
library(ggplot2)
# library(vscDebugger)

# Disable S2 geometry if needed
sf_use_s2(FALSE)

# --- Setup DuckDB Connection -----------------------------------------------
print("Establishing DuckDB connection...")
con <- dbConnect(duckdb::duckdb(), "data/shiny.db")
dbExecute(con, "LOAD 'spatial';")
print("DuckDB connection established and spatial extension loaded.")

# --- Get Fire List Using dbplyr ----------------------------------------------
print("Retrieving distinct fire names from DuckDB...")
fire_names <- tbl(con, "gbif_frap") %>%
  distinct(FIRE_NAME) %>%
  arrange(FIRE_NAME) |>
  collect() |>
  pull(1)
print(paste("Number of fire names retrieved:", length(fire_names)))

# --- Define the UI -----------------------------------------------------------
ui <- fluidPage(
  titlePanel("CNPS Fire Followers Explorer"),
  sidebarLayout(
    sidebarPanel(
      # Use selectizeInput for improved performance with many items
      selectizeInput("fire", "Select Fire",
        choices = unique(fire_names),
        selected = "GLASS",
        options = list(
          placeholder = "Select a fire",
          maxOptions = 1000
        )
      ),
      sliderInput("grid_n",
        "Grid resolution (cells along longest dimension)",
        min = 5, max = 50, value = 20
      ),
      # Replace numeric input with a date slider.
      # Set an initial wide range; it will be updated when a fire is selected.
      sliderInput("time_range",
        "Select date range for species richness",
        min = as.Date("2015-01-01"),
        max = Sys.Date(),
        value = c(as.Date("2015-01-01"), Sys.Date()),
        timeFormat = "%Y-%m-%d"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map", height = 600)),
        tabPanel("Species Richness", plotOutput("richnessPlot"))
      )
    )
  )
)

# --- Define the Server -------------------------------------------------------
server <- function(input, output, session) {
  # Observer: When a fire is selected, update the date slider to center around its ALARM_DATE.
  observe({
    req(input$fire)
    print(paste("updateSlider: Updating date slider for fire =", input$fire))
    # Retrieve fire info from the "frap" table (assumed to hold fire polygon data)
    fire_info <- con |>
      tbl("frap") %>%
      filter(FIRE_NAME == input$fire) |>
      collect()
    if (nrow(fire_info) == 0) {
      print("updateSlider: No fire info found!")
      return()
    }
    alarm_date <- as.Date(fire_info$ALARM_DATE[1])
    new_start <- alarm_date - (365 * 4)
    new_end <- alarm_date + (365 * 4)
    print(paste(
      "updateSlider: Alarm date =", alarm_date,
      "setting slider from", new_start, "to", new_end
    ))
    updateSliderInput(session, "time_range", value = c(new_start, new_end))
  })

  # Reactive: Get fire info for the selected fire as an sf data frame.
  selected_fire <- reactive({
    req(input$fire)
    print(paste("selected_fire: Triggered for fire =", input$fire))
    # Retrieve fire info from DuckDB using dbplyr (from the "frap" table)
    # browser()
    fire_info <- con |>
      tbl("frap") %>%
      filter(FIRE_NAME == input$fire) |>
      # In case there are multiple firs of the same name, just get the biggest one for now
      slice_max(GIS_ACRES) |>
      collect()

    print("selected_fire: Fire info collected from DuckDB.")

    # Convert the WKT string to an sfc geometry (assuming WGS84)
    fire_sf <- fire_info %>%
      mutate(geometry = st_as_sfc(geom_wkt, crs = 4326)) %>%
      st_as_sf() |>
      st_make_valid()

    if (st_geometry_type(fire_sf) == "GEOMETRYCOLLECTION") {
      fire_sf <- fire_sf |>
        mutate(geometry = st_union(st_collection_extract(geometry, "POLYGON")))
    }

    print("selected_fire: Converted fire info to sf object.")
    fire_sf
  })

  # Reactive: Retrieve species observation points for the selected fire using dbplyr.
  species_points <- reactive({
    req(input$fire)
    print(paste("species_points: Retrieving species observations for fire =", input$fire))
    spp_df <- tbl(con, "gbif_frap") %>%
      filter(
        `FIRE_NAME` == input$fire,
        !is.na(latitude),
        !is.na(longitude)
      ) %>%
      select(id, latitude, longitude, observed_on, scientific_name) %>%
      collect()
    print(paste("species_points: Number of species observations retrieved =", nrow(spp_df)))
    if (nrow(spp_df) == 0) {
      print("species_points: No species observations found!")
      return(NULL)
    }
    sfc_points <- st_as_sf(spp_df, coords = c("longitude", "latitude"), crs = 4326)
    print("species_points: Converted species observations to sf object.")
    sfc_points
  })

  # Reactive: Create a grid over the fire polygon and count species observations.
  species_grid <- reactive({
    print("species_grid: Triggered.")
    fire_sf <- selected_fire()
    spp_sf <- species_points()
    req(fire_sf, spp_sf)
    # browser()
    fire_bbox <- st_bbox(fire_sf)
    print(paste(
      "species_grid: Fire bounding box =",
      paste(names(fire_bbox), round(fire_bbox, 2), collapse = ", ")
    ))
    cell_size <- max(
      fire_bbox$xmax - fire_bbox$xmin,
      fire_bbox$ymax - fire_bbox$ymin
    ) / input$grid_n
    print(paste("species_grid: Calculated cell size =", cell_size))
    grid <- st_make_grid(fire_sf, cellsize = cell_size, square = TRUE)

    grid_sf <- st_sf(geometry = grid) |>
      st_make_valid() |>
      st_intersection(fire_sf |> select(geometry))

    if ("GEOMETRYCOLLECTION" %in% unique(grid_sf |> st_geometry_type())) {
      # This turns GEOMETRYCOLLECTIONS into MULTIPOLYGON
      grid_sf <- grid_sf |>
        rowwise() |>
        mutate(geometry = if_else(
          st_geometry_type(geometry) == "GEOMETRYCOLLECTION",
          st_union(st_collection_extract(geometry, "POLYGON")),
          geometry
        )) |>
        ungroup()
    }

    # grid_sf |>
    #   st_geometry_type() |>
    #   unique()
    print(paste("species_grid: Number of grid cells after intersection =", nrow(grid_sf)))
    counts <- lengths(st_intersects(grid_sf, spp_sf))
    grid_sf$count <- counts
    print("species_grid: Computed species counts for each grid cell.")
    grid_sf
  })

  # --- Map Output -------------------------------------------------------------
  output$map <- renderLeaflet({
    print("output$map: Rendering map.")
    fire_sf <- selected_fire()
    req(fire_sf)
    # browser()
    centroid <- st_centroid(fire_sf$geometry)
    coords <- st_coordinates(centroid)
    print(paste("output$map: Map centroid at (", coords[1], ",", coords[2], ")"))
    leaflet() %>%
      addTiles() %>%
      setView(lng = coords[1], lat = coords[2], zoom = 10) %>%
      addPolygons(
        data = fire_sf,
        color = "red",
        weight = 2,
        fill = FALSE,
        group = "Fire",
        popup = ~FIRE_NAME
      )
  })

  # Update the map with the observation density grid.
  observe({
    print("observe: Updating species grid layer on the map.")

    grid_sf <- species_grid()

    req(grid_sf)

    # Apply log transformation to count (adding 1 to avoid log(0))
    grid_sf <- grid_sf %>%
      mutate(log_count = log1p(count)) # log1p(x) = log(x + 1)

    # Define color palette based on log-transformed count
    pal <- colorNumeric("YlOrRd", domain = grid_sf$log_count)

    print("observe: Color palette created for grid cells based on log(count).")

    leafletProxy("map") %>%
      clearGroup("Grid") %>%
      addPolygons(
        data = grid_sf,
        fillColor = ~ pal(log_count), # Use log-transformed count for fill color
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        group = "Grid",
        popup = ~ paste("Obs. Count:", count) # Show raw count in popup
      )

    print("observe: Species grid layer added to the map.")
  })



  # --- Species Richness Panel -----------------------------------------------
  # Compute total species richness and species richness by taxon_class_name
  richness_data <- reactive({
    print("richness_data: Calculating species richness data.")
    fire_sf <- selected_fire()
    req(fire_sf)

    fire_date <- as.Date(fire_sf$ALARM_DATE[1])
    print(paste("richness_data: Fire alarm date =", fire_date))

    # Use the date slider values for the range.
    start_date <- as.Date(input$time_range[1])
    end_date <- as.Date(input$time_range[2])
    print(paste("richness_data: Selected date range =", start_date, "to", end_date))

    ## Total species richness (overall) -----------------------------------------
    # Pre-fire total richness.
    total_pre <- if (start_date < fire_date) {
      tbl(con, "gbif_frap") %>%
        filter(
          `FIRE_NAME` == input$fire,
          observed_on >= start_date,
          observed_on < fire_date
        ) %>%
        summarize(richness = n_distinct(scientific_name)) |>
        collect() |>
        pull(richness)
    } else {
      0
    }
    print(paste("richness_data: Total pre-fire richness =", total_pre))

    # Post-fire total richness.
    total_post <- if (end_date >= fire_date) {
      tbl(con, "gbif_frap") %>%
        filter(
          `FIRE_NAME` == input$fire,
          observed_on >= fire_date,
          observed_on <= end_date
        ) %>%
        summarize(richness = n_distinct(scientific_name)) |>
        collect() |>
        pull(richness)
    } else {
      0
    }
    print(paste("richness_data: Total post-fire richness =", total_post))

    total_df <- data.frame(
      Period   = c("Pre-fire", "Post-fire"),
      Richness = c(total_pre, total_post)
    ) |>
      mutate(Period = factor(Period, levels = c("Pre-fire", "Post-fire")))

    ## Species richness by taxon_class_name -------------------------------
    # Pre-fire breakdown by taxon class.
    class_pre <- if (start_date < fire_date) {
      tbl(con, "gbif_frap") %>%
        filter(
          `FIRE_NAME` == input$fire,
          observed_on >= start_date,
          observed_on < fire_date
        ) %>%
        group_by(taxon_class_name) %>%
        summarize(Richness = n_distinct(scientific_name)) |>
        collect()
    } else {
      data.frame(taxon_class_name = character(), Richness = integer())
    }
    class_pre$Period <- "Pre-fire"

    # Post-fire breakdown by taxon class.
    class_post <- if (end_date >= fire_date) {
      tbl(con, "gbif_frap") %>%
        filter(
          `FIRE_NAME` == input$fire,
          observed_on >= fire_date,
          observed_on <= end_date
        ) %>%
        group_by(taxon_class_name) %>%
        summarize(Richness = n_distinct(scientific_name)) |>
        collect()
    } else {
      data.frame(taxon_class_name = character(), Richness = integer())
    }
    class_post$Period <- "Post-fire"

    class_df <- rbind(class_pre, class_post) |>
      mutate(Period = factor(Period, levels = c("Pre-fire", "Post-fire")))

    print("richness_data: Finished calculating richness data.")

    list(total = total_df, class = class_df)
  })

  # --- Render the Richness Plot -----------------------------------------------
  output$richnessPlot <- renderPlot({
    print("output$richnessPlot: Rendering species richness plot.")
    rd <- richness_data()
    req(rd)

    library(patchwork)

    # Define color mapping
    period_colors <- c(
      "Pre-fire" = "#68C6C0",
      "Post-fire" = "#dd5858" # Firebrick Red (burnt landscape)
    )

    # Plot for total species richness.
    p_total <- ggplot(rd$total, aes(x = Period, y = Richness, fill = Period)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = period_colors) + # Custom colors
      theme_minimal() +
      labs(
        title = "Total Species Richness",
        y = "Number of Unique Species",
        x = ""
      )

    # Plot for species richness by taxon_class_name.
    p_class <- ggplot(rd$class, aes(x = taxon_class_name, y = Richness, fill = Period)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = period_colors) + # Custom colors
      theme_minimal() +
      labs(
        title = "Species Richness by Taxon class",
        y = "Number of Unique Species",
        x = "Taxon class"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # Combine the two plots vertically.
    combined_plot <- p_total / p_class
    combined_plot
  })

  # Close the DuckDB connection when the session ends.
  session$onSessionEnded(function() {
    print("Session ended: Closing DuckDB connection.")
    dbDisconnect(con, shutdown = TRUE)
  })
}

# --- Run the Shiny App -------------------------------------------------------
print("Launching Shiny app...")
shinyApp(ui = ui, server = server)
