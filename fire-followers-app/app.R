# app.R

library(shiny)
library(shinyjs)
library(DBI)
library(duckdb)
library(dplyr)
library(dbplyr)
library(sf)
library(leaflet)
library(ggplot2)
library(patchwork)

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
  useShinyjs(), # initialize shinyjs
  titlePanel("CNPS Fire Followers Explorer"),

  # JavaScript to prevent clicks on disabled tabs
  tags$script(HTML(
    "
    $(document).on('click', '#tabs li a.disabled', function(e) {
      e.preventDefault();
      return false;
    });
    "
  )),

  # Hidden input to store the selected fire (updated by clicking the map)
  tags$div(
    style = "display:none;",
    textInput("fire", "Selected Fire", value = "")
  ),

  # Tabset now has just two tabs: 'Select Fire' and 'Species Richness'
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "Select Fire",
      leafletOutput("fireMap", height = 600)
    ),
    tabPanel(
      "Species Richness",
      sidebarLayout(
        sidebarPanel(
          # Render sliderInput dynamically with fire alarm date in the label
          uiOutput("time_range_ui")
        ),
        mainPanel(
          # Collapsible panels using Bootstrap collapse
          div(
            class = "panel-group", id = "accordion",
            # Panel for species richness plot
            div(
              class = "panel panel-default",
              div(
                class = "panel-heading",
                h4(
                  class = "panel-title",
                  a("Species Richness Plot (click to expand)",
                    `data-toggle` = "collapse", href = "#collapseOne"
                  )
                )
              ),
              div(
                id = "collapseOne", class = "panel-collapse collapse", # starts collapsed
                div(
                  class = "panel-body",
                  plotOutput("richnessPlot")
                )
              )
            ),
            # Panel for fire insights map
            div(
              class = "panel panel-default",
              div(
                class = "panel-heading",
                h4(
                  class = "panel-title",
                  a("Fire Insights Map (click to expand)",
                    `data-toggle` = "collapse", href = "#collapseTwo"
                  )
                )
              ),
              div(
                id = "collapseTwo", class = "panel-collapse collapse", # starts collapsed
                div(
                  class = "panel-body",
                  leafletOutput("map", height = 600)
                )
              )
            )
          )
        )
      )
    )
  )
)

# --- Define the Server -------------------------------------------------------
server <- function(input, output, session) {
  # Observer to disable/enable tabs based on fire selection
  observe({
    if (is.null(input$fire) || input$fire == "") {
      shinyjs::runjs("$('#tabs li a:contains(\"Species Richness\")').addClass('disabled').css({'pointer-events': 'none', 'opacity': '0.5'});")
    } else {
      shinyjs::runjs("$('#tabs li a:contains(\"Species Richness\")').removeClass('disabled').css({'pointer-events': 'auto', 'opacity': '1'});")
    }
  })

  # Render the time range slider UI with the fire alarm date in the label.
  output$time_range_ui <- renderUI({
    req(input$fire)
    selected_fire_name <- input$fire
    fire_info <- con %>%
      tbl("frap") %>%
      filter(FIRE_NAME == selected_fire_name) %>%
      collect()
    if (nrow(fire_info) == 0) {
      return(NULL)
    }
    alarm_date <- as.Date(fire_info$ALARM_DATE[1])
    new_start <- alarm_date - (365 * 4)
    new_end <- alarm_date + (365 * 4)
    sliderInput("time_range",
      label = HTML(paste(
        "Select date range for species richness", "<br>",
        "<span style='font-weight:normal; font-style:italic;'>Fire time:", alarm_date, "</span>"
      )),
      min = as.Date("2015-01-01"),
      max = Sys.Date(),
      value = c(new_start, new_end),
      step = 30,
      timeFormat = "%Y-%m"
    )
  })

  # Reactive: Get all fires (one record per fire, using the largest fire by GIS_ACRES)
  all_fires <- reactive({
    print("all_fires: Retrieving all fire data.")
    fires_info <- tbl(con, "frap") %>%
      group_by(FIRE_NAME) %>%
      slice_max(GIS_ACRES, with_ties = FALSE) %>%
      collect()

    fires_sf <- fires_info %>%
      mutate(geometry = st_as_sfc(geom_wkt, crs = 4326)) %>%
      st_as_sf() %>%
      st_make_valid()

    fires_sf
  })

  # --- First Tab: Fire Centroids Map ---------------------------------------
  output$fireMap <- renderLeaflet({
    print("output$fireMap: Rendering fire centroids map.")
    req(all_fires())
    # Compute centroids of all fire polygons
    fire_centroids <- st_centroid(all_fires())
    leaflet() %>%
      addTiles() %>%
      addMarkers(
        data = fire_centroids,
        label = ~FIRE_NAME,
        layerId = ~FIRE_NAME
      )
  })

  # Update the hidden input 'fire' when a fire centroid is clicked
  observeEvent(input$fireMap_marker_click, {
    click <- input$fireMap_marker_click
    selected_fire_name <- click$id
    print(paste("Fire selected from map:", selected_fire_name))
    updateTextInput(session, "fire", value = selected_fire_name)
  })

  # Reactive: Get fire info for the selected fire as an sf data frame.
  selected_fire <- reactive({
    req(input$fire)
    print(paste("selected_fire: Triggered for fire =", input$fire))
    selected_fire_name <- input$fire
    fire_info <- con |>
      tbl("frap") %>%
      filter(FIRE_NAME == selected_fire_name) %>%
      slice_max(GIS_ACRES, with_ties = FALSE) %>%
      collect()

    print("selected_fire: Fire info collected from DuckDB.")

    fire_sf <- fire_info %>%
      mutate(geometry = st_as_sfc(geom_wkt, crs = 4326)) %>%
      st_as_sf() %>%
      st_make_valid()

    if (st_geometry_type(fire_sf) == "GEOMETRYCOLLECTION") {
      fire_sf <- fire_sf %>%
        mutate(geometry = st_union(st_collection_extract(geometry, "POLYGON")))
    }

    print("selected_fire: Converted fire info to sf object.")
    fire_sf
  })

  # Reactive: Retrieve species observation points for the selected fire.
  species_points <- reactive({
    req(input$fire)
    print(paste("species_points: Retrieving species observations for fire =", input$fire))
    selected_fire_name <- input$fire
    spp_df <- tbl(con, "gbif_frap") %>%
      filter(
        FIRE_NAME == selected_fire_name,
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
    fire_bbox <- st_bbox(fire_sf)
    print(paste(
      "species_grid: Fire bounding box =",
      paste(names(fire_bbox), round(fire_bbox, 2), collapse = ", ")
    ))
    # Use fixed grid resolution of 20 cells along the longest dimension
    cell_size <- max(
      fire_bbox$xmax - fire_bbox$xmin,
      fire_bbox$ymax - fire_bbox$ymin
    ) / 20
    print(paste("species_grid: Calculated cell size =", cell_size))
    grid <- st_make_grid(fire_sf, cellsize = cell_size, square = TRUE)

    grid_sf <- st_sf(geometry = grid) %>%
      st_make_valid() %>%
      st_intersection(fire_sf %>% select(geometry))

    if ("GEOMETRYCOLLECTION" %in% unique(st_geometry_type(grid_sf))) {
      grid_sf <- grid_sf %>%
        rowwise() %>%
        mutate(geometry = if_else(
          st_geometry_type(geometry) == "GEOMETRYCOLLECTION",
          st_union(st_collection_extract(geometry, "POLYGON")),
          geometry
        )) %>%
        ungroup()
    }

    print(paste("species_grid: Number of grid cells after intersection =", nrow(grid_sf)))
    counts <- lengths(st_intersects(grid_sf, spp_sf))
    grid_sf$count <- counts
    print("species_grid: Computed species counts for each grid cell.")
    grid_sf
  })

  # --- Fire Insights Map (now shown within the Species Richness tab) ------
  output$map <- renderLeaflet({
    req(selected_fire(), species_grid())
    print("output$map: Rendering fire insights map.")
    fire_sf <- selected_fire()
    centroid <- st_centroid(fire_sf$geometry)
    coords <- st_coordinates(centroid)
    grid_sf <- species_grid() %>%
      mutate(log_count = log1p(count))

    pal <- colorNumeric("YlOrRd", domain = grid_sf$log_count)

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
      ) %>%
      addPolygons(
        data = grid_sf,
        fillColor = ~ pal(log_count),
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        group = "Grid",
        popup = ~ paste("Obs. Count:", count)
      )
  })

  ## --- Species Richness Data and Plot ---------------------------------------
  richness_data <- reactive({
    print("richness_data: Calculating species richness data.")
    fire_sf <- selected_fire()
    req(fire_sf)

    fire_date <- as.Date(fire_sf$ALARM_DATE[1])
    print(paste("richness_data: Fire alarm date =", fire_date))

    start_date <- as.Date(input$time_range[1])
    end_date <- as.Date(input$time_range[2])
    print(paste("richness_data: Selected date range =", start_date, "to", end_date))

    selected_fire_name <- input$fire

    # Total species richness pre-fire.
    total_pre <- if (start_date < fire_date) {
      tbl(con, "gbif_frap") %>%
        filter(
          FIRE_NAME == selected_fire_name,
          observed_on >= start_date,
          observed_on < fire_date
        ) %>%
        summarize(richness = n_distinct(scientific_name)) %>%
        collect() %>%
        pull(richness)
    } else {
      0
    }
    print(paste("richness_data: Total pre-fire richness =", total_pre))

    # Total species richness post-fire.
    total_post <- if (end_date >= fire_date) {
      tbl(con, "gbif_frap") %>%
        filter(
          FIRE_NAME == selected_fire_name,
          observed_on >= fire_date,
          observed_on <= end_date
        ) %>%
        summarize(richness = n_distinct(scientific_name)) %>%
        collect() %>%
        pull(richness)
    } else {
      0
    }
    print(paste("richness_data: Total post-fire richness =", total_post))

    total_df <- data.frame(
      Period   = c("Pre-fire", "Post-fire"),
      Richness = c(total_pre, total_post)
    ) %>%
      mutate(Period = factor(Period, levels = c("Pre-fire", "Post-fire")))

    # Species richness breakdown by taxon_class_name.
    class_pre <- if (start_date < fire_date) {
      tbl(con, "gbif_frap") %>%
        filter(
          FIRE_NAME == selected_fire_name,
          observed_on >= start_date,
          observed_on < fire_date
        ) %>%
        group_by(taxon_class_name) %>%
        summarize(Richness = n_distinct(scientific_name)) %>%
        collect()
    } else {
      data.frame(taxon_class_name = character(), Richness = integer())
    }
    class_pre$Period <- "Pre-fire"

    class_post <- if (end_date >= fire_date) {
      tbl(con, "gbif_frap") %>%
        filter(
          FIRE_NAME == selected_fire_name,
          observed_on >= fire_date,
          observed_on <= end_date
        ) %>%
        group_by(taxon_class_name) %>%
        summarize(Richness = n_distinct(scientific_name)) %>%
        collect()
    } else {
      data.frame(taxon_class_name = character(), Richness = integer())
    }
    class_post$Period <- "Post-fire"

    class_df <- rbind(class_pre, class_post) %>%
      mutate(Period = factor(Period, levels = c("Pre-fire", "Post-fire")))

    print("richness_data: Finished calculating richness data.")

    list(total = total_df, class = class_df)
  })

  # --- Render the Richness Plot -----------------------------------------------
  output$richnessPlot <- renderPlot({
    print("output$richnessPlot: Rendering species richness plot.")
    rd <- richness_data()
    req(rd)

    period_colors <- c(
      "Pre-fire" = "#68C6C0",
      "Post-fire" = "#dd5858"
    )

    p_total <- ggplot(rd$total, aes(x = Period, y = Richness, fill = Period)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = period_colors) +
      theme_minimal() +
      labs(
        title = "Total Species Richness",
        y = "Number of Unique Species",
        x = ""
      )

    p_class <- ggplot(rd$class, aes(x = taxon_class_name, y = Richness, fill = Period)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = period_colors) +
      theme_minimal() +
      labs(
        title = "Species Richness by Taxon Class",
        y = "Number of Unique Species",
        x = "Taxon Class"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
