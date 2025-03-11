library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DBI)
library(duckdb)
library(dplyr)
library(dbplyr)
library(sf)
library(leaflet)
library(ggplot2)
library(patchwork)
library(purrr)

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
  # Custom CSS to add more padding to taxa_filter dropdown text
  tags$head(
    tags$style(HTML("
      .selectize-control .selectize-input {
        padding: 8px;
      }
    "))
  ),
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

  # Tabset: 'Select Fire' and 'Species Richness'
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "Select Fire",
      leafletOutput("fireMap", height = 600)
    ),
    tabPanel(
      "Species Richness",
      sidebarLayout(
        # Decrease sidebar width by setting width = 3
        sidebarPanel(
          width = 3,
          # Render time slider dynamically with fire alarm date in the label.
          uiOutput("time_range_ui"),
          # Use radioGroupButtons for Display Count toggle
          radioGroupButtons(
            inputId = "count_type",
            label = "Display Count:",
            choices = c("Total Species", "Total Observations"),
            selected = "Total Species",
            justified = TRUE,
            status = "primary"
          ),
          # Single selectizeInput for taxonomic filtering across all levels.
          selectizeInput("taxa_filter", "Taxa Filter",
            choices = NULL,
            options = list(
              placeholder = "Search by class, family, genus, or species",
              render = I('{
                 option: function(item, escape) {
                   return "<div>" + item.label + "</div>";
                 },
                 item: function(item, escape) {
                   return "<div>" + item.label + "</div>";
                 }
               }')
            ),
            multiple = FALSE
          )
        ),
        mainPanel(
          # Collapsible panels using Bootstrap collapse.
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
  # Observer to disable/enable the Species Richness tab based on fire selection.
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
        "Select date range for observations", "<br>",
        "<span style='font-weight:normal; font-style:italic;'>Fire alarm date:", alarm_date, "</span>"
      )),
      min = as.Date("2015-01-01"),
      max = Sys.Date(),
      value = c(new_start, new_end),
      step = 30,
      timeFormat = "%Y-%m"
    )
  })

  # Update the taxa_filter selectizeInput with combined choices.
  observe({
    req(input$fire)
    selected_fire_name <- input$fire
    # Query distinct values for each taxonomic level.
    classes <- tbl(con, "gbif_frap") %>%
      filter(FIRE_NAME == selected_fire_name) %>%
      distinct(taxon_class_name) %>%
      collect() %>%
      pull(taxon_class_name)
    families <- tbl(con, "gbif_frap") %>%
      filter(FIRE_NAME == selected_fire_name) %>%
      distinct(taxon_family_name) %>%
      collect() %>%
      pull(taxon_family_name)
    genera <- tbl(con, "gbif_frap") %>%
      filter(FIRE_NAME == selected_fire_name) %>%
      distinct(taxon_genus_name) %>%
      collect() %>%
      pull(taxon_genus_name)
    species <- tbl(con, "gbif_frap") %>%
      filter(FIRE_NAME == selected_fire_name) %>%
      distinct(taxon_species_name) %>%
      collect() %>%
      pull(taxon_species_name)

    # Create a combined list with level prefixes.
    # Labels are of the form "Alnus <span style='color:gray;'>Genus</span>".
    choices <- c(
      setNames("all", "All taxa"),
      setNames(paste0("class|", classes), paste0(classes, " <span style='color:gray;'>Class</span>")),
      setNames(paste0("family|", families), paste0(families, " <span style='color:gray;'>Family</span>")),
      setNames(paste0("genus|", genera), paste0(genera, " <span style='color:gray;'>Genus</span>")),
      setNames(paste0("species|", species), paste0(species, " <span style='color:gray;'>Species</span>"))
    )
    updateSelectizeInput(session, "taxa_filter",
      choices = choices,
      selected = "all",
      server = TRUE
    )
  })

  # Reactive: Get all fires (one record per fire, using the largest fire by GIS_ACRES).
  all_fires <- reactive({
    print("all_fires: Retrieving all fire data.")
    fires_info <- tbl(con, "frap") %>%
      group_by(FIRE_NAME) %>%
      slice_max(GIS_ACRES, with_ties = FALSE) %>%
      collect()
    fires_sf <- fires_info %>%
      mutate(geometry = st_as_sfc(geom_wkt, crs = 4326) %>% st_zm(drop = TRUE)) %>%
      st_as_sf() %>%
      st_make_valid()
    fires_sf
  })

  # --- First Tab: Fire Centroids Map ---------------------------------------
  output$fireMap <- renderLeaflet({
    print("output$fireMap: Rendering fire centroids map.")
    req(all_fires())
    # Compute centroids of all fire polygons.
    fire_centroids <- st_centroid(all_fires())
    # Create custom icon for fire marker
    fireIcons <- awesomeIcons(
      icon = "fire",
      iconColor = "red",
      markerColor = "orange",
      library = "fa"
    )
    leaflet() %>%
      addTiles() %>%
      addAwesomeMarkers(
        data = fire_centroids,
        label = ~FIRE_NAME,
        layerId = ~FIRE_NAME,
        icon = fireIcons
      )
  })

  # Update the hidden input 'fire' when a fire centroid is clicked.
  observeEvent(input$fireMap_marker_click, {
    click <- input$fireMap_marker_click
    selected_fire_name <- click$id
    print(paste("Fire selected from map:", selected_fire_name))
    updateTextInput(session, "fire", value = selected_fire_name)
    # Automatically switch to the Species Richness tab
    updateTabsetPanel(session, "tabs", selected = "Species Richness")
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
      mutate(geometry = st_as_sfc(geom_wkt, crs = 4326) %>% st_zm(drop = TRUE)) %>%
      st_as_sf() %>%
      st_make_valid()
    if (st_geometry_type(fire_sf) == "GEOMETRYCOLLECTION") {
      fire_sf <- fire_sf %>%
        mutate(geometry = st_union(st_collection_extract(geometry, "POLYGON")))
    }
    print("selected_fire: Converted fire info to sf object.")
    fire_sf
  })

  # Reactive: Retrieve species observation points for the selected fire,
  # further filtering by the taxa_filter if specified.
  species_points <- reactive({
    req(input$fire)
    print(paste("species_points: Retrieving species observations for fire =", input$fire))
    selected_fire_name_local <- input$fire
    spp_query <- tbl(con, "gbif_frap") %>%
      filter(
        FIRE_NAME == selected_fire_name_local,
        !is.na(latitude),
        !is.na(longitude)
      )
    # Apply taxon filtering only if a specific taxon is selected.
    if (!is.null(input$taxa_filter) && input$taxa_filter != "" && input$taxa_filter != "all") {
      split_val <- strsplit(input$taxa_filter, "\\|")[[1]]
      level <- split_val[1]
      value <- split_val[2]
      if (level == "class") {
        spp_query <- spp_query %>% filter(taxon_class_name == value)
      } else if (level == "family") {
        spp_query <- spp_query %>% filter(taxon_family_name == value)
      } else if (level == "genus") {
        spp_query <- spp_query %>% filter(taxon_genus_name == value)
      } else if (level == "species") {
        spp_query <- spp_query %>% filter(taxon_species_name == value)
      }
    }
    spp_df <- spp_query %>%
      select(
        id, latitude, longitude, observed_on, scientific_name,
        taxon_class_name, taxon_family_name, taxon_genus_name, taxon_species_name
      ) %>%
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
    req(input$count_type)
    print("species_grid: Triggered.")
    fire_sf <- selected_fire()
    spp_sf <- species_points()
    req(fire_sf, spp_sf)
    fire_bbox <- st_bbox(fire_sf)
    print(paste("species_grid: Fire bounding box =", paste(names(fire_bbox), round(fire_bbox, 2), collapse = ", ")))
    cell_size <- max(fire_bbox$xmax - fire_bbox$xmin, fire_bbox$ymax - fire_bbox$ymin) / 20
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

    if (input$count_type == "Total Observations") {
      counts <- lengths(st_intersects(grid_sf, spp_sf))
    } else { # Total Species
      counts <- sapply(st_intersects(grid_sf, spp_sf), function(idxs) {
        if (length(idxs) == 0) {
          return(0)
        }
        length(unique(spp_sf$scientific_name[idxs]))
      })
    }
    grid_sf$count <- counts
    print("species_grid: Computed species counts for each grid cell.")
    grid_sf
  })

  # --- Fire Insights Map (shown within the Species Richness tab) ------
  output$map <- renderLeaflet({
    req(selected_fire(), species_grid(), input$count_type)
    print("output$map: Rendering fire insights map.")
    fire_sf <- selected_fire()
    centroid <- st_centroid(fire_sf$geometry)
    coords <- st_coordinates(centroid)
    grid_sf <- species_grid()
    pal <- colorNumeric("YlOrRd", domain = log1p(grid_sf$count))

    popup_text <- if (input$count_type == "Total Observations") {
      ~ paste("Obs. Count:", count)
    } else {
      ~ paste("Species Count:", count)
    }
    legend_title <- if (input$count_type == "Total Observations") {
      "Observation Count"
    } else {
      "Species Count"
    }

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
        fillColor = ~ pal(log1p(count)),
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        group = "Grid",
        popup = popup_text
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = log1p(grid_sf$count),
        labFormat = labelFormat(transform = function(x) round(expm1(x), 0)),
        title = legend_title
      )
  })

  # --- Species Richness Data and Plot ---------------------------------------
  richness_data <- reactive({
    req(input$count_type)
    print("richness_data: Calculating species richness data.")
    fire_sf <- selected_fire()
    req(fire_sf)
    fire_date <- as.Date(fire_sf$ALARM_DATE[1])
    print(paste("richness_data: Fire alarm date =", fire_date))
    start_date <- as.Date(input$time_range[1])
    end_date <- as.Date(input$time_range[2])
    print(paste("richness_data: Selected date range =", start_date, "to", end_date))
    selected_fire_name_local <- input$fire

    # Helper function to apply the taxa_filter.
    apply_taxon_filter <- function(qry) {
      if (!is.null(input$taxa_filter) && input$taxa_filter != "" && input$taxa_filter != "all") {
        split_val <- strsplit(input$taxa_filter, "\\|")[[1]]
        level <- split_val[1]
        value <- split_val[2]
        if (level == "class") {
          qry <- qry %>% filter(taxon_class_name == value)
        } else if (level == "family") {
          qry <- qry %>% filter(taxon_family_name == value)
        } else if (level == "genus") {
          qry <- qry %>% filter(taxon_genus_name == value)
        } else if (level == "species") {
          qry <- qry %>% filter(taxon_species_name == value)
        }
      }
      qry
    }

    # Total species richness pre-fire.
    total_pre <- if (start_date < fire_date) {
      qry <- tbl(con, "gbif_frap") %>%
        filter(
          FIRE_NAME == selected_fire_name_local,
          observed_on >= start_date,
          observed_on < fire_date
        )
      qry <- apply_taxon_filter(qry)
      qry %>%
        summarize(richness = n_distinct(scientific_name)) %>%
        collect() %>%
        pull(richness)
    } else {
      0
    }
    print(paste("richness_data: Total pre-fire richness =", total_pre))

    # Total species richness post-fire.
    total_post <- if (end_date >= fire_date) {
      qry <- tbl(con, "gbif_frap") %>%
        filter(
          FIRE_NAME == selected_fire_name_local,
          observed_on >= fire_date,
          observed_on <= end_date
        )
      qry <- apply_taxon_filter(qry)
      qry %>%
        summarize(richness = n_distinct(scientific_name)) %>%
        collect() %>%
        pull(richness)
    } else {
      0
    }
    print(paste("richness_data: Total post-fire richness =", total_post))

    # Total observation counts pre-fire.
    obs_pre <- if (start_date < fire_date) {
      qry <- tbl(con, "gbif_frap") %>%
        filter(
          FIRE_NAME == selected_fire_name_local,
          observed_on >= start_date,
          observed_on < fire_date
        )
      qry <- apply_taxon_filter(qry)
      qry %>%
        summarize(obs = n()) %>%
        collect() %>%
        pull(obs)
    } else {
      0
    }
    obs_post <- if (end_date >= fire_date) {
      qry <- tbl(con, "gbif_frap") %>%
        filter(
          FIRE_NAME == selected_fire_name_local,
          observed_on >= fire_date,
          observed_on <= end_date
        )
      qry <- apply_taxon_filter(qry)
      qry %>%
        summarize(obs = n()) %>%
        collect() %>%
        pull(obs)
    } else {
      0
    }
    print("richness_data: Finished calculating richness data.")

    if (input$count_type == "Total Species") {
      df <- data.frame(
        Period = c("Pre-fire", "Post-fire"),
        Count = c(total_pre, total_post)
      )
    } else {
      df <- data.frame(
        Period = c("Pre-fire", "Post-fire"),
        Count = c(obs_pre, obs_post)
      )
    }
    df <- df %>% mutate(Period = factor(Period, levels = c("Pre-fire", "Post-fire")))
    df
  })

  # --- Render the Richness Plot -----------------------------------------------
  output$richnessPlot <- renderPlot({
    req(richness_data())
    print("output$richnessPlot: Rendering species richness plot.")
    rd <- richness_data()
    period_colors <- c("Pre-fire" = "#68C6C0", "Post-fire" = "#dd5858")
    p <- ggplot(rd, aes(x = Period, y = Count, fill = Period)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = period_colors) +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold")
      ) +
      labs(
        title = ifelse(input$count_type == "Total Species", "Total Species Richness", "Total Observations"),
        y = ifelse(input$count_type == "Total Species", "Number of Unique Species", "Number of Observations"),
        x = ""
      )
    p
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
