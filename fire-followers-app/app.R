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
library(leafsync)
library(tidyr)
library(stringr)

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
      /* Add white background to sidebarPanel */
      .well.sidebar {
        background-color: white;
      }
      /* Force maps to be side by side */
      #sync-maps {
        display: flex !important;
        flex-direction: row !important;
        gap: 10px;
      }
      #map_pre, #map_post {
        flex: 1;
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
          class = "sidebar",
          # Fire-related filters
          wellPanel(
            h4("Fire Filters"),
            # New searchable, multi-select input for fires.
            selectizeInput("fire_select", "Select Fire(s)",
              choices = fire_names,
              selected = NULL,
              multiple = TRUE,
              options = list(placeholder = "Search for fires")
            ),
            # Render time slider dynamically with fire alarm date in the label.
            uiOutput("time_range_ui")
          ),

          # Observation-related filters
          wellPanel(
            h4("Observation Filters"),
            # Single selectizeInput for taxonomic filtering across all levels.
            selectizeInput("taxa_filter", "Taxa Filter",
              choices = NULL,
              options = list(
                placeholder = "Search by class, family, genus, or species",
                render = I('{
                   option: function(item, escape) {
                     return "<div style=\'padding: 0px 8px\'>" + item.label + "</div>";
                   },
                   item: function(item, escape) {
                     return "<div style=\'padding: 0px 8px\'>" + item.label + "</div>";
                   }
                 }')
              ),
              multiple = FALSE
            ),
            # Use radioGroupButtons for Display Count toggle
            radioGroupButtons(
              inputId = "count_type",
              label = "Display Count:",
              choices = c("Species" = "Total Species", "Observations" = "Total Observations"),
              selected = "Total Species",
              justified = TRUE,
              status = "primary"
            ),
            # New checkbox toggle for Relative Proportion (default off)
            checkboxInput("relative_prop", "Display Relative Proportion", value = FALSE)
          )
        ),
        mainPanel(
          # Replace collapsible panels with tabsetPanel
          tabsetPanel(
            id = "richness_tabs",
            tabPanel(
              "Fire Insights Map",
              fluidRow(
                column(
                  12,
                  h4("Pre-fire vs Post-fire Comparison"),
                  htmlOutput("sync_maps_ui", height = "600px")
                )
              )
            ),
            tabPanel(
              "Species Richness Plot",
              plotOutput("richnessPlot")
            ),
            tabPanel(
              "Top 10 Species",
              plotOutput("top10Plot", height = "600px")
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
    if (is.null(input$fire_select) || length(input$fire_select) == 0) {
      shinyjs::runjs("$('#tabs li a:contains(\"Species Richness\")').addClass('disabled').css({'pointer-events': 'none', 'opacity': '0.5'});")
    } else {
      shinyjs::runjs("$('#tabs li a:contains(\"Species Richness\")').removeClass('disabled').css({'pointer-events': 'auto', 'opacity': '1'});")
    }
  })

  # Render the time range slider UI using the alarm date from the first selected fire.
  output$time_range_ui <- renderUI({
    req(input$fire_select)
    # Use the first selected fire
    selected_fire_name <- input$fire_select[1]
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
    req(input$fire_select)
    selected_fire_name <- input$fire_select[1]
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
    fire_centroids <- st_centroid(all_fires())
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

  # When a fire is clicked on the map, update the species richness fire select input.
  observeEvent(input$fireMap_marker_click, {
    click <- input$fireMap_marker_click
    selected_fire_name <- click$id
    print(paste("Fire selected from map:", selected_fire_name))
    # Combine the new fire with any existing selection (ensuring uniqueness)
    current <- isolate(input$fire_select)
    new_selection <- unique(c(current, selected_fire_name))
    updateSelectizeInput(session, "fire_select", selected = new_selection)
    # Automatically switch to the Species Richness tab.
    updateTabsetPanel(session, "tabs", selected = "Species Richness")
  })

  # Cache the fire data since it's expensive to compute
  selected_fire <- reactive({
    req(input$fire_select)
    print(paste("selected_fire: Triggered for fires =", paste(input$fire_select, collapse = ", ")))
    fire_info <- con |>
      tbl("frap") %>%
      filter(FIRE_NAME %in% input$fire_select) %>%
      group_by(FIRE_NAME) %>%
      slice_max(GIS_ACRES, with_ties = FALSE) %>%
      collect()
    print("selected_fire: Fire info collected from DuckDB.")

    fire_sf <- fire_info %>%
      mutate(geometry = st_as_sfc(geom_wkt, crs = 4326) %>% st_zm(drop = TRUE)) %>%
      st_as_sf() %>%
      st_make_valid()

    # Handle complex geometries
    if (any(st_geometry_type(fire_sf) == "GEOMETRYCOLLECTION")) {
      fire_sf <- fire_sf %>%
        rowwise() %>%
        mutate(geometry = if (st_geometry_type(geometry) == "GEOMETRYCOLLECTION") {
          st_collection_extract(geometry, "POLYGON") %>% st_union()
        } else {
          geometry
        }) %>%
        ungroup()
    }

    print("selected_fire: Converted fire info to sf object.")
    fire_sf
  }) %>% bindCache(input$fire_select)

  # Cache the species points data (filtered by taxa)
  species_points <- reactive({
    req(input$fire_select, input$taxa_filter)
    print(paste("species_points: Retrieving species observations for fires =", paste(input$fire_select, collapse = ", ")))
    spp_query <- tbl(con, "gbif_frap") %>%
      filter(
        FIRE_NAME %in% input$fire_select,
        !is.na(latitude),
        !is.na(longitude)
      )
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
  }) %>% bindCache(input$fire_select, input$taxa_filter)

  # New reactive for all species points (without taxon filtering)
  all_species_points <- reactive({
    req(input$fire_select)
    print(paste("all_species_points: Retrieving all species observations for fires =", paste(input$fire_select, collapse = ", ")))
    spp_query <- tbl(con, "gbif_frap") %>%
      filter(
        FIRE_NAME %in% input$fire_select,
        !is.na(latitude),
        !is.na(longitude)
      )
    spp_df <- spp_query %>%
      select(
        id, latitude, longitude, observed_on, scientific_name,
        taxon_class_name, taxon_family_name, taxon_genus_name, taxon_species_name
      ) %>%
      collect()
    if (nrow(spp_df) == 0) {
      return(NULL)
    }
    st_as_sf(spp_df, coords = c("longitude", "latitude"), crs = 4326)
  }) %>% bindCache(input$fire_select)

  # Separate the species grid data by period
  species_grid_by_period <- reactive({
    req(input$count_type, input$time_range)
    print("species_grid_by_period: Triggered.")
    fire_sf <- selected_fire()
    spp_sf <- species_points()
    req(fire_sf, spp_sf)

    # Get fire date
    fire_info <- con %>%
      tbl("frap") %>%
      filter(FIRE_NAME == !!input$fire_select[1]) %>%
      collect()
    fire_date <- as.Date(fire_info$ALARM_DATE[1])

    # Split species points into pre and post fire (filtered)
    spp_pre <- spp_sf %>% filter(as.Date(observed_on) < fire_date)
    spp_post <- spp_sf %>% filter(as.Date(observed_on) >= fire_date)

    # Create grids for both periods
    fire_bbox <- st_bbox(fire_sf)
    cell_size <- max(fire_bbox$xmax - fire_bbox$xmin, fire_bbox$ymax - fire_bbox$ymin) / 20

    grid <- st_make_grid(fire_sf, cellsize = cell_size, square = TRUE)
    grid_sf <- st_sf(geometry = grid) %>%
      st_make_valid() %>%
      st_intersection(fire_sf %>% select(geometry))

    # Handle any complex geometries in the grid
    if (any(st_geometry_type(grid_sf) == "GEOMETRYCOLLECTION")) {
      grid_sf <- grid_sf %>%
        rowwise() %>%
        mutate(geometry = if (st_geometry_type(geometry) == "GEOMETRYCOLLECTION") {
          st_collection_extract(geometry, "POLYGON") %>% st_union()
        } else {
          geometry
        }) %>%
        ungroup()
    }

    # Calculate counts for both periods with a helper function
    count_points <- function(grid, points) {
      if (input$count_type == "Total Observations") {
        lengths(st_intersects(grid, points))
      } else {
        sapply(st_intersects(grid, points), function(idxs) {
          if (length(idxs) == 0) {
            return(0)
          }
          length(unique(points$scientific_name[idxs]))
        })
      }
    }

    grid_pre <- grid_sf
    grid_post <- grid_sf
    grid_pre$count <- count_points(grid_pre, spp_pre)
    grid_post$count <- count_points(grid_post, spp_post)

    # If relative proportion is requested, recalc counts using all (unfiltered) points.
    if (input$relative_prop) {
      req(all_species_points())
      all_pts <- all_species_points()
      spp_pre_all <- all_pts %>% filter(as.Date(observed_on) < fire_date)
      spp_post_all <- all_pts %>% filter(as.Date(observed_on) >= fire_date)
      total_pre <- count_points(grid_pre, spp_pre_all)
      total_post <- count_points(grid_post, spp_post_all)
      grid_pre$count <- ifelse(total_pre > 0, grid_pre$count / total_pre, 0)
      grid_post$count <- ifelse(total_post > 0, grid_post$count / total_post, 0)
    }

    list(pre = grid_pre, post = grid_post, fire = fire_sf)
  }) %>% bindCache(input$fire_select, input$taxa_filter, input$count_type, input$time_range, input$relative_prop)

  # Create a shared color palette reactive
  shared_palette <- reactive({
    req(species_grid_by_period())
    data <- species_grid_by_period()
    # Combine counts from both periods to get full range
    all_counts <- c(data$pre$count, data$post$count)
    colorNumeric("YlOrRd", domain = log1p(all_counts))
  })

  # Render pre-fire and post-fire maps with synced view.
  output$sync_maps_ui <- renderUI({
    req(species_grid_by_period())
    print("Creating and syncing maps")
    data <- species_grid_by_period()

    # Create shared color palette
    all_counts <- c(data$pre$count, data$post$count)
    pal <- if (input$relative_prop) {
      colorNumeric("YlOrRd", domain = all_counts)
    } else {
      colorNumeric("YlOrRd", domain = log1p(all_counts))
    }

    # Pre-fire map
    pre_label <- if (input$count_type == "Total Observations") {
      if (input$relative_prop) {
        ~ paste("Pre-fire Relative Proportion:", count)
      } else {
        ~ paste("Pre-fire Obs. Count:", count)
      }
    } else {
      if (input$relative_prop) {
        ~ paste("Pre-fire Relative Proportion:", count)
      } else {
        ~ paste("Pre-fire Species Count:", count)
      }
    }
    pre_map <- leaflet() %>%
      addTiles() %>%
      addControl(
        html = "<h4 style='text-align:center; margin:0; background-color:white; padding:5px; border-radius:3px; box-shadow:0 0 5px rgba(0,0,0,0.2);'>Pre-Fire</h4>",
        position = "topright"
      ) %>%
      addPolygons(
        data = data$fire,
        color = "red",
        weight = 2,
        fill = FALSE
      ) %>%
      addPolygons(
        data = data$pre,
        fillColor = ~ if (input$relative_prop) pal(count) else pal(log1p(count)),
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        label = pre_label,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      )

    # Post-fire map
    post_label <- if (input$count_type == "Total Observations") {
      if (input$relative_prop) {
        ~ paste("Post-fire Relative Proportion:", count)
      } else {
        ~ paste("Post-fire Obs. Count:", count)
      }
    } else {
      if (input$relative_prop) {
        ~ paste("Post-fire Relative Proportion:", count)
      } else {
        ~ paste("Post-fire Species Count:", count)
      }
    }
    post_map <- leaflet() %>%
      addTiles() %>%
      addControl(
        html = "<h4 style='text-align:center; margin:0; background-color:white; padding:5px; border-radius:3px; box-shadow:0 0 5px rgba(0,0,0,0.2);'>Post-Fire</h4>",
        position = "topright"
      ) %>%
      addPolygons(
        data = data$fire,
        color = "red",
        weight = 2,
        fill = FALSE
      ) %>%
      addPolygons(
        data = data$post,
        fillColor = ~ if (input$relative_prop) pal(count) else pal(log1p(count)),
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        label = post_label,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = if (input$relative_prop) all_counts else log1p(all_counts),
        labFormat = labelFormat(transform = if (input$relative_prop) identity else function(x) round(expm1(x), 0)),
        title = if (input$relative_prop) {
          "Relative Proportion"
        } else if (input$count_type == "Total Observations") {
          "Observation Count"
        } else {
          "Species Count"
        }
      )

    # Synchronized maps
    sync_maps <- leafsync::sync(list(pre_map, post_map))
    sync_maps
  }) %>% bindCache(input$fire_select, input$taxa_filter, input$count_type, input$time_range, input$relative_prop)

  # --- Species Richness Data and Plot ---------------------------------------
  richness_data <- reactive({
    req(input$count_type, input$fire_select)
    print("richness_data: Calculating species richness data.")
    # Use the alarm date from the first selected fire.
    fire_info <- con %>%
      tbl("frap") %>%
      filter(FIRE_NAME == !!input$fire_select[1]) %>%
      collect()
    fire_date <- as.Date(fire_info$ALARM_DATE[1])
    print(paste("richness_data: Fire alarm date =", fire_date))
    start_date <- as.Date(input$time_range[1])
    end_date <- as.Date(input$time_range[2])
    print(paste("richness_data: Selected date range =", start_date, "to", end_date))
    selected_fire_names <- input$fire_select

    # Function to conditionally apply the taxon filter
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

    # Filtered counts (based on taxa selection)
    total_pre <- if (start_date < fire_date) {
      qry <- tbl(con, "gbif_frap") %>%
        filter(
          FIRE_NAME %in% selected_fire_names,
          observed_on >= start_date,
          observed_on < fire_date
        )
      qry <- apply_taxon_filter(qry)
      qry %>%
        summarize(val = if (input$count_type == "Total Species") n_distinct(scientific_name) else n()) %>%
        collect() %>%
        pull(val)
    } else {
      0
    }
    total_post <- if (end_date >= fire_date) {
      qry <- tbl(con, "gbif_frap") %>%
        filter(
          FIRE_NAME %in% selected_fire_names,
          observed_on >= fire_date,
          observed_on <= end_date
        )
      qry <- apply_taxon_filter(qry)
      qry %>%
        summarize(val = if (input$count_type == "Total Species") n_distinct(scientific_name) else n()) %>%
        collect() %>%
        pull(val)
    } else {
      0
    }

    # Unfiltered totals (all taxa) for the same period
    total_pre_all <- if (start_date < fire_date) {
      tbl(con, "gbif_frap") %>%
        filter(
          FIRE_NAME %in% selected_fire_names,
          observed_on >= start_date,
          observed_on < fire_date
        ) %>%
        summarize(val = if (input$count_type == "Total Species") n_distinct(scientific_name) else n()) %>%
        collect() %>%
        pull(val)
    } else {
      0
    }
    total_post_all <- if (end_date >= fire_date) {
      tbl(con, "gbif_frap") %>%
        filter(
          FIRE_NAME %in% selected_fire_names,
          observed_on >= fire_date,
          observed_on <= end_date
        ) %>%
        summarize(val = if (input$count_type == "Total Species") n_distinct(scientific_name) else n()) %>%
        collect() %>%
        pull(val)
    } else {
      0
    }

    # If relative proportion is on, compute ratio; otherwise use filtered raw numbers.
    if (input$relative_prop) {
      value_pre <- if (total_pre_all > 0) total_pre / total_pre_all else 0
      value_post <- if (total_post_all > 0) total_post / total_post_all else 0
      df <- data.frame(
        Period = c("Pre-fire", "Post-fire"),
        Count = c(value_pre, value_post)
      )
    } else {
      df <- data.frame(
        Period = c("Pre-fire", "Post-fire"),
        Count = c(total_pre, total_post)
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
        y = ifelse(input$relative_prop, "Relative Proportion", ifelse(input$count_type == "Total Species", "Number of Unique Species", "Number of Observations")),
        x = ""
      )
    p
  })

  # --- Top 10 Species Data and Plot -----------------------------------------
  top10_data <- reactive({
    req(input$count_type, input$fire_select)
    print("top10_data: Calculating top 10 species data.")

    # Get fire date from first selected fire
    fire_info <- con %>%
      tbl("frap") %>%
      filter(FIRE_NAME == !!input$fire_select[1]) %>%
      collect()
    fire_date <- as.Date(fire_info$ALARM_DATE[1])
    start_date <- as.Date(input$time_range[1])
    end_date <- as.Date(input$time_range[2])
    selected_fire_names <- input$fire_select

    # Base query with fire and date filters
    base_query <- tbl(con, "gbif_frap") %>%
      filter(FIRE_NAME %in% selected_fire_names)

    # Apply taxa filter if specified
    if (!is.null(input$taxa_filter) && input$taxa_filter != "" && input$taxa_filter != "all") {
      split_val <- strsplit(input$taxa_filter, "\\|")[[1]]
      level <- split_val[1]
      value <- split_val[2]
      base_query <- switch(level,
        "class" = base_query %>% filter(taxon_class_name == value),
        "family" = base_query %>% filter(taxon_family_name == value),
        "genus" = base_query %>% filter(taxon_genus_name == value),
        "species" = base_query %>% filter(taxon_species_name == value),
        base_query
      )
    }

    # Get pre-fire data
    pre_fire <- if (start_date < fire_date) {
      base_query %>%
        filter(
          observed_on >= start_date,
          observed_on < fire_date
        ) %>%
        group_by(scientific_name) %>%
        summarize(
          count = if (input$count_type == "Total Species") n_distinct(scientific_name) else n(),
          .groups = "drop"
        ) %>%
        collect()
    } else {
      data.frame(scientific_name = character(0), count = numeric(0))
    }
    pre_fire$period <- "Pre-fire"

    # Get post-fire data
    post_fire <- if (end_date >= fire_date) {
      base_query %>%
        filter(
          observed_on >= fire_date,
          observed_on <= end_date
        ) %>%
        group_by(scientific_name) %>%
        summarize(
          count = if (input$count_type == "Total Species") n_distinct(scientific_name) else n(),
          .groups = "drop"
        ) %>%
        collect()
    } else {
      data.frame(scientific_name = character(0), count = numeric(0))
    }
    post_fire$period <- "Post-fire"

    # Combine and get top 10 species across both periods
    combined <- bind_rows(pre_fire, post_fire)
    top_species <- combined %>%
      group_by(scientific_name) %>%
      summarize(total_count = sum(count)) %>%
      arrange(desc(total_count)) %>%
      slice_head(n = 10) %>%
      pull(scientific_name)

    # Filter for only top 10 species and ensure both periods are represented
    result <- combined %>%
      filter(scientific_name %in% top_species) %>%
      complete(scientific_name = top_species, period = c("Pre-fire", "Post-fire"), fill = list(count = 0))

    # If relative proportion is requested, calculate percentages
    if (input$relative_prop) {
      result <- result %>%
        group_by(period) %>%
        mutate(
          total = sum(count),
          count = if (total > 0) count / total else 0
        ) %>%
        select(-total) %>%
        ungroup()
    }

    # Reorder factors for plotting
    result %>%
      mutate(
        scientific_name = factor(scientific_name, levels = top_species),
        period = factor(period, levels = c("Pre-fire", "Post-fire"))
      )
  })

  # Render the Top 10 Plot
  output$top10Plot <- renderPlot({
    req(input$count_type)

    if (input$count_type == "Total Species") {
      # Create a blank plot with just text
      ggplot() +
        annotate("text",
          x = 0.5, y = 0.5,
          label = "Please select 'Observations' from the Display Count options\nin the sidebar to view the top 10 species.",
          size = 6
        ) +
        theme_void() +
        xlim(0, 1) +
        ylim(0, 1)
    } else {
      req(top10_data())
      print("output$top10Plot: Rendering top 10 species plot.")

      period_colors <- c("Pre-fire" = "#68C6C0", "Post-fire" = "#dd5858")

      p <- ggplot(top10_data(), aes(x = scientific_name, y = count, fill = period)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = period_colors) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold"),
          legend.title = element_blank(),
          legend.position = "top"
        ) +
        labs(
          title = "Top 10 Observed Species",
          x = "Species",
          y = if (input$relative_prop) "Relative Proportion" else "Count"
        )

      # If the names are too long, try to wrap them
      if (any(nchar(levels(top10_data()$scientific_name)) > 30)) {
        p <- p + scale_x_discrete(labels = function(x) str_wrap(x, width = 30))
      }

      p
    }
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
