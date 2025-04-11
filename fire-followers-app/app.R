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
con <- dbConnect(duckdb::duckdb(), "data/shiny.db", read_only = TRUE)
# con |> dbListTables()
# dbExecute(con, "INSTALL 'spatial'; LOAD 'spatial';")
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
  includeCSS("www/cnps-style.css"),
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
      /* Style for help tooltips */
      .help-tooltip {
        display: inline-block;
        width: 16px;
        height: 16px;
        line-height: 16px;
        text-align: center;
        border-radius: 50%;
        background-color: #e0e0e0;
        color: #666;
        font-size: 12px;
        margin-left: 5px;
        cursor: help;
        position: relative;
        top: -2px;
      }
      .help-tooltip:hover {
        background-color: #d0d0d0;
      }
      .tooltip-text {
        visibility: hidden;
        width: 200px;
        background-color: #555;
        color: #fff;
        text-align: center;
        border-radius: 6px;
        padding: 8px;
        position: absolute;
        z-index: 1;
        bottom: 125%;
        left: 50%;
        margin-left: -100px;
        opacity: 0;
        transition: opacity 0.3s;
        font-size: 14px;
        font-style: normal;
      }
      .tooltip-text::after {
        content: '';
        position: absolute;
        top: 100%;
        left: 50%;
        margin-left: -5px;
        border-width: 5px;
        border-style: solid;
        border-color: #555 transparent transparent transparent;
      }
      .help-tooltip:hover .tooltip-text {
        visibility: visible;
        opacity: 1;
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

  # Tabset: 'Select Fire', 'Biodiversity Insights', and 'About'
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "Select Fire",
      fluidRow(
        column(
          8,
          offset = 2,
          h4("Please select a fire on the map to begin exploring data", style = "text-align: center; margin-top: 20px;"),
          # p("Click on any fire marker to view biodiversity insights for that fire.", style = "text-align: center;"),
          leafletOutput("fireMap", height = 450)
        )
      ),
      br(),
      fluidRow(
        column(10,
          offset = 1,
          br(),
          p("The CNPS Fire Followers Explorer is an interactive application designed to explore plant biodiversity
            patterns before and after wildfire events in California using data from the CNPS California Fire Followers 2020 iNaturalist project. This tool allows users to visualize and analyze
            how plant communities respond to fires of different severities and how species composition changes over time."),
          p(
            "This application was developed as a joint collaboration between the",
            tags$b("California Native Plant Society"), "and",
            tags$b("Avery Hill"), "at the California Academy of Sciences."
          ),
          h3("Data Sources"),
          tags$ul(
            tags$li(
              "Observation data was accessed from the iNaturalist project: ",
              tags$a(
                href = "https://www.inaturalist.org/projects/california-fire-followers-2020-data-management",
                "California Fire Followers 2020", target = "_blank"
              )
            ),
            tags$li(
              "Fire perimeter data came from the California Department of Forestry and Fire Protection: ",
              tags$a(
                href = "https://www.fire.ca.gov/what-we-do/fire-resource-assessment-program/fire-perimeters",
                "Fire Resource Assessment Program", target = "_blank"
              )
            ),
            tags$li(
              "Fire severity data came from the Monitoring Trends in Burn Severity (MTBS) project: ",
              tags$a(
                href = "https://www.mtbs.gov/index.php/project-overview",
                "MTBS Project Overview", target = "_blank"
              )
            )
          ),
          h3("Technical Details"),
          p("This application was built using R Shiny, an open-source framework for building interactive web
            applications. It uses DuckDB for efficient data storage and processing, and various spatial libraries
            for visualization and analysis."),
          p(tags$a(
            href = "https://github.com/avephill/cpns-fire-followers-app",
            "View project on GitHub", target = "_blank"
          )),
          br(),
          hr(),
          p(
            style = "color: #666; font-style: italic;",
            "Created: ", format(Sys.Date(), "%B %Y")
          )
        )
      )
    ),
    tabPanel(
      "Biodiversity Insights",
      sidebarLayout(
        # Decrease sidebar width by setting width = 3
        sidebarPanel(
          width = 3,
          class = "sidebar",
          # Fire-related filters
          wellPanel(
            h4("Fire Filters"),
            # New searchable, multi-select input for fires.
            selectizeInput("fire_select",
              div(
                "Select Fire(s)",
                tags$span(
                  class = "help-tooltip",
                  "?",
                  tags$span(
                    class = "tooltip-text",
                    "You can select multiple fires by clicking or using the search box"
                  )
                )
              ),
              choices = c("All Fires" = "all", setNames(fire_names, fire_names)),
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
            selectizeInput("taxa_filter",
              div(
                "Taxa Filter",
                tags$span(
                  class = "help-tooltip",
                  "?",
                  tags$span(
                    class = "tooltip-text",
                    "You can search and filter by class, family, genus, or species"
                  )
                )
              ),
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
              label = "Show total:",
              choices = c("Species" = "Total Species", "Obs" = "Total Observations"),
              selected = "Total Observations",
              justified = TRUE,
              status = "success",
              size = "sm",
              checkIcon = list(
                yes = icon("check", lib = "font-awesome")
              ),
              width = "100%"
            ),
            # Replace prettySwitch with a better formatted label + description
            tags$div(
              # style = "display: flex; flex-direction: column; align-items: center; gap: 5px;",
              tags$p(style = "margin: 0; font-weight: bold; font-size: 14px;", "Normalize counts:"),
              switchInput(
                inputId = "relative_prop",
                label = "Normalized",
                labelWidth = "100px",
                value = FALSE,
                size = "small"
              ),
              tags$p(
                style = "color: #666; font-size: 0.9em; margin: 0; text-align: center;",
                "(Shows proportion of observations rather than raw counts, useful for comparing across uneven sampling periods)"
              ),
            ),
            # Add HTML output to display the denominator values
            conditionalPanel(
              condition = "input.relative_prop == true",
              htmlOutput("proportion_info", style = "padding-left: 10px; color: #666; font-style: italic;")
            )
          ),

          # Add new wellPanel for data download
          wellPanel(
            h4("Download Data"),
            p("Download the filtered observation data as a CSV file:"),
            downloadButton("downloadData", "Download CSV", class = "btn-primary")
          )
        ),
        mainPanel(
          # Replace collapsible panels with tabsetPanel
          tabsetPanel(
            id = "richness_tabs",
            tabPanel(
              "Density Maps",
              fluidRow(
                column(
                  12,
                  h4("Pre-fire vs Post-fire Comparison"),
                  htmlOutput("sync_maps_ui", height = "600px")
                )
              )
            ),
            tabPanel(
              "Fire Severity Barplot",
              fluidRow(
                column(
                  8,
                  plotOutput("richnessPlot")
                ),
                column(
                  4,
                  wellPanel(
                    h4("MTBS Fire Severity Classes"),
                    tags$ul(
                      tags$li(tags$b("1 - Unburned to Low: "), "Little or no change from pre-fire conditions"),
                      tags$li(tags$b("2 - Low: "), "Surface fires with minimal overstory effects"),
                      tags$li(tags$b("3 - Moderate: "), "Mixture of surface and canopy effects"),
                      tags$li(tags$b("4 - High: "), "Complete or near complete consumption of vegetation")
                    ),
                    p(
                      "Data source: ",
                      tags$a(
                        href = "https://www.mtbs.gov", "Monitoring Trends in Burn Severity (MTBS)",
                        target = "_blank"
                      )
                    )
                  )
                )
              )
            ),
            tabPanel(
              "Top 10 Species",
              plotOutput("top10Plot", height = "600px")
            ),
            tabPanel(
              "Fire Influence",
              plotOutput("fireInfluencePlot", height = "600px")
            )
          )
        )
      )
    )
  )
)


# --- Define the Server -------------------------------------------------------
server <- function(input, output, session) {
  # Observer to disable/enable the Biodiversity Insights tab based on fire selection.
  observe({
    if (is.null(input$fire_select) || length(input$fire_select) == 0) {
      shinyjs::runjs("$('#tabs li a:contains(\"Biodiversity Insights\")').addClass('disabled').css({'pointer-events': 'none', 'opacity': '0.5'});")
    } else {
      shinyjs::runjs("$('#tabs li a:contains(\"Biodiversity Insights\")').removeClass('disabled').css({'pointer-events': 'auto', 'opacity': '1'});")
    }
  })

  # Render the time range slider UI using the alarm date from the first selected fire.
  output$time_range_ui <- renderUI({
    req(input$fire_select)
    # Get all selected fires
    selected_fire_names <- input$fire_select
    fire_info <- con %>%
      tbl("gbif_frap") %>%
      filter(FIRE_NAME %in% selected_fire_names) %>%
      # For each fire, get the record with the largest GIS_ACRES
      group_by(FIRE_NAME) %>%
      slice_max(GIS_ACRES, with_ties = FALSE) %>%
      collect()
    if (nrow(fire_info) == 0) {
      return(NULL)
    }

    # Get all fire names and their alarm dates (now using the largest fire records)
    fire_dates <- fire_info %>%
      select(FIRE_NAME, ALARM_DATE, GIS_ACRES) %>%
      distinct() %>%
      mutate(ALARM_DATE = as.Date(ALARM_DATE)) %>%
      arrange(ALARM_DATE)

    # Format for display
    date_strings <- paste(fire_dates$FIRE_NAME, ":", fire_dates$ALARM_DATE)
    dates_html <- paste(date_strings, collapse = ", ")

    # Use the first fire's date for slider range calculation
    alarm_date <- fire_dates$ALARM_DATE[1]
    new_start <- alarm_date - (365 * 4)
    new_end <- alarm_date + (365 * 4)

    sliderInput("time_range",
      label = HTML(paste(
        "Select date range for observations", "<br>",
        "<span style='font-weight:normal; font-style:italic;'>Fire alarm dates: ", dates_html, "</span>"
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

    # Get taxon names based on whether "all" is selected
    if ("all" %in% input$fire_select) {
      # For "all" fires, get all unique taxon names
      classes <- tbl(con, "gbif_frap") %>%
        distinct(taxon_class_name) %>%
        collect() %>%
        pull(taxon_class_name)
      families <- tbl(con, "gbif_frap") %>%
        distinct(taxon_family_name) %>%
        collect() %>%
        pull(taxon_family_name)
      genera <- tbl(con, "gbif_frap") %>%
        distinct(taxon_genus_name) %>%
        collect() %>%
        pull(taxon_genus_name)
      species <- tbl(con, "gbif_frap") %>%
        distinct(taxon_species_name) %>%
        collect() %>%
        pull(taxon_species_name)
    } else {
      # For specific fires, get taxon names from selected fires
      classes <- tbl(con, "gbif_frap") %>%
        filter(FIRE_NAME %in% input$fire_select) %>%
        distinct(taxon_class_name) %>%
        collect() %>%
        pull(taxon_class_name)
      families <- tbl(con, "gbif_frap") %>%
        filter(FIRE_NAME %in% input$fire_select) %>%
        distinct(taxon_family_name) %>%
        collect() %>%
        pull(taxon_family_name)
      genera <- tbl(con, "gbif_frap") %>%
        filter(FIRE_NAME %in% input$fire_select) %>%
        distinct(taxon_genus_name) %>%
        collect() %>%
        pull(taxon_genus_name)
      species <- tbl(con, "gbif_frap") %>%
        filter(FIRE_NAME %in% input$fire_select) %>%
        distinct(taxon_species_name) %>%
        collect() %>%
        pull(taxon_species_name)
    }

    # Remove NA values
    classes <- classes[!is.na(classes)]
    families <- families[!is.na(families)]
    genera <- genera[!is.na(genera)]
    species <- species[!is.na(species)]

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

  # --- Reactive: Get all fires (one record per fire, using the largest fire by GIS_ACRES).
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

  # New reactive to get fire date based on selected fires
  fire_date <- reactive({
    req(input$fire_select)
    print(paste("fire_date: Getting fire dates for fires =", paste(input$fire_select, collapse = ", ")))

    # Get fire info based on selection
    if ("all" %in% input$fire_select) {
      # For "all" fires, get all fires
      fire_info <- con |>
        tbl("frap") %>%
        group_by(FIRE_NAME) %>%
        slice_max(GIS_ACRES, with_ties = FALSE) %>%
        collect()
    } else {
      # For specific fires, get those fires' info
      fire_info <- con |>
        tbl("frap") %>%
        filter(FIRE_NAME %in% input$fire_select) %>%
        group_by(FIRE_NAME) %>%
        slice_max(GIS_ACRES, with_ties = FALSE) %>%
        collect()
    }

    # Calculate average fire date
    mean(as.Date(fire_info$ALARM_DATE), na.rm = TRUE)
  }) %>% bindCache(input$fire_select)

  # New reactive value to centralize all the common filtering logic
  filtered_gbif_data <- reactive({
    req(input$fire_select, input$time_range)
    print(paste("filtered_gbif_data: Filtering gbif_frap data for fires =", paste(input$fire_select, collapse = ", ")))

    # Get fire date from the fire_date reactive
    f_date <- fire_date()

    # Get the time range values
    start_date <- as.Date(input$time_range[1])
    end_date <- as.Date(input$time_range[2])

    # Base query with date filter
    query <- tbl(con, "gbif_frap") %>%
      filter(
        observed_on >= start_date,
        observed_on <= end_date
      )

    # Add fire filter if not "all"
    if (!("all" %in% input$fire_select)) {
      query <- query %>% filter(FIRE_NAME %in% input$fire_select)
    }

    # Apply taxon filter if specified
    if (!is.null(input$taxa_filter) && input$taxa_filter != "" && input$taxa_filter != "all") {
      split_val <- strsplit(input$taxa_filter, "\\|")[[1]]
      level <- split_val[1]
      value <- split_val[2]

      query <- switch(level,
        "class" = query %>% filter(taxon_class_name == value),
        "family" = query %>% filter(taxon_family_name == value),
        "genus" = query %>% filter(taxon_genus_name == value),
        "species" = query %>% filter(taxon_species_name == value),
        query
      )
    }

    # Check if the query would return an empty result
    count <- query %>%
      summarize(n = n()) %>%
      collect() %>%
      pull(n)

    if (count == 0) {
      # Show a notification if no data is found
      showNotification(
        "No observations found with the current filters. Try adjusting your filters.",
        type = "warning",
        duration = 5
      )
    }

    # Return both the query, fire date, and time range for downstream use
    list(
      query = query,
      fire_date = f_date,
      start_date = start_date,
      end_date = end_date,
      empty = (count == 0)
    )
  }) %>% bindCache(input$fire_select, input$taxa_filter, input$time_range)

  # Also create a reactive for unfiltered data (no taxon filter) for normalization
  unfiltered_gbif_data <- reactive({
    req(input$fire_select, input$time_range)
    print(paste("unfiltered_gbif_data: Filtering gbif_frap data without taxon filter for fires =", paste(input$fire_select, collapse = ", ")))

    # Get fire date from the fire_date reactive
    f_date <- fire_date()

    # Get the time range values
    start_date <- as.Date(input$time_range[1])
    end_date <- as.Date(input$time_range[2])

    # Base query with date filter
    query <- tbl(con, "gbif_frap") %>%
      filter(
        observed_on >= start_date,
        observed_on <= end_date
      )

    # Add fire filter if not "all"
    if (!("all" %in% input$fire_select)) {
      query <- query %>% filter(FIRE_NAME %in% input$fire_select)
    }

    # Check if the query would return an empty result
    count <- query %>%
      summarize(n = n()) %>%
      collect() %>%
      pull(n)

    # Return both the query, fire date, and time range for downstream use
    list(
      query = query,
      fire_date = f_date,
      start_date = start_date,
      end_date = end_date,
      empty = (count == 0)
    )
  }) %>% bindCache(input$fire_select, input$time_range)

  # New reactive values for normalization totals
  total_pre_all_taxa <- reactive({
    req(unfiltered_gbif_data())
    print("total_pre_all_taxa: Calculating total pre-fire counts for normalization.")

    # Get unfiltered data
    unfiltered_data <- unfiltered_gbif_data()

    # Get fire date and start date
    fire_date <- unfiltered_data$fire_date
    start_date <- unfiltered_data$start_date

    # Get pre-fire total across all taxa
    if (start_date < fire_date) {
      unfiltered_data$query %>%
        filter(observed_on < fire_date) %>%
        summarize(val = if (input$count_type == "Total Species") n_distinct(scientific_name) else n()) %>%
        collect() %>%
        pull(val)
    } else {
      0
    }
  }) %>% bindCache(input$fire_select, input$time_range, input$count_type)

  total_post_all_taxa <- reactive({
    req(unfiltered_gbif_data())
    print("total_post_all_taxa: Calculating total post-fire counts for normalization.")

    # Get unfiltered data
    unfiltered_data <- unfiltered_gbif_data()

    # Get fire date and end date
    fire_date <- unfiltered_data$fire_date
    end_date <- unfiltered_data$end_date

    # Get post-fire total across all taxa
    if (end_date >= fire_date) {
      unfiltered_data$query %>%
        filter(observed_on >= fire_date) %>%
        summarize(val = if (input$count_type == "Total Species") n_distinct(scientific_name) else n()) %>%
        collect() %>%
        pull(val)
    } else {
      0
    }
  }) %>% bindCache(input$fire_select, input$time_range, input$count_type)

  # --- First Tab: Fire Centroids Map ---------------------------------------
  output$fireMap <- renderLeaflet({
    print("output$fireMap: Rendering fire centroids map.")
    req(all_fires())
    fire_centroids <- st_centroid(all_fires())

    # Create icons for selected and unselected fires
    fireIcons <- awesomeIcons(
      icon = "fire-flame-curved",
      iconColor = "#FFFFFF",
      markerColor = "#CD5C5C",
      library = "fa"
    )

    selectedFireIcons <- awesomeIcons(
      icon = "fire-flame-curved",
      iconColor = "white",
      markerColor = "blue",
      library = "fa"
    )

    # Create the base map
    map <- leaflet() %>%
      addProviderTiles("CartoDB.Positron")

    # Add markers for unselected fires
    unselected_fires <- fire_centroids
    if (!is.null(input$fire_select) && length(input$fire_select) > 0) {
      unselected_fires <- fire_centroids %>%
        filter(!FIRE_NAME %in% input$fire_select)
    }

    map <- map %>%
      addAwesomeMarkers(
        data = unselected_fires,
        label = ~FIRE_NAME,
        layerId = ~FIRE_NAME,
        icon = fireIcons
      )

    # Add markers for selected fires if any are selected
    if (!is.null(input$fire_select) && length(input$fire_select) > 0) {
      selected_fires <- fire_centroids %>%
        filter(FIRE_NAME %in% input$fire_select)

      map <- map %>%
        addAwesomeMarkers(
          data = selected_fires,
          label = ~FIRE_NAME,
          layerId = ~FIRE_NAME,
          icon = selectedFireIcons
        )
    }

    map
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
    # Automatically switch to the Biodiversity Insights tab.
    updateTabsetPanel(session, "tabs", selected = "Biodiversity Insights")
  })

  # Cache the fire data since it's expensive to compute
  selected_fire <- reactive({
    req(input$fire_select)
    print("selected_fire: Retrieving fire polygons")

    # If "all" is selected, get all fires
    if ("all" %in% input$fire_select) {
      fire_info <- con |>
        tbl("frap") %>%
        group_by(FIRE_NAME) %>%
        slice_max(GIS_ACRES, with_ties = FALSE) %>%
        collect()
    } else {
      fire_info <- con |>
        tbl("frap") %>%
        filter(FIRE_NAME %in% input$fire_select) %>%
        group_by(FIRE_NAME) %>%
        slice_max(GIS_ACRES, with_ties = FALSE) %>%
        collect()
    }

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

    fire_sf
  }) %>% bindCache(input$fire_select)

  # Cache the species points data (filtered by taxa)
  species_points <- reactive({
    req(filtered_gbif_data())
    print("species_points: Using filtered_gbif_data to get species points")

    # Get filtered data
    filtered_data <- filtered_gbif_data()

    # If query is empty based on the flag, return NULL early
    if (filtered_data$empty) {
      return(NULL)
    }

    spp_df <- filtered_data$query %>%
      filter(!is.na(latitude), !is.na(longitude)) %>%
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
  }) %>% bindCache(input$fire_select, input$taxa_filter, input$time_range)

  # New reactive for all species points (without taxon filtering)
  all_species_points <- reactive({
    req(unfiltered_gbif_data())
    print("all_species_points: Using unfiltered_gbif_data to get all species points")

    # Get unfiltered data
    unfiltered_data <- unfiltered_gbif_data()

    # If query is empty based on the flag, return NULL early
    if (unfiltered_data$empty) {
      return(NULL)
    }

    spp_df <- unfiltered_data$query %>%
      filter(!is.na(latitude), !is.na(longitude)) %>%
      select(
        id, latitude, longitude, observed_on, scientific_name,
        taxon_class_name, taxon_family_name, taxon_genus_name, taxon_species_name
      ) %>%
      collect()

    if (nrow(spp_df) == 0) {
      return(NULL)
    }
    st_as_sf(spp_df, coords = c("longitude", "latitude"), crs = 4326)
  }) %>% bindCache(input$fire_select, input$time_range)

  # Update species_grid_by_period to use the new reactive values
  species_grid_by_period <- reactive({
    req(input$count_type, input$time_range, fire_date(), selected_fire())
    print("species_grid_by_period: Triggered.")

    # Get required data
    fire_sf <- selected_fire()
    f_date <- fire_date()

    # Use species_points() to get the filtered species data
    req(species_points())
    spp_sf <- species_points()

    # Split species points into pre and post fire (filtered)
    spp_pre <- spp_sf %>% filter(as.Date(observed_on) < f_date)
    spp_post <- spp_sf %>% filter(as.Date(observed_on) >= f_date)

    # Create grid from fire perimeter
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

    # If relative proportion is requested, use the reactive total values
    if (input$relative_prop) {
      pre_total <- total_pre_all_taxa()
      post_total <- total_post_all_taxa()

      grid_pre$count <- ifelse(pre_total > 0, grid_pre$count / pre_total, 0)
      grid_post$count <- ifelse(post_total > 0, grid_post$count / post_total, 0)
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
    # Make sure we have the necessary inputs
    req(input$fire_select, input$taxa_filter, input$count_type, input$time_range)
    # Get the grid data directly, which will be cached
    data <- species_grid_by_period()
    req(data)

    print("Creating and syncing maps")

    # Create shared color palette
    all_counts <- c(data$pre$count, data$post$count)
    pal <- if (input$relative_prop) {
      colorNumeric(c("#f7f7f7", "#1a4b7c"), domain = all_counts)
    } else {
      colorNumeric(c("#f7f7f7", "#1a4b7c"), domain = log1p(all_counts))
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
    req(input$count_type, filtered_gbif_data(), unfiltered_gbif_data())
    print("richness_data: Calculating species richness data.")

    # Get filtered data
    filtered_data <- filtered_gbif_data()

    # Get necessary values
    fire_date <- filtered_data$fire_date
    start_date <- filtered_data$start_date
    end_date <- filtered_data$end_date

    print(paste("richness_data: Average fire alarm date =", fire_date))
    print(paste("richness_data: Selected date range =", start_date, "to", end_date))

    # Get pre-fire data grouped by severity
    pre_fire_by_severity <- if (start_date < fire_date) {
      # Use the filtered query to get pre-fire data
      counts <- filtered_data$query %>%
        filter(observed_on < fire_date) %>%
        group_by(mtbs_fire_severity) %>%
        summarize(val = if (input$count_type == "Total Species") n_distinct(scientific_name) else n()) %>%
        collect() %>%
        # Ensure mtbs_fire_severity is character type
        mutate(mtbs_fire_severity = as.character(mtbs_fire_severity))

      # Get total across all severities for pre-fire
      total_pre <- filtered_data$query %>%
        filter(observed_on < fire_date) %>%
        summarize(val = if (input$count_type == "Total Species") n_distinct(scientific_name) else n()) %>%
        collect() %>%
        pull(val)

      # Add total row and handle NA values
      counts <- counts %>%
        mutate(mtbs_fire_severity = ifelse(is.na(mtbs_fire_severity), "No Data", mtbs_fire_severity))

      total_row <- data.frame(mtbs_fire_severity = "All", val = total_pre)
      bind_rows(total_row, counts) %>%
        mutate(Period = "Pre-fire")
    } else {
      data.frame(mtbs_fire_severity = character(0), val = numeric(0), Period = character(0))
    }

    # Get post-fire data grouped by severity
    post_fire_by_severity <- if (end_date >= fire_date) {
      # Use the filtered query to get post-fire data
      counts <- filtered_data$query %>%
        filter(observed_on >= fire_date) %>%
        group_by(mtbs_fire_severity) %>%
        summarize(val = if (input$count_type == "Total Species") n_distinct(scientific_name) else n()) %>%
        collect() %>%
        # Ensure mtbs_fire_severity is character type
        mutate(mtbs_fire_severity = as.character(mtbs_fire_severity))

      # Get total across all severities for post-fire
      total_post <- filtered_data$query %>%
        filter(observed_on >= fire_date) %>%
        summarize(val = if (input$count_type == "Total Species") n_distinct(scientific_name) else n()) %>%
        collect() %>%
        pull(val)

      # Add total row and handle NA values
      counts <- counts %>%
        mutate(mtbs_fire_severity = ifelse(is.na(mtbs_fire_severity), "No Data", mtbs_fire_severity))

      total_row <- data.frame(mtbs_fire_severity = "All", val = total_post)
      bind_rows(total_row, counts) %>%
        mutate(Period = "Post-fire")
    } else {
      data.frame(mtbs_fire_severity = character(0), val = numeric(0), Period = character(0))
    }

    # Combine pre and post fire data
    combined_data <- bind_rows(pre_fire_by_severity, post_fire_by_severity) %>%
      rename(Count = val, Severity = mtbs_fire_severity)

    # If relative proportion is requested, use the reactive total values
    if (input$relative_prop) {
      pre_total <- total_pre_all_taxa()
      post_total <- total_post_all_taxa()

      # Calculate relative proportions
      combined_data <- combined_data %>%
        mutate(
          Count = case_when(
            Period == "Pre-fire" & pre_total > 0 ~ Count / pre_total,
            Period == "Post-fire" & post_total > 0 ~ Count / post_total,
            TRUE ~ 0
          )
        )
    }

    # Arrange severity levels with "All" first
    severity_order <- c("All", "1", "2", "3", "4", "5", "6", "No Data")
    combined_data <- combined_data %>%
      mutate(
        Period = factor(Period, levels = c("Pre-fire", "Post-fire")),
        Severity = factor(Severity, levels = severity_order)
      ) %>%
      arrange(Severity, Period)

    combined_data
  })

  # --- Render the Richness Plot -----------------------------------------------
  output$richnessPlot <- renderPlot({
    req(richness_data())
    print("output$richnessPlot: Rendering species richness plot with severity breakdown.")
    # 5 and 6 are just mask values I think. remake them into 'No Data'

    rd <- richness_data() |>
      mutate(Severity = as.character(Severity)) |>
      mutate(Severity = ifelse(Severity %in% c("5", "6", "7", "8"), "No Data", Severity)) |>
      mutate(Severity = factor(Severity, levels = c("All", "1", "2", "3", "4", "No Data")))

    # Define colors for pre-fire and post-fire periods (same as original)
    period_colors <- c("Pre-fire" = "#68C6C0", "Post-fire" = "#dd5858")

    # Create the plot with severity on x-axis and period as fill color
    p <- ggplot(rd, aes(x = Severity, y = Count, fill = Period)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
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
        title = ifelse(input$count_type == "Total Species",
          "Species Richness by Fire Severity",
          "Observations by Fire Severity"
        ),
        y = ifelse(input$relative_prop,
          "Relative Proportion",
          ifelse(input$count_type == "Total Species",
            "Number of Unique Species",
            "Number of Observations"
          )
        ),
        x = "Fire Severity"
      )

    p
  })

  # --- Top 10 Species Data and Plot -----------------------------------------
  top10_data <- reactive({
    req(input$count_type, filtered_gbif_data())
    print("top10_data: Calculating top 10 species data.")

    # Get filtered data
    filtered_data <- filtered_gbif_data()

    # Get necessary values
    fire_date <- filtered_data$fire_date
    start_date <- filtered_data$start_date
    end_date <- filtered_data$end_date

    # Get pre-fire data
    pre_fire <- if (start_date < fire_date) {
      filtered_data$query %>%
        filter(observed_on < fire_date) %>%
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
      filtered_data$query %>%
        filter(observed_on >= fire_date) %>%
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

    # Combine data
    combined <- bind_rows(pre_fire, post_fire)

    # Check if we have any data
    if (nrow(combined) == 0) {
      return(data.frame(
        scientific_name = character(0),
        count = numeric(0),
        period = character(0)
      ))
    }

    # If relative proportion is requested, use the reactive total values
    if (input$relative_prop) {
      pre_total <- total_pre_all_taxa()
      post_total <- total_post_all_taxa()

      # Apply the proportions
      combined <- combined %>%
        mutate(
          count = case_when(
            period == "Pre-fire" & pre_total > 0 ~ count / pre_total,
            period == "Post-fire" & post_total > 0 ~ count / post_total,
            TRUE ~ 0
          )
        )
    }

    # Now find top 10 species based on the potentially normalized counts
    # For normalized counts, we use the sum of proportions for ranking
    # For raw counts, we use the sum of observations
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

  # Update the proportion_info to use the reactive values
  output$proportion_info <- renderUI({
    req(total_pre_all_taxa(), total_post_all_taxa())

    # Get the pre and post totals from the reactive values
    pre_total <- total_pre_all_taxa()
    post_total <- total_post_all_taxa()

    # Determine what type of counts we're showing
    count_type_text <- if (input$count_type == "Total Species") "total species" else "total observations"

    # Create HTML output with the total values
    HTML(paste0(
      "Denominator totals:<br>",
      "Pre-fire: ", pre_total, " ", count_type_text, "<br>",
      "Post-fire: ", post_total, " ", count_type_text
    ))
  })

  # Add new reactive for fire influence data
  fire_influence_data <- reactive({
    req(input$count_type, filtered_gbif_data())
    print("fire_influence_data: Calculating species fire influence data.")

    # Get filtered data
    filtered_data <- filtered_gbif_data()

    # Get necessary values
    fire_date <- filtered_data$fire_date
    start_date <- filtered_data$start_date
    end_date <- filtered_data$end_date

    # Get pre-fire and post-fire counts
    pre_fire <- filtered_data$query %>%
      filter(observed_on < fire_date) %>%
      group_by(scientific_name) %>%
      summarize(
        pre_count = if (input$count_type == "Total Species") n_distinct(scientific_name) else n(),
        .groups = "drop"
      ) %>%
      collect()

    post_fire <- filtered_data$query %>%
      filter(observed_on >= fire_date) %>%
      group_by(scientific_name) %>%
      summarize(
        post_count = if (input$count_type == "Total Species") n_distinct(scientific_name) else n(),
        .groups = "drop"
      ) %>%
      collect()

    # Combine and calculate ratios
    combined <- full_join(pre_fire, post_fire, by = "scientific_name") %>%
      mutate(
        pre_count = replace_na(pre_count, 0),
        post_count = replace_na(post_count, 0)
      ) %>%
      # Filter out species with very few observations (before normalization)
      filter(pre_count + post_count >= 1)

    # Check if we have any data
    if (nrow(combined) == 0) {
      return(data.frame(
        scientific_name = character(0),
        pre_count = numeric(0),
        post_count = numeric(0),
        ratio = numeric(0),
        label = character(0),
        period = character(0),
        count = numeric(0)
      ))
    }

    # If relative proportion is requested, use the reactive total values
    if (input$relative_prop) {
      pre_total <- total_pre_all_taxa()
      post_total <- total_post_all_taxa()

      # Apply normalization
      combined <- combined %>%
        mutate(
          pre_count_norm = if (pre_total > 0) pre_count / pre_total else 0,
          post_count_norm = if (post_total > 0) post_count / post_total else 0,
          # Calculate ratio and absolute difference based on normalized counts
          ratio = (post_count_norm + 0.001) / (pre_count_norm + 0.001),
          diff = post_count_norm - pre_count_norm
        )
    } else {
      # For non-normalized, calculate standard ratio and difference
      combined <- combined %>%
        mutate(
          # Add small constant to avoid division by zero
          ratio = (post_count + 0.1) / (pre_count + 0.1),
          diff = post_count - pre_count
        )
    }

    # Filter for top 10 species
    combined <- combined %>%
      arrange(desc(diff)) %>%
      slice_head(n = 10)

    # Create labels and reshape for plotting
    combined <- combined %>%
      mutate(
        # Create label combining name and ratio
        label = if (input$relative_prop) {
          sprintf("%s\n(+%.3f)", scientific_name, diff)
        } else {
          sprintf("%s\n(+%d)", scientific_name, round(diff))
        },
        # Factor for plotting order
        label = factor(label, levels = label[order(diff)])
      ) %>%
      # Reshape for plotting
      pivot_longer(
        cols = c(pre_count, post_count),
        names_to = "period",
        values_to = "count"
      ) %>%
      mutate(
        period = case_when(
          period == "pre_count" ~ "Pre-fire",
          period == "post_count" ~ "Post-fire"
        ),
        # Make period a factor to ensure correct ordering
        period = factor(period, levels = c("Pre-fire", "Post-fire"))
      )

    # If relative proportion is requested, use the reactive total values for display normalization
    if (input$relative_prop) {
      pre_total <- total_pre_all_taxa()
      post_total <- total_post_all_taxa()

      # Apply the proportions for display
      combined <- combined %>%
        mutate(
          count = case_when(
            period == "Pre-fire" & pre_total > 0 ~ count / pre_total,
            period == "Post-fire" & post_total > 0 ~ count / post_total,
            TRUE ~ 0
          )
        )
    }

    combined
  })

  # Add new plot output (add before the server's closing brace)
  output$fireInfluencePlot <- renderPlot({
    req(fire_influence_data())
    print("output$fireInfluencePlot: Rendering fire influence plot.")

    period_colors <- c("Pre-fire" = "#68C6C0", "Post-fire" = "#dd5858")

    p <- ggplot(
      fire_influence_data(),
      aes(
        x = label,
        y = count,
        fill = factor(period, levels = c("Pre-fire", "Post-fire"))
      )
    ) +
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
        title = "Species Most Influenced by Fire",
        subtitle = "Top 10 species with greatest pre-fire to post-fire change",
        x = "Species (with post- and pre- fire difference)",
        y = if (input$relative_prop) "Relative Proportion" else "Count"
      )

    # If the names are too long, try to wrap them
    if (any(nchar(levels(fire_influence_data()$label)) > 30)) {
      p <- p + scale_x_discrete(labels = function(x) str_wrap(x, width = 30))
    }

    p
  })

  # Add new download handler for filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("fire_followers_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      req(filtered_gbif_data())

      # Get the filtered data
      filtered_data <- filtered_gbif_data()

      # If the query is empty, show a notification
      if (filtered_data$empty) {
        showNotification("No data available to download with current filters.", type = "warning")
        return()
      }

      # Collect the data and write to CSV

      data_to_download <- filtered_data$query |>
        select(-geom) |>
        # These should be the same, but just making extra sure
        filter(
          geoprivacy != "obscured",
          coordinates_obscured == FALSE
        ) |>
        collect()

      write.csv(data_to_download, file, row.names = FALSE)
    }
  )

  # Close the DuckDB connection when the session ends.
  session$onSessionEnded(function() {
    print("Session ended: Closing DuckDB connection.")
    dbDisconnect(con, shutdown = TRUE)
  })
}

# --- Run the Shiny App -------------------------------------------------------
print("Launching Shiny app...")
shinyApp(ui = ui, server = server)
