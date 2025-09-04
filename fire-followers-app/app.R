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
library(shinybusy) # Add shinybusy package for loading spinners
library(plotly) # Add plotly for interactive plots
library(leaflegend) # Add leaflegend package for better legend control
library(memoise)

# Disable S2 geometry if needed
sf_use_s2(FALSE)
CACHE <- "/tmp/Rtmp-urban"

# --- Define the UI -----------------------------------------------------------
ui <- fluidPage(
  useShinyjs(), # initialize shinyjs
  add_busy_spinner(
    spin = "double-bounce",
    color = "#4d7b43",
    margins = c(50, 50),
    position = "top-right"
  ),
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
      /* Improve the appearance of taxa filter dropdown options */
      .selectize-dropdown-content .option {
        padding: 10px 8px !important;
        vertical-align: top !important;
      }
      .selectize-dropdown-content .option * {
        vertical-align: top !important;
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
      /* Reduce space between tabs */
      .tabbable > .nav > li > a {
        padding-top: 5px;
        padding-bottom: 5px;
      }
      /* Reduce space between main tabs and nested tabset panel */
      .mainPanel > .tabbable {
        margin-top: -10px;
      }
      #richness_tabs {
        margin-top: 0;
      }
      /* Further reduce spacing within tab content */
      .tab-content {
        padding-top: 0 !important;
      }
      .tab-pane {
        padding-top: 0;
        margin-top: 0;
      }
      .tabbable .nav-tabs {
        margin-bottom: 5px;
      }
      /* Add styling for download button container */
      .well-panel-download {
        padding: 15px;
        margin-bottom: 0;
      }
      .well-panel-download .btn {
        width: 100%;
        margin-top: 10px;
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
          z-index: 9999;
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
      /* Add styling for the fire selection map container */
      .fire-map-container {
        padding: 15px;
        background: white;
        border: 1px solid #ddd;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        margin-bottom: 20px;
      }
      .fire-map-container h4 {
        margin-bottom: 15px;
      }
      /* App title styling */
      .app-title {
        text-align: center;
        margin-bottom: 20px;
        padding-bottom: 15px;
        border-bottom: 2px solid #4d7b43;
      }
      .app-title h1 {
        color: #345930;
        font-weight: 700;
        margin-bottom: 5px;
      }
      .app-title p {
        color: #6a9a61;
        font-size: 16px;
        max-width: 800px;
        margin: 0 auto;
      }
    "))
  ),

  # Custom title panel with description
  div(
    class = "app-title",
    h1("CNPS Fire Followers Explorer") # ,
    # p("Visualize and analyze plant biodiversity patterns before and after wildfire events in California")
  ),

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
          6,
          offset = 3,
          h5("Please select a fire on the map to begin exploring data", style = "text-align: center; margin-top: 15px; color: #345930; font-weight: 500;"),
          div(
            class = "fire-map-container",
            leafletOutput("fireMap", height = 450)
          )
        )
      ),
      # br(),
      fluidRow(
        column(10,
          offset = 1,
          br(),
          includeHTML("about.html")
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
                    "You can select multiple fires by going back to the 'select fire' map or using the search box here. You can also select 'All Fires'"
                  )
                )
              ),
              choices = NULL, # Initialize with NULL choices
              selected = NULL,
              multiple = TRUE,
              options = list(placeholder = "Loading fires...")
            ),
            # Render time slider dynamically with fire alarm date in the label.
            uiOutput("time_range_ui"),
            style = "background-color: white; border-radius: 6px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);"
          ),

          # Observation-related filters
          wellPanel(
            h4("Observation Filters"),
            # Single selectizeInput for taxonomic filtering across all levels.
            selectizeInput("taxa_filter",
              div(
                "Taxa Filter:",
                tags$span(
                  class = "help-tooltip",
                  "?",
                  tags$span(
                    class = "tooltip-text",
                    "You can search and filter by class, family, genus, or species name and common name"
                  )
                )
              ),
              choices = NULL,
              options = list(
                placeholder = "Search by class, family, genus, species, or common name",
                searchField = "value", # Changed to just search the value field which now contains both scientific and common name
                render = I('{
                   option: function(item, escape) {
                     return "<div style=\'padding: 6px 8px\'>" + item.label + "</div>";
                   },
                   item: function(item, escape) {
                     return "<div style=\'padding: 6px 8px\'>" + item.label + "</div>";
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
            # Create a cleaner solution with direct divs
            tags$div(
              style = "margin-bottom: 15px;",
              tags$div(
                style = "margin-bottom: 5px; font-weight: bold;",
                "Normalize counts:",
                tags$span(
                  class = "help-tooltip",
                  "?",
                  tags$span(
                    class = "tooltip-text",
                    "Shows proportion of selected taxon count to all taxa counts for a given period and fire, useful for mitigating the effects of sampling effort. Raw counts are susceptible to sampling bias from targeted post-fire sampling effort."
                  )
                )
              ),
              tags$div(
                style = "margin-bottom: 10px; font-size: 14px; color: #666; font-style: italic; text-align: center;",
                HTML("
                  <div style='display: inline-block; vertical-align: middle; margin: 0 8px;'>
                    <div style='border-bottom: 1px solid #666; padding-bottom: 2px;'>Selected taxon count<sub>t</sub></div>
                    <div style='padding-top: 2px;'>Total count<sub>t</sub></div>
                  </div>
                  <br>
                  where t = pre-fire or post-fire
                ")
              ),
              tags$div(
                style = "display: flex; justify-content: center;",
                switchInput(
                  inputId = "relative_prop",
                  label = "Normalized",
                  labelWidth = "100px",
                  value = FALSE,
                  size = "small"
                )
              )
            ),
            # Add HTML output to display the denominator values
            conditionalPanel(
              condition = "input.relative_prop == true",
              tags$div(
                style = "margin-top: 8px; padding: 8px; background: #f5f1e8; border-radius: 6px; box-shadow: inset 0 1px 3px rgba(0,0,0,0.05);",
                htmlOutput("proportion_info")
              )
            ),
            style = "background-color: white; border-radius: 6px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);"
          ),

          # Add new wellPanel for data download
          wellPanel(
            class = "well-panel-download",
            h4("Download Data"),
            p("Download the filtered observation data as a CSV file:"),
            downloadButton("downloadData", "Download CSV", class = "btn-primary"),
            style = "background-color: white; border-radius: 6px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);"
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
              "Fire Severity",
              fluidRow(
                column(
                  8,
                  plotlyOutput("richnessPlot")
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
                    ),
                    style = "background-color: white; border-radius: 6px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);"
                  )
                )
              )
            ),
            tabPanel(
              "Most Frequent Species",
              plotlyOutput("top10Plot", height = "600px")
            ),
            tabPanel(
              "Greatest Post-fire Change",
              plotlyOutput("fireInfluencePlot", height = "600px")
            )
          )
        )
      )
    )
  )
)


# --- Define the Server -------------------------------------------------------
server <- function(input, output, session) {
  # --- Setup DuckDB Connection -----------------------------------------------
  print("Establishing DuckDB connection...")
  con <- dbConnect(duckdb::duckdb(), dbdir = "data/shiny.db", read_only = TRUE)
  # con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  # # Then copy necessary tables from the disk database
  # dbExecute(con, "ATTACH 'data/shiny.db' AS source_db (READ_ONLY)")
  # dbExecute(con, "CREATE TABLE gbif_frap AS SELECT * FROM source_db.gbif_frap")
  # dbExecute(con, "CREATE TABLE frap AS SELECT * FROM source_db.frap")
  # dbExecute(con, "DETACH source_db")
  print("DuckDB connection established and spatial extension loaded.")

  # --- Get Fire List Using dbplyr ----------------------------------------------
  print("Retrieving distinct fire names from DuckDB...")
  fire_names <- tbl(con, "gbif_frap") %>%
    distinct(FIRE_NAME) %>%
    arrange(FIRE_NAME) |>
    collect() |>
    pull(1)
  print(paste("Number of fire names retrieved:", length(fire_names)))

  # Update the fire selection dropdown with the retrieved fire_names
  updateSelectizeInput(session, "fire_select",
    choices = c("All Fires" = "all", setNames(fire_names, fire_names)),
    selected = NULL,
    options = list(placeholder = "Search for fires")
  )

  # Add observer to handle "All Fires" selection
  observeEvent(input$fire_select, {
    if ("all" %in% input$fire_select) {
      # If "All Fires" is selected, remove other selections
      updateSelectizeInput(session, "fire_select", selected = "all")
    }
  })

  # Observer to disable/enable the Biodiversity Insights tab based on fire selection.
  observe({
    if (is.null(input$fire_select) || length(input$fire_select) == 0) {
      shinyjs::runjs("$('#tabs li a:contains(\"Biodiversity Insights\")').addClass('disabled').css({'pointer-events': 'none', 'opacity': '0.5'});")
    } else {
      shinyjs::runjs("$('#tabs li a:contains(\"Biodiversity Insights\")').removeClass('disabled').css({'pointer-events': 'auto', 'opacity': '1'});")
      # Don't show spinner here - let individual reactive elements handle their own loading states
    }
  })

  # Create a reactive time range slider that updates with fire selection
  output$time_range_ui <- renderUI({
    req(input$fire_select)

    # Get fire info to calculate dates
    selected_fire_names <- input$fire_select
    fire_info <- con %>%
      tbl("gbif_frap") %>%
      filter(FIRE_NAME %in% selected_fire_names) %>%
      group_by(FIRE_NAME) %>%
      slice_max(GIS_ACRES, with_ties = FALSE) %>%
      collect()

    # Calculate time range based on fire dates
    if (nrow(fire_info) > 0) {
      fire_dates <- fire_info %>%
        select(FIRE_NAME, ALARM_DATE, GIS_ACRES) %>%
        distinct() %>%
        mutate(ALARM_DATE = as.Date(ALARM_DATE)) %>%
        arrange(ALARM_DATE)

      alarm_date <- fire_dates$ALARM_DATE[1]
      new_start <- alarm_date - (365 * 4)
      new_end <- alarm_date + (365 * 4)

      # Format fire dates for display
      date_strings <- paste0("<div style='margin-top: 3px;'><i>", fire_dates$FIRE_NAME, "</i>: ", fire_dates$ALARM_DATE, "</div>")
      dates_html <- paste(date_strings, collapse = "")

      fire_dates_display <- paste0(
        "<div style='font-weight: 600; margin-bottom: 5px; color: #345930;'>Fire alarm dates:</div>",
        dates_html
      )
    } else {
      new_start <- as.Date("2015-01-01")
      new_end <- Sys.Date()
      fire_dates_display <- "No fire information available"
    }

    tagList(
      sliderInput("time_range",
        label = "Select date range for observations",
        min = as.Date("2015-01-01"),
        max = Sys.Date(),
        value = c(new_start, new_end),
        step = 30,
        timeFormat = "%Y-%m"
      ),
      tags$div(
        id = "fire_dates_info",
        style = "margin-top: 8px; padding: 8px; background: #f5f1e8; border-radius: 6px; box-shadow: inset 0 1px 3px rgba(0,0,0,0.05);",
        HTML(fire_dates_display)
      )
    )
  })

  # No need for separate observer - time_range_ui output handles everything reactively

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
      # For species, get both the species name and common name
      species_df <- tbl(con, "gbif_frap") %>%
        # Standardize common_name capitalization to prevent duplicates
        mutate(common_name = str_to_lower(common_name)) %>%
        distinct(taxon_species_name, common_name) %>%
        collect() |>
        # The str_to_sentence doesn't work in dbplyr, so we do it here
        mutate(common_name = str_to_sentence(common_name))
    } else {
      # For specific fires, get taxon names from selected fires
      classes <- tbl(con, "gbif_frap") %>%
        filter(FIRE_NAME %in% input$fire_select) %>%
        distinct(taxon_class_name) %>%
        pull(taxon_class_name)
      families <- tbl(con, "gbif_frap") %>%
        filter(FIRE_NAME %in% input$fire_select) %>%
        distinct(taxon_family_name) %>%
        pull(taxon_family_name)
      genera <- tbl(con, "gbif_frap") %>%
        filter(FIRE_NAME %in% input$fire_select) %>%
        distinct(taxon_genus_name) %>%
        pull(taxon_genus_name)
      # For species, get both the species name and common name
      species_df <- tbl(con, "gbif_frap") %>%
        filter(FIRE_NAME %in% input$fire_select) %>%
        # Standardize common_name capitalization to prevent duplicates
        mutate(common_name = str_to_lower(common_name)) %>%
        distinct(taxon_species_name, common_name) %>%
        collect() |>
        # The str_to_sentence doesn't work in dbplyr, so we do it here
        mutate(common_name = str_to_sentence(common_name))
    }

    # Remove NA values
    classes <- classes[!is.na(classes)]
    families <- families[!is.na(families)]
    genera <- genera[!is.na(genera)]

    # Process species data - remove rows with NA species names
    species_df <- species_df[!is.na(species_df$taxon_species_name), ]

    # Create species choices
    species_choices <- lapply(1:nrow(species_df), function(i) {
      sci_name <- species_df$taxon_species_name[i]
      common <- species_df$common_name[i]

      # Create the display text - scientific name plus common name if available
      display_text <- if (!is.na(common) && common != "") {
        paste0(
          "<i>", sci_name, "</i> <span style='color:#6a9a61; font-style: normal;'>Species</span>",
          "<br><span style='font-size:12px; color:#888;'>", common, "</span>"
        )
      } else {
        paste0("<i>", sci_name, "</i> <span style='color:#6a9a61; font-style: normal;'>Species</span>")
      }

      # For value, add common name to make it searchable but keep the database value intact
      search_value <- if (!is.na(common) && common != "") {
        # Create an internal data structure that stores both scientific and common name
        # Format: species|scientific_name:::common_name
        # The ::: separator won't interfere with the | we use to split it for filtering
        paste0("species|", sci_name, ":::", common)
      } else {
        paste0("species|", sci_name)
      }

      setNames(search_value, display_text)
    })

    # Combine into a single vector
    species_choices <- unlist(species_choices)

    # Create the main choices list
    choices <- c(
      setNames("all", "All taxa"),
      setNames(paste0("class|", classes), paste0(classes, " <span style='color:#6a9a61; font-style: italic;'>Class</span>")),
      setNames(paste0("family|", families), paste0(families, " <span style='color:#6a9a61; font-style: italic;'>Family</span>")),
      setNames(paste0("genus|", genera), paste0(genera, " <span style='color:#6a9a61; font-style: italic;'>Genus</span>")),
      species_choices
    )

    # Get current selection to preserve it if possible
    current_selection <- isolate(input$taxa_filter)

    # Only update if the choices have actually changed
    # This prevents unnecessary updates that could trigger the relative_prop observer
    if (is.null(current_selection) || current_selection == "") {
      # First time loading, set default
      selected_value <- "class|Liliopsida"
    } else {
      # Try to preserve current selection if it's still valid
      selected_value <- if (current_selection %in% choices) current_selection else "class|Liliopsida"
    }

    # Update the dropdown with new options to enable searching by common name
    updateSelectizeInput(session, "taxa_filter",
      choices = choices,
      selected = selected_value,
      server = TRUE,
      options = list(
        placeholder = "Search by class, family, genus, species, or common name",
        searchField = "value", # Changed to just search the value field which now contains both scientific and common name
        render = I('{
          option: function(item, escape) {
            return "<div>" + item.label + "</div>";
          },
          item: function(item, escape) {
            return "<div>" + item.label + "</div>";
          }
        }')
      )
    )
  })

  # Observer to automatically switch relative_prop to TRUE when a specific taxon is selected
  # Only trigger when the user actually changes the taxa filter, not when it's programmatically updated
  observeEvent(input$taxa_filter, {
    # If user selects anything other than "All taxa", switch relative_prop to TRUE
    if (input$taxa_filter != "all") {
      updateSwitchInput(session, "relative_prop", value = TRUE)
    }
  })

  # --- Reactive: Get all fires (one record per fire, using the largest fire by GIS_ACRES).
  # Create a memoised version of the all_fires function
  get_all_fires_memoised <- memoise(function(fire_names, con) {
    print("get_all_fires_memoised: Retrieving all fire data from cache or database.")
    tryCatch(
      {
        fires_info <- tbl(con, "frap") %>%
          # Do this because at the top we're only pulling fire_names that have gbif records associated with them
          filter(FIRE_NAME %in% fire_names) |>
          group_by(FIRE_NAME) %>%
          slice_max(GIS_ACRES, with_ties = FALSE) %>%
          collect()

        if (nrow(fires_info) == 0) {
          # Handle case where no fires are found
          print("get_all_fires_memoised: No fires found matching the criteria")
          return(NULL)
        }

        fires_sf <- fires_info %>%
          mutate(geometry = st_as_sfc(geom_wkt, crs = 4326) %>% st_zm(drop = TRUE)) %>%
          st_as_sf() %>%
          st_make_valid()

        # Ensure we have a valid sf object
        if (!is(fires_sf, "sf") || nrow(fires_sf) == 0) {
          print("get_all_fires_memoised: Failed to create a valid sf object")
          return(NULL)
        }

        fires_sf
      },
      error = function(e) {
        # Log the error
        print(paste("Error in get_all_fires_memoised:", e$message))
        NULL
      }
    )
  }, cache = cache_filesystem(CACHE))

  all_fires <- reactive({
    print("all_fires: Calling memoised function.")
    get_all_fires_memoised(fire_names, con)
  }) %>% bindCache(fire_names) # Cache based on fire_names since they don't change in a session

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

    # Don't show spinner here - let individual outputs handle their own loading states

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

      # Extract just the scientific name if there's a common name appended with :::
      if (grepl(":::", value)) {
        value <- strsplit(value, ":::")[[1]][1]
      }

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

    # Don't need to hide spinner since we're not showing it here

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

  # Separate reactive for fire centroids that properly depends on all_fires and fire_select
  fire_centroids_filtered <- reactive({
    print("fire_centroids_filtered: Processing fire centroids for map display")

    # Get all fires first
    all_fire_data <- all_fires()
    if (is.null(all_fire_data)) {
      print("fire_centroids_filtered: No fire data available")
      return(NULL)
    }

    # Create centroids for all fires
    all_centroids <- st_centroid(all_fire_data)

    # Return a list with selected and unselected centroids
    if (is.null(input$fire_select) || length(input$fire_select) == 0) {
      # If no selection, all fires are unselected
      list(
        selected = NULL,
        unselected = all_centroids,
        all = all_centroids
      )
    } else if ("all" %in% input$fire_select) {
      # If "all" is selected, all fires are selected
      list(
        selected = all_centroids,
        unselected = NULL,
        all = all_centroids
      )
    } else {
      # Filter centroids based on selection
      selected <- all_centroids %>% filter(FIRE_NAME %in% input$fire_select)
      unselected <- all_centroids %>% filter(!FIRE_NAME %in% input$fire_select)

      list(
        selected = if (nrow(selected) > 0) selected else NULL,
        unselected = if (nrow(unselected) > 0) unselected else NULL,
        all = all_centroids
      )
    }
  }) %>% bindCache(input$fire_select, fire_names) # Cache on both fire_names and fire_select

  # --- First Tab: Fire Centroids Map ---------------------------------------
  output$fireMap <- renderLeaflet({
    print("output$fireMap: Initial render - creating base map only")

    # Create a basic leaflet map (will be updated by observer)
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -119.5, lat = 37.5, zoom = 5) %>% # More zoomed out view of California
      addControl(
        html = "<div style='padding: 12px; background: white; border-radius: 6px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);'>Loading fire data...</div>",
        position = "topright"
      )
  })

  # Use an observer to update the map when fire selection changes
  observe({
    print("fireMap observer triggered - updating markers")

    # Get all fires first
    all_fire_data <- all_fires()
    if (is.null(all_fire_data)) {
      print("No fire data available")
      return()
    }

    # Create centroids for all fires
    centroids <- st_centroid(all_fire_data)
    print(paste("Number of fires for map:", nrow(centroids)))

    # Create icons for selected and unselected fires
    fireIcons <- awesomeIcons(
      icon = "fire-flame-curved",
      iconColor = "#FFFFFF",
      markerColor = "orange", # Using predefined orange color for fire theme
      library = "fa"
    )

    selectedFireIcons <- awesomeIcons(
      icon = "fire-flame-curved",
      iconColor = "#FFFFFF",
      markerColor = "cadetblue", # Using predefined lightblue color
      library = "fa"
    )

    # Get the leaflet proxy to update existing map
    proxy <- leafletProxy("fireMap")

    # Clear existing markers and controls
    proxy %>%
      clearMarkers() %>%
      clearControls()

    # Check if "all" is selected
    if (!is.null(input$fire_select) && "all" %in% input$fire_select) {
      # If "all" is selected, show all markers as selected
      print("'All Fires' selected, showing all fires as selected")
      proxy %>%
        addAwesomeMarkers(
          data = centroids,
          label = ~FIRE_NAME,
          layerId = ~FIRE_NAME, # Keep original FIRE_NAME as layer ID
          icon = selectedFireIcons
        )
    } else if (is.null(input$fire_select) || length(input$fire_select) == 0) {
      # If no fires selected, show all as unselected
      print("No fires selected, showing all fires as unselected")
      proxy %>%
        addAwesomeMarkers(
          data = centroids,
          label = ~FIRE_NAME,
          layerId = ~FIRE_NAME, # Keep original FIRE_NAME as layer ID
          icon = fireIcons
        )
    } else {
      # Specific fires selected
      print(paste("Selected fires:", paste(input$fire_select, collapse = ", ")))

      # Use direct subsetting with [ ] instead of filter()
      unselected <- centroids[!centroids$FIRE_NAME %in% input$fire_select, ]
      selected <- centroids[centroids$FIRE_NAME %in% input$fire_select, ]

      print(paste("Selected fires count:", nrow(selected)))
      print(paste("Unselected fires count:", nrow(unselected)))

      # Add markers for unselected fires if there are any
      if (nrow(unselected) > 0) {
        proxy %>%
          addAwesomeMarkers(
            data = unselected,
            label = ~FIRE_NAME,
            layerId = ~FIRE_NAME, # Keep original FIRE_NAME as layer ID
            icon = fireIcons
          )
      }

      # Add markers for selected fires if there are any
      if (nrow(selected) > 0) {
        proxy %>%
          addAwesomeMarkers(
            data = selected,
            label = ~FIRE_NAME,
            layerId = ~FIRE_NAME, # Keep original FIRE_NAME as layer ID
            icon = selectedFireIcons
          )
      }
    }
  })

  # When a fire is clicked on the map, update the species richness fire select input.
  observeEvent(input$fireMap_marker_click, {
    click <- input$fireMap_marker_click
    selected_fire_name <- click$id
    print(paste("Fire selected from map:", selected_fire_name))

    # Don't show spinner here - let individual outputs handle their own loading states

    # Extract the actual fire name if it contains "unselected_" or "selected_" prefix
    if (grepl("^(un)?selected_", selected_fire_name)) {
      selected_fire_name <- sub("^(un)?selected_", "", selected_fire_name)
    }

    print(paste("Processed fire name for selection:", selected_fire_name))

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

    # Don't show spinner here - let individual outputs handle their own loading states

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

    # Don't need to hide spinner since we're not showing it here

    fire_sf
  }) %>% bindCache(input$fire_select)

  # Cache the species points data (filtered by taxa)
  species_points <- reactive({
    req(filtered_gbif_data())
    print("species_points: Using filtered_gbif_data to get species points")

    # Don't show spinner here - let individual outputs handle their own loading states

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

    # Don't need to hide spinner since we're not showing it here

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

    # Don't show spinner here - let individual outputs handle their own loading states

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

    # If relative proportion is requested, we need to calculate cell-specific totals
    if (input$relative_prop) {
      # Get all species points (without taxon filtering)
      req(all_species_points())
      all_spp_sf <- all_species_points()

      # Split all species points into pre and post fire periods
      all_spp_pre <- all_spp_sf %>% filter(as.Date(observed_on) < f_date)
      all_spp_post <- all_spp_sf %>% filter(as.Date(observed_on) >= f_date)

      # Calculate all-taxa totals for each grid cell
      grid_pre$total <- count_points(grid_pre, all_spp_pre)
      grid_post$total <- count_points(grid_post, all_spp_post)

      # Normalize each cell individually by dividing by its all-taxa total
      grid_pre$count <- ifelse(grid_pre$total > 0, grid_pre$count / grid_pre$total, 0)
      grid_post$count <- ifelse(grid_post$total > 0, grid_post$count / grid_post$total, 0)
    }

    # Don't need to hide spinner since we're not showing it here

    list(pre = grid_pre, post = grid_post, fire = fire_sf)
  }) %>% bindCache(input$fire_select, input$taxa_filter, input$count_type, input$time_range, input$relative_prop)

  # Render pre-fire and post-fire maps with synced view.
  output$sync_maps_ui <- renderUI({
    # Make sure we have the necessary inputs
    req(input$fire_select, input$taxa_filter, input$count_type, input$time_range)

    # Show spinner for map creation and synchronization
    show_spinner()

    print("Creating and syncing maps")

    # Get the grid data directly, which will be cached
    data <- species_grid_by_period()
    req(data)

    # Create shared color palette
    all_counts <- c(data$pre$count, data$post$count)
    pal <- if (input$relative_prop) {
      colorNumeric(viridis::viridis(256), domain = all_counts)
    } else {
      colorNumeric(viridis::viridis(256), domain = log1p(all_counts))
    }

    # Pre-fire map
    pre_label <- if (input$count_type == "Total Observations") {
      if (input$relative_prop) {
        ~ paste("Pre-fire Relative Frequency:", count)
      } else {
        ~ paste("Pre-fire Obs. Count:", count)
      }
    } else {
      if (input$relative_prop) {
        ~ paste("Pre-fire Relative Frequency:", count)
      } else {
        ~ paste("Pre-fire Species Count:", count)
      }
    }
    pre_map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      # Add taxa filter banner in top left
      addControl(
        html = {
          # Extract the display name for the selected taxon
          taxon_display <- if (input$taxa_filter == "all") {
            "All taxa"
          } else if (grepl("^class\\|", input$taxa_filter)) {
            gsub("^class\\|", "", input$taxa_filter)
          } else if (grepl("^family\\|", input$taxa_filter)) {
            gsub("^family\\|", "", input$taxa_filter)
          } else if (grepl("^genus\\|", input$taxa_filter)) {
            gsub("^genus\\|", "", input$taxa_filter)
          } else if (grepl("^species\\|", input$taxa_filter)) {
            # Extract just the scientific name if there's a common name
            species_part <- gsub("^species\\|", "", input$taxa_filter)
            if (grepl(":::", species_part)) {
              gsub(":::.+$", "", species_part)
            } else {
              species_part
            }
          } else {
            input$taxa_filter
          }
          paste0("<h4 style='text-align:center; margin:0; background-color:#6a9a61; color:white; padding:8px; border-radius:6px; box-shadow:0 0 5px rgba(0,0,0,0.3); font-size:14px;'>", taxon_display, "</h4>")
        },
        position = "topleft"
      ) %>%
      addControl(
        html = "<h4 style='text-align:center; margin:0; background-color:white; padding:8px; border-radius:6px; box-shadow:0 0 5px rgba(0,0,0,0.2); color: #345930;'>Pre-Fire</h4>",
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
        ~ paste("Post-fire Relative Frequency:", count)
      } else {
        ~ paste("Post-fire Obs. Count:", count)
      }
    } else {
      if (input$relative_prop) {
        ~ paste("Post-fire Relative Frequency:", count)
      } else {
        ~ paste("Post-fire Species Count:", count)
      }
    }
    post_map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addControl(
        html = "<h4 style='text-align:center; margin:0; background-color:white; padding:8px; border-radius:6px; box-shadow:0 0 5px rgba(0,0,0,0.2); color: #345930;'>Post-Fire</h4>",
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
      addLegendNumeric(
        position = "bottomright",
        pal = pal,
        values = if (input$relative_prop) all_counts else log1p(all_counts),
        title = if (input$relative_prop) {
          "Relative Frequency"
        } else if (input$count_type == "Total Observations") {
          "Observation Count"
        } else {
          "Species Count"
        },
        orientation = "horizontal",
        shape = "rect",
        width = 100,
        height = 20,
        bins = 7,
        numberFormat = if (input$relative_prop) {
          function(x) prettyNum(x, format = "f", big.mark = ",", digits = 3, scientific = FALSE)
        } else {
          function(x) prettyNum(round(expm1(x), 0), format = "f", big.mark = ",", scientific = FALSE)
        }
      )

    # Synchronized maps
    sync_maps <- leafsync::sync(list(pre_map, post_map))

    # Hide spinner when map creation is complete
    hide_spinner()

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
  output$richnessPlot <- renderPlotly({
    req(richness_data())
    print("output$richnessPlot: Rendering species richness plot with severity breakdown.")

    # Show spinner during plot generation
    show_spinner()

    # 5 and 6 are just mask values I think. remake them into 'No Data'

    rd <- richness_data() |>
      mutate(Severity = as.character(Severity)) |>
      mutate(Severity = ifelse(Severity %in% c("5", "6", "7", "8"), "No Data", Severity)) |>
      mutate(Severity = factor(Severity, levels = c("All", "1", "2", "3", "4", "No Data")))

    # Define colors for pre-fire and post-fire periods (same as original)
    period_colors <- c("Pre-fire" = "#68C6C0", "Post-fire" = "#dd5858")

    # Create the plot with severity on x-axis and period as fill color
    p <- ggplot(rd, aes(
      x = Severity, y = Count, fill = Period,
      text = paste(
        "Count:", round(Count, 4),
        "<br>Period:", Period,
        "<br>Severity:", Severity
      )
    )) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      scale_fill_manual(values = period_colors) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold", color = "#345930"),
        plot.title = element_text(size = 16, face = "bold", color = "#345930"),
        panel.grid.major = element_line(color = "#f0f0f0"),
        panel.grid.minor = element_line(color = "#f9f9f9"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = NA),
        legend.title = element_blank(),
        legend.position = "top",
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        plot.margin = unit(c(1, 1, 1, 1), "cm")
      ) +
      labs(
        title = ifelse(input$count_type == "Total Species",
          "Species Richness by Fire Severity",
          "Observations by Fire Severity"
        ),
        subtitle = "Distribution across MTBS fire severity classes",
        y = ifelse(input$relative_prop,
          "Relative Frequency",
          ifelse(input$count_type == "Total Species",
            "Number of Unique Species",
            "Number of Observations"
          )
        ),
        x = "Fire Severity"
      )

    # Hide spinner when plot is complete
    hide_spinner()

    # Store the title and subtitle
    plot_title <- ifelse(input$count_type == "Total Species",
      "Species Richness by Fire Severity",
      "Observations by Fire Severity"
    )
    plot_subtitle <- "Distribution across MTBS fire severity classes"

    # Convert to plotly with tooltips and add subtitle
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(
        title = list(
          text = paste0(plot_title, "<br><span style='font-size: 14px; color: #6a9a61;'>", plot_subtitle, "</span>"),
          font = list(family = "Arial", size = 16)
        ),
        margin = list(t = 80) # Add more top margin for the subtitle
      )
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
        group_by(scientific_name, common_name) %>% # Include common_name
        summarize(
          count = if (input$count_type == "Total Species") n_distinct(scientific_name) else n(),
          .groups = "drop"
        ) %>%
        collect()
    } else {
      data.frame(scientific_name = character(0), common_name = character(0), count = numeric(0))
    }
    pre_fire$period <- "Pre-fire"

    # Get post-fire data
    post_fire <- if (end_date >= fire_date) {
      filtered_data$query %>%
        filter(observed_on >= fire_date) %>%
        group_by(scientific_name, common_name) %>% # Include common_name
        summarize(
          count = if (input$count_type == "Total Species") n_distinct(scientific_name) else n(),
          .groups = "drop"
        ) %>%
        collect()
    } else {
      data.frame(scientific_name = character(0), common_name = character(0), count = numeric(0))
    }
    post_fire$period <- "Post-fire"

    # Combine data
    combined <- bind_rows(pre_fire, post_fire)

    # Check if we have any data
    if (nrow(combined) == 0) {
      return(data.frame(
        scientific_name = character(0),
        common_name = character(0),
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
      group_by(scientific_name, common_name) %>% # Include common_name in grouping
      summarize(total_count = sum(count), .groups = "drop") %>%
      arrange(desc(total_count)) %>%
      slice_head(n = 10)

    top_scientific_names <- top_species %>% pull(scientific_name)

    # Filter for only top 10 species and ensure both periods are represented
    result <- combined %>%
      filter(scientific_name %in% top_scientific_names) %>%
      complete(scientific_name = top_scientific_names, period = c("Pre-fire", "Post-fire"), fill = list(count = 0))

    # Ensure we have common names for all entries (handling NAs from complete())
    result <- result %>%
      group_by(scientific_name) %>%
      mutate(common_name = ifelse(is.na(common_name),
        first(common_name[!is.na(common_name)]),
        common_name
      )) %>%
      ungroup()

    # Reorder factors for plotting
    result %>%
      mutate(
        scientific_name = factor(scientific_name, levels = top_scientific_names),
        period = factor(period, levels = c("Pre-fire", "Post-fire"))
      )
  })

  # Render the Top 10 Plot
  output$top10Plot <- renderPlotly({
    req(input$count_type)

    # Show spinner during plot generation
    show_spinner()

    if (input$count_type == "Total Species") {
      # Create a blank plot with just text
      p <- ggplot() +
        annotate("text",
          x = 0.5, y = 0.5,
          label = "Please select 'Observations' from the \n sidebar to view the top 10 species.",
          size = 6
        ) +
        theme_void() +
        xlim(0, 1) +
        ylim(0, 1)

      # Hide spinner for this simple plot
      hide_spinner()

      return(ggplotly(p) %>% config(displayModeBar = FALSE))
    } else {
      req(top10_data())
      print("output$top10Plot: Rendering top 10 species plot.")

      period_colors <- c("Pre-fire" = "#68C6C0", "Post-fire" = "#dd5858")

      # Prepare the data with common names
      plot_data <- top10_data()

      # Create custom axis labels
      x_labels <- setNames(
        paste0("<i>", plot_data$scientific_name, "</i><br><span style='font-size:10px;color:#888888'>", plot_data$common_name, "</span>"),
        plot_data$scientific_name
      )

      p <- ggplot(plot_data, aes(
        x = scientific_name, y = count, fill = period,
        text = paste(
          "Count:", round(count, 4),
          "<br>Period:", period,
          "<br>Species:", scientific_name,
          "<br>Common Name:", common_name
        )
      )) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = period_colors) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 14, face = "bold", color = "#345930"),
          plot.title = element_text(size = 16, face = "bold", color = "#345930"),
          plot.subtitle = element_text(size = 13, color = "#6a9a61"),
          panel.grid.major = element_line(color = "#f0f0f0"),
          panel.grid.minor = element_line(color = "#f9f9f9"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white", color = NA),
          legend.title = element_blank(),
          legend.position = "top",
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "white", color = NA),
          plot.margin = unit(c(1, 1, 1, 1), "cm")
        ) +
        labs(
          title = "Top 10 Observed Species",
          subtitle = "Most frequently observed species before + after fire",
          x = "",
          y = if (input$relative_prop) "Relative Frequency" else "Count",
          fill = "Period"
        )

      # If the names are too long, try to wrap them
      if (any(nchar(levels(plot_data$scientific_name)) > 30)) {
        p <- p + scale_x_discrete(labels = function(x) str_wrap(x, width = 25))
      }

      # Hide spinner when plot is complete
      hide_spinner()

      # Store the subtitle
      plot_title <- "Top 10 Observed Species"
      plot_subtitle <- "Most frequently observed species before + after fire"

      # Convert to plotly with tooltips and add subtitle
      p_plotly <- ggplotly(p, tooltip = "text") %>%
        config(displayModeBar = FALSE) %>%
        layout(
          title = list(
            text = paste0(plot_title, "<br><span style='font-size: 14px; color: #6a9a61;'>", plot_subtitle, "</span>"),
            font = list(family = "Arial", size = 16)
          ),
          margin = list(t = 80) # Add more top margin for the subtitle
        )

      # Update x-axis labels with italicized scientific names and smaller common names
      p_plotly %>%
        layout(
          xaxis = list(
            tickmode = "array",
            tickvals = seq_along(levels(plot_data$scientific_name)),
            ticktext = unique(x_labels[levels(plot_data$scientific_name)])
          )
        )
    }
  })

  # Update the proportion_info to use the reactive values
  output$proportion_info <- renderUI({
    req(total_pre_all_taxa(), total_post_all_taxa())

    # Get the pre and post totals from the reactive values
    pre_total <- total_pre_all_taxa()
    post_total <- total_post_all_taxa()

    # Determine what type of counts we're showing
    count_type_text <- if (input$count_type == "Total Species") "species" else "observations"

    # Create HTML output with the total values
    HTML(paste0(
      "<div style='font-weight: 600; margin-bottom: 8px; color: #345930;'>Denominator totals:</div>",
      "<div style='display: grid; gap: 5px;'>",
      "<div>Total count<sub>t=pre-fire</sub>: <span style='font-weight: 500; color: #68C6C0;'>", pre_total, " ", count_type_text, "</span></div>",
      "<div>Total count<sub>t=post-fire</sub>: <span style='font-weight: 500; color: #dd5858;'>", post_total, " ", count_type_text, "</span></div>",
      "</div>"
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
      group_by(scientific_name, common_name) %>% # Include common_name
      summarize(
        pre_count = if (input$count_type == "Total Species") n_distinct(scientific_name) else n(),
        .groups = "drop"
      ) %>%
      collect()

    post_fire <- filtered_data$query %>%
      filter(observed_on >= fire_date) %>%
      group_by(scientific_name, common_name) %>% # Include common_name
      summarize(
        post_count = if (input$count_type == "Total Species") n_distinct(scientific_name) else n(),
        .groups = "drop"
      ) %>%
      collect()

    # Combine and calculate ratios
    combined <- full_join(pre_fire, post_fire, by = c("scientific_name", "common_name")) %>%
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
        common_name = character(0),
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
        # Create label combining name and ratio (but we'll replace this in the plot)
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
  output$fireInfluencePlot <- renderPlotly({
    req(input$count_type)

    # Show spinner during plot generation
    show_spinner()

    if (input$count_type == "Total Species") {
      # Create a blank plot with just text
      p <- ggplot() +
        annotate("text",
          x = 0.5, y = 0.5,
          label = "Please select 'Observations' from the \n sidebar to view the species most influenced by fire.",
          size = 6
        ) +
        theme_void() +
        xlim(0, 1) +
        ylim(0, 1)

      # Hide spinner for this simple plot
      hide_spinner()

      return(ggplotly(p) %>% config(displayModeBar = FALSE))
    } else {
      req(fire_influence_data())
      print("output$fireInfluencePlot: Rendering fire influence plot.")

      period_colors <- c("Pre-fire" = "#68C6C0", "Post-fire" = "#dd5858")

      # Prepare the data
      plot_data <- fire_influence_data()

      # Calculate change frequencies from labels and sort species by change_freq in descending order
      species_totals <- plot_data %>%
        group_by(scientific_name) %>%
        summarize(
          total_count = sum(count),
          change_freq = first(gsub(".*\\((.*)\\).*", "\\1", label))
        ) %>%
        # Sort by change_freq in descending order (removing the + sign for numeric comparison)
        mutate(change_numeric = as.numeric(gsub("\\+", "", change_freq))) %>%
        arrange(desc(change_numeric))

      # Reorder scientific names by change frequency in descending order
      plot_data$scientific_name <- factor(
        plot_data$scientific_name,
        levels = species_totals$scientific_name
      )

      # Create custom axis labels with scientific names in italics and common names smaller and gray
      label_map <- plot_data %>%
        select(scientific_name, common_name) %>%
        distinct()

      # Extract change frequency values from the original labels
      change_freq <- plot_data %>%
        select(scientific_name, label) %>%
        distinct() %>%
        mutate(change_freq = gsub(".*\\((.*)\\).*", "\\1", label))

      # Merge change frequency back with label_map
      label_map <- left_join(label_map,
        change_freq %>% select(scientific_name, change_freq),
        by = "scientific_name"
      )

      # Create the mapping for x-axis labels that includes both common name and change frequency
      x_labels <- setNames(
        paste0(
          "<i>", label_map$scientific_name, "</i> ", label_map$change_freq,
          "<br><span style='font-size:10px;color:#888888'>",
          label_map$common_name, "</span>"
        ),
        label_map$scientific_name
      )

      p <- ggplot(
        plot_data,
        aes(
          x = scientific_name,
          y = count,
          fill = factor(period, levels = c("Pre-fire", "Post-fire")),
          text = paste(
            "Count:", round(count, 4),
            "<br>Period:", period,
            "<br>Species:", scientific_name,
            "<br>Common Name:", common_name
          )
        )
      ) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = period_colors) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 14, face = "bold", color = "#345930"),
          plot.title = element_text(size = 16, face = "bold", color = "#345930"),
          plot.subtitle = element_text(size = 13, color = "#6a9a61"),
          panel.grid.major = element_line(color = "#f0f0f0"),
          panel.grid.minor = element_line(color = "#f9f9f9"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white", color = NA),
          legend.title = element_blank(),
          legend.position = "top",
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "white", color = NA),
          plot.margin = unit(c(1, 1, 1, 1), "cm")
        ) +
        labs(
          title = "Species with Greatest Change in Observation Frequency Post-Fire",
          subtitle = "Top 10 species with greatest pre-fire to post-fire change",
          x = "Species (with post- and pre- fire difference)",
          y = if (input$relative_prop) "Relative Frequency" else "Count",
          fill = "Period"
        )

      # If the names are too long, try to wrap them
      if (any(nchar(levels(plot_data$scientific_name)) > 30)) {
        p <- p + scale_x_discrete(labels = function(x) str_wrap(x, width = 25))
      }

      # Hide spinner when plot is complete
      hide_spinner()

      # Store the title and subtitle
      plot_title <- "Species with Greatest Change in Observation Frequency Post-Fire"
      plot_subtitle <- "Top 10 species with greatest pre-fire to post-fire change"

      # Convert to plotly with tooltips and add subtitle
      p_plotly <- ggplotly(p, tooltip = "text") %>%
        config(displayModeBar = FALSE) %>%
        layout(
          title = list(
            text = paste0(plot_title, "<br><span style='font-size: 14px; color: #6a9a61;'>", plot_subtitle, "</span>"),
            font = list(family = "Arial", size = 16)
          ),
          margin = list(t = 80) # Add more top margin for the subtitle
        )

      # Update x-axis labels with italicized scientific names and common names
      p_plotly %>%
        layout(
          xaxis = list(
            tickmode = "array",
            tickvals = seq_along(levels(plot_data$scientific_name)),
            ticktext = x_labels[levels(plot_data$scientific_name)]
          )
        )
    }
  })

  # Add new download handler for filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("fire_followers_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      req(filtered_gbif_data())

      # Show spinner during data preparation for download
      show_spinner()

      # Get the filtered data
      filtered_data <- filtered_gbif_data()

      # If the query is empty, show a notification
      if (filtered_data$empty) {
        showNotification("No data available to download with current filters.", type = "warning")
        hide_spinner()
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

      # Hide spinner once download preparation is complete
      hide_spinner()
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
