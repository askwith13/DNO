library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)
library(httr)
library(jsonlite)
library(readr)
library(tidyr)
library(viridis)
library(lpSolve)
library(DT)
library(shinycssloaders)
library(osrm)

ui <- dashboardPage(
  dashboardHeader(title = "Diagnostic Network Optimization"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "data_input", icon = icon("upload")),
      menuItem("Optimization Settings", tabName = "settings", icon = icon("cogs")),
      menuItem("Optimization Results", tabName = "results", icon = icon("chart-bar")),
      menuItem("Interactive Map", tabName = "map", icon = icon("map")),
      menuItem("Isochrone Analysis", tabName = "isochrone", icon = icon("clock")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .progress-text {
          font-family: monospace;
          font-size: 12px;
          background-color: #f8f9fa;
          padding: 10px;
          border: 1px solid #dee2e6;
          border-radius: 4px;
          white-space: pre-wrap;
          max-height: 200px;
          overflow-y: auto;
        }
        .progress {
          background-color: #e9ecef;
          border-radius: 4px;
        }
        .progress-bar {
          background-color: #007bff;
          transition: width 0.3s ease;
        }
      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('updateProgressBar', function(data) {
          var progressBar = document.getElementById('progress_bar');
          var progressText = document.getElementById('progress_text');
          if (progressBar && progressText) {
            progressBar.style.width = data.percent + '%';
            progressText.textContent = data.message + ' (' + data.percent + '%)';
          }
        });
      "))
    ),
    
    tabItems(
      tabItem(tabName = "data_input",
              h2("Upload Your Network Data"),
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Data Templates and Upload",
                    p("Step 1: Download the template files and fill in your data"),
                    div(style = "margin-bottom: 15px;",
                        downloadButton("download_district_template", "Download District Lab Template", 
                                       class = "btn-info", icon = icon("download")),
                        downloadButton("download_cdst_template", "Download CDST Lab Template", 
                                       class = "btn-info", icon = icon("download"))
                    ),
                    hr(),
                    p("Step 2: Upload your completed data files"),
                    fileInput("district_file", "Upload District Lab Data (CSV)", 
                              accept = c(".csv"), width = "100%"),
                    fileInput("cdst_file", "Upload CDST Lab Data (CSV)", 
                              accept = c(".csv"), width = "100%"),
                    fileInput("shapefile", "Upload State Map (GeoJSON/Shapefile)",
                              accept = c(".geojson", ".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"),
                              multiple = TRUE, width = "100%"),
                    hr(),
                    h4("Data Validation Status:"),
                    verbatimTextOutput("data_validation_status")
                )
              )
      ),
      
      tabItem(tabName = "settings",
              h2("Optimization Settings"),
              fluidRow(
                box(width = 6, title = "Distance Calculation Method", status = "warning", solidHeader = TRUE,
                    radioButtons("distance_method", "Choose distance calculation method:",
                                 choices = list(
                                   "OpenRouteService API (Most accurate, real driving times)" = "ors_api",
                                   "OSRM Package (Good balance of accuracy and speed)" = "osrm",
                                   "Euclidean Distance (Fastest, straight-line approximation)" = "euclidean"
                                 ),
                                 selected = "osrm"),
                    conditionalPanel(
                      condition = "input.distance_method == 'ors_api'",
                      textInput("ors_api_key", "OpenRouteService API Key:",
                                value = "5b3ce3597851110001cf62481a51a0c1d253455eb67aa25df367bd8d",
                                placeholder = "Enter your ORS API key"),
                      numericInput("api_delay", "Delay between API calls (seconds):", 
                                   value = 2, min = 0.5, max = 10, step = 0.5),
                      p("Rate limit: ~40 requests/minute. Adjust delay to avoid hitting limits.", 
                        style = "font-size:11px; color:#666;"),
                      checkboxInput("ors_fallback", "Enable fallback to OSRM/Euclidean if ORS fails", value = TRUE),
                      p("If ORS API fails, automatically try OSRM, then Euclidean distance", 
                        style = "font-size:11px; color:#666;")
                    ),
                    conditionalPanel(
                      condition = "input.distance_method == 'osrm'",
                      p("Note: OSRM package uses public OSRM server. For production use, consider setting up your own OSRM server."),
                      numericInput("osrm_timeout", "OSRM Timeout (seconds):", value = 30, min = 10, max = 120),
                      checkboxInput("osrm_fallback", "Enable Euclidean fallback if OSRM fails", value = TRUE),
                      p("If OSRM routing fails, automatically fall back to Euclidean distance", 
                        style = "font-size:11px; color:#666;")
                    ),
                    conditionalPanel(
                      condition = "input.distance_method == 'euclidean'",
                      p("Fast calculation using straight-line distances. No fallback needed.", 
                        style = "font-size:11px; color:#666;")
                    ),
                    hr(),
                    h5("Display Options:"),
                    checkboxInput("enable_progress_bar", "Show visual progress bar during optimization", value = TRUE),
                    p("Display animated progress bar in addition to text logs", 
                      style = "font-size:11px; color:#666;")
                ),
                box(width = 6, title = "Optimization Algorithm", status = "success", solidHeader = TRUE,
                    radioButtons("optimization_method", "Choose optimization algorithm:",
                                 choices = list(
                                   "Linear Programming (Optimal solution, slower)" = "linear_programming",
                                   "Greedy Heuristic (Fast approximation)" = "greedy"
                                 ),
                                 selected = "linear_programming"),
                    numericInput("capacity_flexibility", "Capacity Flexibility (%):",
                                 value = 5, min = 0, max = 50, step = 1),
                    p("Allow labs to exceed capacity by this percentage", style = "font-size:12px; color:#666;"),
                    checkboxInput("prioritize_balance", "Prioritize Load Balancing",
                                  value = FALSE),
                    p("Give preference to more balanced lab utilization", style = "font-size:12px; color:#666;"),
                    checkboxInput("enable_detailed_logs", "Enable Detailed Optimization Logs", value = TRUE)
                )
              ),
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Run Optimization",
                    actionButton("run_optimization", "Run Optimization", 
                                 icon = icon("play"), class = "btn-primary btn-lg",
                                 style = "margin-bottom: 15px;"),
                    br(),
                    # Progress bar
                    conditionalPanel(
                      condition = "input.enable_progress_bar == true",
                      div(id = "progress_container", style = "margin-bottom: 15px;",
                          h5("Optimization Progress:"),
                          div(class = "progress", style = "height: 25px;",
                              div(id = "progress_bar", class = "progress-bar progress-bar-striped progress-bar-animated", 
                                  role = "progressbar", style = "width: 0%", 
                                  tags$span(id = "progress_text", "Ready to start...")
                              )
                          )
                      )
                    ),
                    h4("Optimization Logs:"),
                    div(id = "optimization_status", class = "progress-text",
                        style = "min-height: 150px;",
                        textOutput("optimization_status")),
                    br(),
                    actionButton("test_logging", "Test Logging System", 
                                 class = "btn-secondary btn-sm",
                                 style = "margin-top: 10px;")
                )
              )
      ),
      
      tabItem(tabName = "results",
              h2("Optimization Results"),
              fluidRow(
                box(width = 12, title = "Summary Statistics", status = "info", solidHeader = TRUE,
                    tableOutput("summary_table") %>% withSpinner())
              ),
              fluidRow(
                box(width = 6, title = "Lab Utilization Comparison", status = "primary", solidHeader = TRUE,
                    plotOutput("utilization_plot", height = "400px") %>% withSpinner(),
                    downloadButton("download_utilization", "Download Plot", class = "btn-sm")),
                box(width = 6, title = "Top Travel Time Improvements", status = "success", solidHeader = TRUE,
                    plotOutput("improvement_plot", height = "400px") %>% withSpinner(),
                    downloadButton("download_improvement", "Download Plot", class = "btn-sm"))
              ),
              # NEW: Travel Time Distribution Histograms
              fluidRow(
                box(width = 12, title = "Travel Time Distribution Comparison", status = "warning", solidHeader = TRUE,
                    plotOutput("travel_time_histograms", height = "500px") %>% withSpinner(),
                    downloadButton("download_histograms", "Download Histograms", class = "btn-sm"),
                    br(), br(),
                    p("These histograms show how the optimization affects the overall distribution of travel times across all districts. 
                      The left panel shows individual histograms, while the right panel shows overlapping density curves for direct comparison.", 
                      style = "font-size:12px; color:#666; margin-top:10px;"))
              ),
              fluidRow(
                box(width = 12, title = "Detailed Assignments", status = "warning", solidHeader = TRUE,
                    DTOutput("assignments_table") %>% withSpinner(),
                    br(),
                    downloadButton("download_assignments", "Download Assignment Data", class = "btn-primary"))
              )
      ),
      
      tabItem(tabName = "map",
              h2("Network Optimization Map"),
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Interactive Network Map",
                    p("🔴 Red markers: CDST Labs | 🔵 Blue circles: District HQs | 🟢 Green lines: New assignments"),
                    leafletOutput("network_map", height = "700px") %>% withSpinner())
              )
      ),
      
      tabItem(tabName = "isochrone",
              h2("Isochrone Analysis"),
              fluidRow(
                box(width = 4, title = "Isochrone Settings", status = "info", solidHeader = TRUE,
                    selectInput("isochrone_lab", "Select CDST Lab:",
                                choices = NULL),
                    numericInput("isochrone_time", "Travel Time (minutes):",
                                 value = 60, min = 15, max = 180, step = 15),
                    radioButtons("isochrone_method", "Calculation Method:",
                                 choices = list(
                                   "OSRM Isochrone (Route-based)" = "osrm",
                                   "Euclidean Buffer (Distance-based)" = "euclidean"
                                 ),
                                 selected = "euclidean"),
                    actionButton("generate_isochrone", "Generate Isochrone",
                                 icon = icon("clock"), class = "btn-primary"),
                    br(), br(),
                    textOutput("isochrone_status")
                ),
                box(width = 8, title = "Isochrone Map", status = "primary", solidHeader = TRUE,
                    leafletOutput("isochrone_map", height = "600px") %>% withSpinner())
              ),
              fluidRow(
                box(width = 12, title = "Districts Within Isochrone", status = "success", solidHeader = TRUE,
                    DTOutput("isochrone_districts") %>% withSpinner())
              )
      ),
      
      tabItem(tabName = "about",
              h2("About This Tool"),
              box(width = 12,
                  h3("Diagnostic Network Optimization Tool"),
                  p("This tool helps optimize the assignment of district laboratories to CDST (Culture Drug Sensitivity Testing) facilities to minimize travel time while respecting capacity constraints."),
                  
                  h4("Key Features:"),
                  tags$ul(
                    tags$li("📊 Multiple distance calculation methods with automatic fallbacks"),
                    tags$li("🧮 Two optimization algorithms (Linear Programming, Greedy Heuristic)"),
                    tags$li("🗺️ Interactive mapping and visualization with detailed popups"),
                    tags$li("⏰ Isochrone analysis for accessibility studies"),
                    tags$li("⚙️ Customizable capacity constraints and optimization parameters"),
                    tags$li("📈 Comprehensive results visualization and export capabilities")
                  ),
                  
                  h4("Distance Calculation Methods:"),
                  tags$ul(
                    tags$li(tags$b("OpenRouteService API:"), "Most accurate driving times using real road networks and traffic patterns"),
                    tags$li(tags$b("OSRM Package:"), "Good balance of accuracy and speed using open-source routing engine"),
                    tags$li(tags$b("Euclidean Distance:"), "Fastest method using straight-line distances (suitable for quick analysis)")
                  ),
                  
                  h4("Optimization Algorithms:"),
                  tags$ul(
                    tags$li(tags$b("Linear Programming:"), "Finds the mathematically optimal solution using integer programming"),
                    tags$li(tags$b("Greedy Heuristic:"), "Fast approximation method suitable for large datasets")
                  ),
                  
                  h4("Data Requirements:"),
                  tags$ul(
                    tags$li("District lab data: Location coordinates, current CDST assignments, quarterly test volumes"),
                    tags$li("CDST lab data: Location coordinates, quarterly capacity limits"),
                    tags$li("State boundary: GeoJSON or Shapefile for geographic context")
                  ),
                  
                  hr(),
                  p(tags$b("Version:"), "2.0 Enhanced | ", 
                    tags$b("Created by:"), "PATH | ",
                    tags$b("License:"), "Non-commercial use only"),
                  p(style = "font-size:12px; color:#666;", 
                    "For technical support or feature requests, please contact the development team.")
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Reactive values for storing data
  rv <- reactiveValues(
    district_labs = NULL,
    cdst_labs = NULL,
    shapefile = NULL,
    optimization_results = NULL,
    lab_utilization = NULL,
    travel_time_matrix = NULL,
    optimization_method = "Not run yet",
    distance_method = "Not selected",
    isochrone_data = NULL,
    current_isochrone = NULL,
    optimization_logs = "System initialized - logs will appear here during optimization",
    data_validation = list(districts = FALSE, cdst = FALSE, shapefile = FALSE)
  )
  
  # Enhanced logging function with proper reactivity
  log_message <- function(message, type = "INFO") {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    log_entry <- paste0("[", timestamp, "] ", type, ": ", message)
    
    # Update the reactive value
    if (rv$optimization_logs == "") {
      rv$optimization_logs <- log_entry
    } else {
      rv$optimization_logs <- paste(rv$optimization_logs, log_entry, sep = "\n")
    }
    
    # Print to R console for debugging
    cat(log_entry, "\n")
    
    # Force UI update by invalidating later
    invalidateLater(10, session)
  }
  
  # Progress bar update function
  update_progress_bar <- function(progress_percent, message = "") {
    if (input$enable_progress_bar) {
      session$sendCustomMessage("updateProgressBar", list(
        percent = progress_percent,
        message = message
      ))
    }
  }
  
  # Function to get travel time from OpenRouteService with enhanced error handling
  get_travel_time_ors <- function(start_coords, end_coords, api_key, delay = 2, enable_fallback = TRUE) {
    tryCatch({
      # Add delay to respect rate limits
      Sys.sleep(delay)
      
      url <- "https://api.openrouteservice.org/v2/directions/driving-car"
      
      body <- list(
        coordinates = list(
          c(start_coords[2], start_coords[1]),  # ORS expects [lon, lat]
          c(end_coords[2], end_coords[1])
        )
      )
      
      response <- POST(
        url,
        add_headers("Authorization" = paste("Bearer", api_key)),
        body = body,
        encode = "json",
        timeout(30)
      )
      
      if (status_code(response) == 200) {
        result <- content(response, "parsed")
        # Return travel time in minutes
        return(result$routes[[1]]$summary$duration / 60)
      } else {
        warning(paste("ORS API returned status code:", status_code(response)))
        stop("API request failed")
      }
    }, error = function(e) {
      warning(paste("ORS API call failed:", e$message))
      if (enable_fallback) {
        log_message("ORS failed, trying OSRM fallback", type = "WARN")
        tryCatch({
          return(get_travel_time_osrm(start_coords, end_coords, timeout = 30, enable_fallback = TRUE))
        }, error = function(e2) {
          log_message("OSRM also failed, using Euclidean fallback", type = "WARN")
          return(get_travel_time_euclidean(start_coords, end_coords))
        })
      } else {
        stop("ORS API call failed and fallback disabled")
      }
    })
  }
  
  # Function to get travel time from OSRM with enhanced error handling
  get_travel_time_osrm <- function(start_coords, end_coords, timeout = 30, enable_fallback = TRUE) {
    tryCatch({
      # Create temporary data frames for OSRM
      src <- data.frame(id = 1, lon = start_coords[2], lat = start_coords[1])
      dst <- data.frame(id = 1, lon = end_coords[2], lat = end_coords[1])
      
      # Calculate travel time using OSRM
      result <- osrmRoute(src = src, dst = dst, returnclass = "sf")
      
      # Return travel time in minutes
      return(result$duration)
    }, error = function(e) {
      warning(paste("OSRM routing failed:", e$message))
      if (enable_fallback) {
        # Fallback to Euclidean distance
        return(get_travel_time_euclidean(start_coords, end_coords))
      } else {
        stop("OSRM routing failed and fallback disabled")
      }
    })
  }
  
  # Function to calculate Euclidean distance
  get_travel_time_euclidean <- function(start_coords, end_coords) {
    # Euclidean distance in kilometers (rough conversion using 111 km per degree)
    distance_km <- sqrt((start_coords[1] - end_coords[1])^2 + 
                          (start_coords[2] - end_coords[2])^2) * 111
    # Convert to time assuming average speed of 30 km/h
    return(distance_km * 2)  # Return in minutes
  }
  
  # Data validation function
  validate_data <- function() {
    status_messages <- c()
    
    # Validate district labs
    if (!is.null(rv$district_labs)) {
      required_cols <- c("district", "current_cdst", "lat", "lon", "tests_per_quarter")
      if (all(required_cols %in% names(rv$district_labs))) {
        if (all(!is.na(rv$district_labs$lat) & !is.na(rv$district_labs$lon))) {
          rv$data_validation$districts <- TRUE
          status_messages <- c(status_messages, "✅ District lab data: Valid")
        } else {
          status_messages <- c(status_messages, "❌ District lab data: Missing coordinates")
        }
      } else {
        status_messages <- c(status_messages, "❌ District lab data: Missing required columns")
      }
    } else {
      status_messages <- c(status_messages, "⏳ District lab data: Not uploaded")
    }
    
    # Validate CDST labs
    if (!is.null(rv$cdst_labs)) {
      required_cols <- c("lab_name", "address", "lat", "lon", "capacity")
      if (all(required_cols %in% names(rv$cdst_labs))) {
        if (all(!is.na(rv$cdst_labs$lat) & !is.na(rv$cdst_labs$lon))) {
          rv$data_validation$cdst <- TRUE
          status_messages <- c(status_messages, "✅ CDST lab data: Valid")
        } else {
          status_messages <- c(status_messages, "❌ CDST lab data: Missing coordinates")
        }
      } else {
        status_messages <- c(status_messages, "❌ CDST lab data: Missing required columns")
      }
    } else {
      status_messages <- c(status_messages, "⏳ CDST lab data: Not uploaded")
    }
    
    # Validate shapefile
    if (!is.null(rv$shapefile)) {
      rv$data_validation$shapefile <- TRUE
      status_messages <- c(status_messages, "✅ Shapefile: Valid")
    } else {
      status_messages <- c(status_messages, "⏳ Shapefile: Not uploaded (optional)")
    }
    
    return(paste(status_messages, collapse = "\n"))
  }
  
  # Download templates
  output$download_district_template <- downloadHandler(
    filename = function() {
      "district_lab_template.csv"
    },
    content = function(file) {
      template <- data.frame(
        district = c("Sample District 1", "Sample District 2"),
        current_cdst = c("CDST Lab A", "CDST Lab B"),
        lat = c(25.5937, 26.2124),
        lon = c(85.1376, 78.1772),
        tests_per_quarter = c(150, 200),
        stringsAsFactors = FALSE
      )
      write.csv(template, file, row.names = FALSE)
    }
  )
  
  output$download_cdst_template <- downloadHandler(
    filename = function() {
      "cdst_lab_template.csv"
    },
    content = function(file) {
      template <- data.frame(
        lab_name = c("CDST Lab A", "CDST Lab B"),
        address = c("123 Main St, City A", "456 Park Ave, City B"),
        lat = c(25.5937, 26.2124),
        lon = c(85.1376, 78.1772),
        capacity = c(500, 750),
        stringsAsFactors = FALSE
      )
      write.csv(template, file, row.names = FALSE)
    }
  )
  
  # Read uploaded files with enhanced validation
  observeEvent(input$district_file, {
    req(input$district_file)
    tryCatch({
      rv$district_labs <- read_csv(input$district_file$datapath, show_col_types = FALSE)
      names(rv$district_labs) <- c("district", "current_cdst", "lat", "lon", "tests_per_quarter")
      
      # Convert numeric columns
      rv$district_labs$lat <- as.numeric(rv$district_labs$lat)
      rv$district_labs$lon <- as.numeric(rv$district_labs$lon)
      rv$district_labs$tests_per_quarter <- as.numeric(rv$district_labs$tests_per_quarter)
      
      showNotification("District lab data uploaded successfully!", type = "message")
      log_message(paste("Loaded", nrow(rv$district_labs), "district laboratories"))
    }, error = function(e) {
      showNotification(paste("Error loading district data:", e$message), type = "error")
      log_message(paste("ERROR loading district data:", e$message), type = "ERROR")
      rv$district_labs <- NULL
    })
  })
  
  observeEvent(input$cdst_file, {
    req(input$cdst_file)
    tryCatch({
      rv$cdst_labs <- read_csv(input$cdst_file$datapath, show_col_types = FALSE)
      names(rv$cdst_labs) <- c("lab_name", "address", "lat", "lon", "capacity")
      
      # Convert numeric columns
      rv$cdst_labs$lat <- as.numeric(rv$cdst_labs$lat)
      rv$cdst_labs$lon <- as.numeric(rv$cdst_labs$lon)
      rv$cdst_labs$capacity <- as.numeric(rv$cdst_labs$capacity)
      
      # Update isochrone lab choices
      updateSelectInput(session, "isochrone_lab",
                        choices = setNames(rv$cdst_labs$lab_name, rv$cdst_labs$lab_name))
      
      showNotification("CDST lab data uploaded successfully!", type = "message")
      log_message(paste("Loaded", nrow(rv$cdst_labs), "CDST laboratories"))
    }, error = function(e) {
      showNotification(paste("Error loading CDST data:", e$message), type = "error")
      rv$cdst_labs <- NULL
    })
  })
  
  observeEvent(input$shapefile, {
    req(input$shapefile)
    tryCatch({
      # Handle shapefile upload (multiple files)
      if (length(input$shapefile$datapath) > 1) {
        log_message("Processing shapefile with multiple components...")
        
        # Create a temporary directory for shapefile components
        temp_dir <- tempdir()
        shapefile_dir <- file.path(temp_dir, "shapefile_upload")
        if (!dir.exists(shapefile_dir)) {
          dir.create(shapefile_dir)
        }
        
        # Copy all uploaded files to temp directory with original names
        for (i in 1:length(input$shapefile$datapath)) {
          original_name <- input$shapefile$name[i]
          temp_path <- input$shapefile$datapath[i]
          new_path <- file.path(shapefile_dir, original_name)
          file.copy(temp_path, new_path, overwrite = TRUE)
        }
        
        # Find the .shp file
        shp_files <- list.files(shapefile_dir, pattern = "\\.shp$", full.names = TRUE)
        if (length(shp_files) == 0) {
          showNotification("No .shp file found in upload. Please upload all shapefile components (.shp, .dbf, .shx, .prj)", type = "error")
          log_message("ERROR: No .shp file found in upload", type = "ERROR")
          return()
        }
        
        # Read shapefile
        shp_file <- shp_files[1]  # Use the first .shp file found
        log_message(paste("Reading shapefile:", basename(shp_file)))
        
        rv$shapefile <- st_read(shp_file, quiet = TRUE)
        showNotification("Shapefile uploaded successfully!", type = "message")
        log_message(paste("Shapefile loaded successfully with", nrow(rv$shapefile), "features"))
        
      } else {
        # Handle single file upload (GeoJSON or single shapefile attempt)
        if (grepl("\\.geojson$", input$shapefile$name, ignore.case = TRUE)) {
          log_message("Processing GeoJSON file...")
          rv$shapefile <- st_read(input$shapefile$datapath, quiet = TRUE)
          showNotification("GeoJSON uploaded successfully!", type = "message")
          log_message(paste("GeoJSON loaded successfully with", nrow(rv$shapefile), "features"))
        } else if (grepl("\\.shp$", input$shapefile$name, ignore.case = TRUE)) {
          showNotification("Shapefile requires multiple files (.shp, .dbf, .shx, .prj). Please select all shapefile components together.", type = "error")
          log_message("ERROR: Single .shp file uploaded - need all components", type = "ERROR")
          return()
        } else {
          showNotification("Please upload a valid GeoJSON file or all shapefile components (.shp, .dbf, .shx, .prj)", type = "error")
          log_message("ERROR: Unsupported file format", type = "ERROR")
          return()
        }
      }
      
    }, error = function(e) {
      showNotification(paste("Error loading spatial file:", e$message), type = "error")
      log_message(paste("ERROR loading spatial file:", e$message), type = "ERROR")
      rv$shapefile <- NULL
    })
  })
  
  # Test logging system
  observeEvent(input$test_logging, {
    log_message("Testing logging system - this message should appear in the logs area")
    log_message("System time check", type = "INFO")
    log_message("Warning test message", type = "WARN") 
    log_message("Error test message", type = "ERROR")
    showNotification("Test messages added to logs", type = "message")
  })
  
  # Data validation status output
  output$data_validation_status <- renderText({
    validate_data()
  })
  
  # Optimization status/logs output - ensure proper reactivity
  output$optimization_status <- renderText({
    # Create explicit reactive dependency
    current_logs <- rv$optimization_logs
    
    if (is.null(current_logs) || current_logs == "") {
      return("Ready to run optimization. Please upload data files first.")
    } else {
      return(current_logs)
    }
  })
  
  # Enhanced Linear Programming Optimization function
  optimize_lab_assignment_lp <- function(travel_times, capacities, demands, capacity_flex = 0.05, balance_priority = FALSE) {
    log_message("Starting Linear Programming optimization")
    
    n_districts <- nrow(travel_times)
    n_labs <- ncol(travel_times)
    
    log_message(paste("Problem size:", n_districts, "districts,", n_labs, "labs"))
    
    # Decision variables: x_ij = 1 if district i assigned to lab j, 0 otherwise
    n_vars <- n_districts * n_labs
    
    # Objective function coefficients
    if (balance_priority) {
      log_message("Applying load balancing priority")
      # Add penalty for imbalanced utilization
      avg_utilization <- sum(demands) / sum(capacities)
      balance_penalty <- matrix(0, nrow = n_districts, ncol = n_labs)
      for (j in 1:n_labs) {
        target_load <- capacities[j] * avg_utilization
        for (i in 1:n_districts) {
          balance_penalty[i, j] <- abs(demands[i] - target_load) / target_load
        }
      }
      objective_coeffs <- as.vector(travel_times * matrix(demands, nrow = n_districts, ncol = n_labs) + 
                                      balance_penalty * 10)  # Weight balance penalty
    } else {
      objective_coeffs <- as.vector(travel_times * matrix(demands, nrow = n_districts, ncol = n_labs))
    }
    
    # Constraint matrix setup
    n_constraints <- n_districts + n_labs
    constraint_matrix <- matrix(0, nrow = n_constraints, ncol = n_vars)
    constraint_directions <- character(n_constraints)
    constraint_rhs <- numeric(n_constraints)
    
    log_message("Setting up constraints")
    
    # Constraint 1: Each district must be assigned to exactly one lab
    for (i in 1:n_districts) {
      constraint_row <- i
      for (j in 1:n_labs) {
        var_index <- (i - 1) * n_labs + j
        constraint_matrix[constraint_row, var_index] <- 1
      }
      constraint_directions[constraint_row] <- "="
      constraint_rhs[constraint_row] <- 1
    }
    
    # Constraint 2: Lab capacity limits (with flexibility)
    for (j in 1:n_labs) {
      constraint_row <- n_districts + j
      for (i in 1:n_districts) {
        var_index <- (i - 1) * n_labs + j
        constraint_matrix[constraint_row, var_index] <- demands[i]
      }
      constraint_directions[constraint_row] <- "<="
      constraint_rhs[constraint_row] <- capacities[j] * (1 + capacity_flex)
    }
    
    log_message(paste("Solving LP with", capacity_flex * 100, "% capacity flexibility"))
    
    # Solve the LP problem
    lp_result <- lp(
      direction = "min",
      objective.in = objective_coeffs,
      const.mat = constraint_matrix,
      const.dir = constraint_directions,
      const.rhs = constraint_rhs,
      all.bin = TRUE,
      compute.sens = FALSE
    )
    
    # Extract solution
    if (lp_result$status == 0) {
      log_message("LP solved successfully - optimal solution found")
      solution_vector <- lp_result$solution
      assignment_matrix <- matrix(solution_vector, nrow = n_districts, ncol = n_labs, byrow = TRUE)
      lab_loads <- colSums(assignment_matrix * matrix(demands, nrow = n_districts, ncol = n_labs))
    } else {
      log_message(paste("LP solver failed with status:", lp_result$status), type = "ERROR")
      # If infeasible, return empty assignment
      assignment_matrix <- matrix(0, nrow = n_districts, ncol = n_labs)
      lab_loads <- rep(0, n_labs)
    }
    
    return(list(
      assignment = assignment_matrix,
      lab_loads = lab_loads,
      objective_value = ifelse(lp_result$status == 0, lp_result$objval, Inf),
      status = lp_result$status
    ))
  }
  
  # Enhanced Greedy heuristic optimization
  optimize_lab_assignment_greedy <- function(districts, labs, travel_times, balance_priority = FALSE) {
    log_message("Starting Greedy Heuristic optimization")
    
    n_districts <- nrow(districts)
    n_labs <- nrow(labs)
    
    # Initialize assignments and remaining capacities
    assignments <- rep(NA, n_districts)
    remaining_capacity <- labs$capacity
    
    # Sort districts by demand (highest first) or by travel time priority
    if (balance_priority) {
      log_message("Applying load balancing priority")
      # Sort by a combination of demand and average travel time
      avg_travel_time <- rowMeans(travel_times)
      priority_score <- districts$tests_per_quarter / avg_travel_time
      demand_order <- order(priority_score, decreasing = TRUE)
    } else {
      demand_order <- order(districts$tests_per_quarter, decreasing = TRUE)
    }
    
    # Assign districts to labs
    assigned_count <- 0
    for (i in demand_order) {
      # Find available labs sorted by travel time
      lab_order <- order(travel_times[i, ])
      
      assigned <- FALSE
      for (j in lab_order) {
        if (remaining_capacity[j] >= districts$tests_per_quarter[i]) {
          assignments[i] <- j
          remaining_capacity[j] <- remaining_capacity[j] - districts$tests_per_quarter[i]
          assigned <- TRUE
          assigned_count <- assigned_count + 1
          break
        }
      }
      
      # If no lab has enough capacity, assign to the one with most remaining capacity
      if (!assigned) {
        best_lab <- which.max(remaining_capacity)
        assignments[i] <- best_lab
        remaining_capacity[best_lab] <- max(0, remaining_capacity[best_lab] - districts$tests_per_quarter[i])
        assigned_count <- assigned_count + 1
        log_message(paste("District", districts$district[i], "assigned to", labs$lab_name[best_lab], 
                          "with capacity overflow"), type = "WARN")
      }
    }
    
    log_message(paste("Greedy algorithm completed -", assigned_count, "districts assigned"))
    
    # Create assignment matrix
    assignment_matrix <- matrix(0, nrow = n_districts, ncol = n_labs)
    for (i in 1:n_districts) {
      if (!is.na(assignments[i])) {
        assignment_matrix[i, assignments[i]] <- 1
      }
    }
    
    # Calculate lab loads
    lab_loads <- colSums(assignment_matrix * matrix(districts$tests_per_quarter, 
                                                    nrow = n_districts, 
                                                    ncol = n_labs))
    
    # Calculate objective value
    objective_value <- sum(assignment_matrix * travel_times * 
                             matrix(districts$tests_per_quarter, 
                                    nrow = n_districts, 
                                    ncol = n_labs))
    
    return(list(
      assignment = assignment_matrix,
      lab_loads = lab_loads,
      objective_value = objective_value,
      status = ifelse(any(is.na(assignments)), 2, 0)
    ))
  }
  
  # Run optimization when button is clicked
  observeEvent(input$run_optimization, {
    req(rv$district_labs, rv$cdst_labs)
    
    # Clear previous results and logs
    rv$optimization_results <- NULL
    rv$lab_utilization <- NULL
    rv$travel_time_matrix <- NULL
    rv$optimization_logs <- ""
    
    # Reset progress bar
    update_progress_bar(0, "Initializing...")
    
    # Test the logging system
    log_message("Logging system initialized - messages will appear here")
    
    log_message("=== DIAGNOSTIC NETWORK OPTIMIZATION STARTED ===")
    log_message(paste("Method:", input$optimization_method))
    log_message(paste("Distance calculation:", input$distance_method))
    
    # Initialize progress bar
    update_progress_bar(0, "Starting optimization...")
    
    # Validate data one more time
    if (!rv$data_validation$districts || !rv$data_validation$cdst) {
      log_message("Data validation failed - please check uploaded files", type = "ERROR")
      showNotification("Please upload valid district and CDST lab data files", type = "error")
      update_progress_bar(0, "Validation failed")
      return()
    }
    
    n_districts <- nrow(rv$district_labs)
    n_labs <- nrow(rv$cdst_labs)
    
    log_message(paste("Processing", n_districts, "districts and", n_labs, "CDST labs"))
    update_progress_bar(5, "Data validation complete")
    
    travel_time_matrix <- matrix(0, nrow = n_districts, ncol = n_labs)
    rownames(travel_time_matrix) <- rv$district_labs$district
    colnames(travel_time_matrix) <- rv$cdst_labs$lab_name
    
    # Calculate travel times based on selected method
    calculation_success <- TRUE
    method_used <- input$distance_method
    
    log_message(paste("Calculating travel time matrix using", method_used))
    update_progress_bar(10, "Starting travel time calculations...")
    
    # Progress tracking
    total_calculations <- n_districts * n_labs
    completed_calculations <- 0
    
    for (i in 1:n_districts) {
      for (j in 1:n_labs) {
        start_coords <- c(rv$district_labs$lat[i], rv$district_labs$lon[i])
        end_coords <- c(rv$cdst_labs$lat[j], rv$cdst_labs$lon[j])
        
        tt <- tryCatch({
          switch(input$distance_method,
                 "ors_api" = get_travel_time_ors(start_coords, end_coords, 
                                                 input$ors_api_key, input$api_delay, input$ors_fallback),
                 "osrm" = get_travel_time_osrm(start_coords, end_coords, 
                                               input$osrm_timeout, input$osrm_fallback),
                 "euclidean" = get_travel_time_euclidean(start_coords, end_coords)
          )
        }, error = function(e) {
          calculation_success <<- FALSE
          log_message(paste("Failed to calculate travel time for", 
                            rv$district_labs$district[i], "to", 
                            rv$cdst_labs$lab_name[j], "- using Euclidean fallback"), type = "WARN")
          # Final fallback to Euclidean
          get_travel_time_euclidean(start_coords, end_coords)
        })
        
        travel_time_matrix[i, j] <- tt
        completed_calculations <- completed_calculations + 1
        
        # Update progress bar (10% to 70% for travel time calculations)
        progress_percent <- round(10 + (completed_calculations / total_calculations * 60),2)
        progress_message <- paste("Calculating travel times:", completed_calculations, "of", total_calculations)
        update_progress_bar(progress_percent, progress_message)
        
        # Log progress more frequently for user feedback
        if (completed_calculations %% max(1, total_calculations %/% 20) == 0) {
          progress_pct_display <- round(10 + (completed_calculations / total_calculations * 60), 2)
          log_message(paste("Travel time calculations:", completed_calculations, "of", total_calculations, 
                            "completed (", progress_pct_display, "%)"))
        }
      }
    }
    
    if (!calculation_success) {
      method_used <- paste(method_used, "(with fallback)")
      log_message("Some travel time calculations failed - fallback methods used", type = "WARN")
      showNotification("Some travel time calculations failed - using fallback methods", 
                       type = "warning")
    }
    
    rv$travel_time_matrix <- travel_time_matrix
    rv$distance_method <- method_used
    
    log_message("Travel time matrix calculation completed")
    log_message(paste("Matrix statistics - Average:", round(mean(travel_time_matrix), 2), "min, Max:", round(max(travel_time_matrix), 2), "min"))
    update_progress_bar(75, "Running optimization algorithm...")
    
    # Run optimization based on selected algorithm
    log_message("=== STARTING OPTIMIZATION ALGORITHM ===")
    log_message(paste("Selected algorithm:", input$optimization_method))
    log_message(paste("Capacity flexibility:", input$capacity_flexibility, "%"))
    log_message(paste("Load balancing priority:", input$prioritize_balance))
    
    if (input$optimization_method == "linear_programming") {
      optimization_result <- optimize_lab_assignment_lp(
        travel_time_matrix, 
        rv$cdst_labs$capacity, 
        rv$district_labs$tests_per_quarter,
        input$capacity_flexibility / 100,
        input$prioritize_balance
      )
      rv$optimization_method <- "Linear Programming"
    } else {
      optimization_result <- optimize_lab_assignment_greedy(
        rv$district_labs,
        rv$cdst_labs,
        travel_time_matrix,
        input$prioritize_balance
      )
      rv$optimization_method <- "Greedy Heuristic"
    }
    
    update_progress_bar(90, "Processing results...")
    log_message("=== PROCESSING RESULTS ===")
    
    # Process results
    results <- data.frame(
      district = rv$district_labs$district,
      current_assignment = rv$district_labs$current_cdst,
      optimized_assignment = "",
      current_travel_time = 0,
      optimized_travel_time = 0,
      improvement_minutes = 0,
      district_lat = rv$district_labs$lat,
      district_lon = rv$district_labs$lon,
      tests_per_quarter = rv$district_labs$tests_per_quarter,
      stringsAsFactors = FALSE
    )
    
    # Fill in optimized assignments
    for (i in 1:nrow(rv$district_labs)) {
      assigned_lab_idx <- which(optimization_result$assignment[i, ] == 1)
      if (length(assigned_lab_idx) > 0) {
        results$optimized_assignment[i] <- rv$cdst_labs$lab_name[assigned_lab_idx]
        results$optimized_travel_time[i] <- travel_time_matrix[i, assigned_lab_idx]
      }
      
      # Calculate current travel time - FIXED BUG HERE
      current_lab_idx <- which(rv$cdst_labs$lab_name == rv$district_labs$current_cdst[i])
      if (length(current_lab_idx) > 0) {
        results$current_travel_time[i] <- travel_time_matrix[i, current_lab_idx]
      }
    }
    
    # Calculate improvements
    results$improvement_minutes <- results$current_travel_time - results$optimized_travel_time
    
    # Lab utilization summary
    lab_utilization <- data.frame(
      lab_name = rv$cdst_labs$lab_name,
      capacity = rv$cdst_labs$capacity,
      current_load = 0,
      optimized_load = optimization_result$lab_loads,
      current_utilization = 0,
      optimized_utilization = optimization_result$lab_loads / rv$cdst_labs$capacity * 100
    )
    
    # Calculate current load
    for (i in 1:nrow(rv$cdst_labs)) {
      current_districts <- rv$district_labs[rv$district_labs$current_cdst == rv$cdst_labs$lab_name[i], ]
      lab_utilization$current_load[i] <- sum(current_districts$tests_per_quarter, na.rm = TRUE)
      lab_utilization$current_utilization[i] <- lab_utilization$current_load[i] / rv$cdst_labs$capacity[i] * 100
    }
    
    rv$optimization_results <- results
    rv$lab_utilization <- lab_utilization
    
    # Log final results
    reassigned_count <- sum(results$current_assignment != results$optimized_assignment)
    total_time_saved <- sum(results$improvement_minutes)
    avg_time_saved <- mean(results$improvement_minutes)
    
    log_message("=== OPTIMIZATION COMPLETED SUCCESSFULLY ===")
    log_message(paste("Districts reassigned:", reassigned_count, "of", nrow(results)))
    log_message(paste("Total time saved per quarter:", round(total_time_saved, 2), "minutes"))
    log_message(paste("Average time saved per district:", round(avg_time_saved, 2), "minutes"))
    log_message(paste("Optimization objective value:", round(optimization_result$objective_value, 2)))
    log_message("Results are now available in the Results and Map tabs")
    
    update_progress_bar(100, "Optimization completed successfully!")
    showNotification("Optimization completed successfully!", type = "message")
  })
  
  # Generate isochrone with enhanced status reporting
  observeEvent(input$generate_isochrone, {
    req(rv$cdst_labs, input$isochrone_lab, input$isochrone_time)
    
    output$isochrone_status <- renderText({
      "Generating isochrone..."
    })
    
    # Get selected lab coordinates
    lab_idx <- which(rv$cdst_labs$lab_name == input$isochrone_lab)
    lab_coords <- c(rv$cdst_labs$lat[lab_idx], rv$cdst_labs$lon[lab_idx])
    
    if (input$isochrone_method == "osrm") {
      tryCatch({
        # Create isochrone using OSRM
        iso_result <- osrmIsochrone(
          loc = c(rv$cdst_labs$lon[lab_idx], rv$cdst_labs$lat[lab_idx]),
          breaks = input$isochrone_time,
          returnclass = "sf"
        )
        rv$current_isochrone <- iso_result
        output$isochrone_status <- renderText({
          paste("OSRM isochrone generated successfully for", input$isochrone_time, "minutes")
        })
        showNotification("Isochrone generated successfully", type = "message")
      }, error = function(e) {
        output$isochrone_status <- renderText({
          paste("OSRM failed:", e$message, "- generating Euclidean buffer instead")
        })
        showNotification("OSRM isochrone failed - generating Euclidean buffer", type = "warning")
        
        # Fallback to Euclidean buffer
        lab_point <- st_sfc(st_point(c(rv$cdst_labs$lon[lab_idx], rv$cdst_labs$lat[lab_idx])), 
                            crs = 4326)
        # Convert time to approximate distance (assuming 30 km/h)
        buffer_distance_km <- (input$isochrone_time / 60) * 30
        
        # Transform to a projected CRS for accurate buffering (using UTM zone estimation)
        # This is a rough approach - for production, you'd want to determine the correct UTM zone
        lab_projected <- st_transform(lab_point, crs = 3857)  # Web Mercator
        iso_buffer_projected <- st_buffer(lab_projected, dist = buffer_distance_km * 1000)  # meters
        iso_buffer <- st_transform(iso_buffer_projected, crs = 4326)  # back to WGS84
        
        rv$current_isochrone <- st_sf(
          isomin = input$isochrone_time,
          geometry = iso_buffer
        )
      })
    } else {
      # Euclidean buffer method - FIXED VERSION
      tryCatch({
        lab_point <- st_sfc(st_point(c(rv$cdst_labs$lon[lab_idx], rv$cdst_labs$lat[lab_idx])), 
                            crs = 4326)
        # Convert time to approximate distance (assuming 30 km/h)
        buffer_distance_km <- (input$isochrone_time / 60) * 30
        
        # Transform to a projected CRS for accurate buffering
        lab_projected <- st_transform(lab_point, crs = 3857)  # Web Mercator
        iso_buffer_projected <- st_buffer(lab_projected, dist = buffer_distance_km * 1000)  # convert km to meters
        iso_buffer <- st_transform(iso_buffer_projected, crs = 4326)  # back to WGS84
        
        rv$current_isochrone <- st_sf(
          isomin = input$isochrone_time,
          geometry = iso_buffer
        )
        
        # Debug information
        print(paste("Isochrone created with geometry type:", st_geometry_type(rv$current_isochrone)[1]))
        print(paste("Isochrone CRS:", st_crs(rv$current_isochrone)$input))
        
        output$isochrone_status <- renderText({
          paste("Euclidean buffer generated successfully for", input$isochrone_time, "minutes travel time.",
                "Buffer radius:", round(buffer_distance_km, 1), "km")
        })
        showNotification("Euclidean buffer generated successfully", type = "message")
      }, error = function(e) {
        output$isochrone_status <- renderText({
          paste("Error generating buffer:", e$message)
        })
        showNotification(paste("Error generating isochrone:", e$message), type = "error")
      })
    }
  })
  
  # Enhanced summary table
  output$summary_table <- renderTable({
    req(rv$optimization_results, rv$lab_utilization)
    
    # Calculate additional metrics
    reassigned_districts <- sum(rv$optimization_results$current_assignment != rv$optimization_results$optimized_assignment)
    total_time_saved_hours <- sum(rv$optimization_results$improvement_minutes) / 60
    districts_improved <- sum(rv$optimization_results$improvement_minutes > 0)
    
    current_max_util <- rv$lab_utilization[which.max(rv$lab_utilization$current_utilization), ]
    optimized_max_util <- rv$lab_utilization[which.max(rv$lab_utilization$optimized_utilization), ]
    current_min_util <- rv$lab_utilization[which.min(rv$lab_utilization$current_utilization), ]
    optimized_min_util <- rv$lab_utilization[which.min(rv$lab_utilization$optimized_utilization), ]
    
    data.frame(
      Metric = c(
        "Distance Calculation Method",
        "Optimization Algorithm",
        "Total Districts",
        "Districts Reassigned",
        "Districts with Improved Travel Time",
        "Average Travel Time Reduction (min)",
        "Total Time Saved per Quarter (hours)",
        "Most Utilized Lab (Current)",
        "Most Utilized Lab (Optimized)",
        "Least Utilized Lab (Current)",
        "Least Utilized Lab (Optimized)",
        "Current Max Utilization (%)",
        "Optimized Max Utilization (%)",
        "Current Min Utilization (%)",
        "Optimized Min Utilization (%)"
      ),
      Value = c(
        rv$distance_method,
        rv$optimization_method,
        nrow(rv$district_labs),
        reassigned_districts,
        districts_improved,
        round(mean(rv$optimization_results$improvement_minutes), 2),
        round(total_time_saved_hours, 2),
        current_max_util$lab_name,
        optimized_max_util$lab_name,
        current_min_util$lab_name,
        optimized_min_util$lab_name,
        round(current_max_util$current_utilization, 1),
        round(optimized_max_util$optimized_utilization, 1),
        round(current_min_util$current_utilization, 1),
        round(optimized_min_util$optimized_utilization, 1)
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Enhanced utilization plot
  output$utilization_plot <- renderPlot({
    req(rv$lab_utilization)
    
    rv$lab_utilization %>%
      select(lab_name, current_utilization, optimized_utilization) %>%
      pivot_longer(cols = c(current_utilization, optimized_utilization),
                   names_to = "scenario", values_to = "utilization") %>%
      mutate(scenario = ifelse(scenario == "current_utilization", "Current", "Optimized")) %>%
      
      ggplot(aes(x = reorder(lab_name, utilization), y = utilization, fill = scenario)) +
      geom_col(position = "dodge", alpha = 0.8) +
      geom_hline(yintercept = 100, linetype = "dashed", color = "red", alpha = 0.7, size = 1) +
      geom_text(aes(label = paste0(round(utilization, 1), "%")), 
                position = position_dodge(width = 0.9), 
                hjust = -0.1, size = 3, fontface = "bold") +
      scale_fill_manual(values = c("Current" = "#FF6B6B", "Optimized" = "#4ECDC4"), name = "Scenario") +
      labs(title = "Lab Utilization Comparison: Current vs Optimized",
           subtitle = paste("Method:", rv$optimization_method, "|", rv$distance_method),
           x = "CDST Labs", 
           y = "Utilization (%)",
           caption = "Red dashed line indicates 100% capacity") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray50"),
        legend.position = "top",
        panel.grid.minor = element_blank()
      ) +
      coord_flip() +
      scale_y_continuous(limits = c(0, max(c(rv$lab_utilization$current_utilization, 
                                             rv$lab_utilization$optimized_utilization)) * 1.2))
  })
  
  # Enhanced improvement plot
  output$improvement_plot <- renderPlot({
    req(rv$optimization_results)
    
    top_improvements <- rv$optimization_results %>%
      filter(improvement_minutes > 0) %>%
      arrange(desc(improvement_minutes)) %>%
      slice_head(n = 20)
    
    if (nrow(top_improvements) == 0) {
      # Create empty plot with message
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No improvements found", size = 16) +
        theme_void()
    } else {
      top_improvements %>%
        ggplot(aes(x = reorder(district, improvement_minutes), y = improvement_minutes)) +
        geom_col(fill = "steelblue", alpha = 0.8, color = "navy", size = 0.3) +
        geom_text(aes(label = paste0(round(improvement_minutes, 1), "m")), 
                  hjust = -0.1, size = 3, fontface = "bold") +
        labs(title = paste("Top", min(20, nrow(top_improvements)), "Districts with Travel Time Improvements"),
             subtitle = paste("Method:", rv$optimization_method, "|", rv$distance_method),
             x = "District", 
             y = "Time Saved (minutes)",
             caption = paste("Total districts improved:", sum(rv$optimization_results$improvement_minutes > 0))) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12, color = "gray50"),
          panel.grid.minor = element_blank()
        ) +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
    }
  })
  
  # NEW: Travel Time Histograms
  output$travel_time_histograms <- renderPlot({
    req(rv$optimization_results)
    
    # Prepare data for histograms
    histogram_data <- rv$optimization_results %>%
      select(district, current_travel_time, optimized_travel_time) %>%
      pivot_longer(cols = c(current_travel_time, optimized_travel_time),
                   names_to = "scenario", values_to = "travel_time") %>%
      mutate(scenario = ifelse(scenario == "current_travel_time", "Pre-Optimization", "Post-Optimization"))
    
    # Calculate summary statistics
    current_mean <- mean(rv$optimization_results$current_travel_time, na.rm = TRUE)
    optimized_mean <- mean(rv$optimization_results$optimized_travel_time, na.rm = TRUE)
    current_median <- median(rv$optimization_results$current_travel_time, na.rm = TRUE)
    optimized_median <- median(rv$optimization_results$optimized_travel_time, na.rm = TRUE)
    
    # Create side-by-side histograms
    p1 <- histogram_data %>%
      ggplot(aes(x = travel_time, fill = scenario)) +
      geom_histogram(alpha = 0.7, bins = 25, position = "identity") +
      facet_wrap(~scenario, ncol = 2, scales = "free_y") +
      geom_vline(data = data.frame(
        scenario = c("Pre-Optimization", "Post-Optimization"),
        mean_val = c(current_mean, optimized_mean),
        median_val = c(current_median, optimized_median)
      ), aes(xintercept = mean_val), color = "red", linetype = "dashed", size = 1) +
      geom_vline(data = data.frame(
        scenario = c("Pre-Optimization", "Post-Optimization"),
        mean_val = c(current_mean, optimized_mean),
        median_val = c(current_median, optimized_median)
      ), aes(xintercept = median_val), color = "blue", linetype = "dotted", size = 1) +
      scale_fill_manual(values = c("Pre-Optimization" = "#FF6B6B", "Post-Optimization" = "#4ECDC4")) +
      labs(title = "Travel Time Distribution: Before vs After Optimization",
           subtitle = paste("Red dashed = Mean | Blue dotted = Median | Method:", rv$optimization_method),
           x = "Travel Time (minutes)",
           y = "Number of Districts",
           fill = "Scenario") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray50"),
        legend.position = "top",
        strip.text = element_text(face = "bold", size = 12),
        panel.grid.minor = element_blank()
      )
    
    # Create overlapping density curves
    p2 <- histogram_data %>%
      ggplot(aes(x = travel_time, fill = scenario, color = scenario)) +
      geom_density(alpha = 0.5, size = 1.2) +
      geom_vline(xintercept = current_mean, color = "#FF6B6B", linetype = "dashed", size = 1) +
      geom_vline(xintercept = optimized_mean, color = "#4ECDC4", linetype = "dashed", size = 1) +
      scale_fill_manual(values = c("Pre-Optimization" = "#FF6B6B", "Post-Optimization" = "#4ECDC4")) +
      scale_color_manual(values = c("Pre-Optimization" = "#FF6B6B", "Post-Optimization" = "#4ECDC4")) +
      labs(title = "Travel Time Density Comparison",
           subtitle = paste("Dashed lines show means | Overall improvement:", 
                            round(current_mean - optimized_mean, 1), "minutes"),
           x = "Travel Time (minutes)",
           y = "Density",
           fill = "Scenario",
           color = "Scenario") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray50"),
        legend.position = "top",
        panel.grid.minor = element_blank()
      )
    
    # Combine the plots using grid.arrange
    library(gridExtra)
    grid.arrange(p1, p2, ncol = 2, widths = c(2, 1))
  })
  
  # Enhanced assignments table
  output$assignments_table <- renderDT({
    req(rv$optimization_results)
    
    # Prepare display data
    display_data <- rv$optimization_results %>%
      select(
        District = district,
        `Current Assignment` = current_assignment,
        `Optimized Assignment` = optimized_assignment,
        `Current Travel Time (min)` = current_travel_time,
        `Optimized Travel Time (min)` = optimized_travel_time,
        `Time Saved (min)` = improvement_minutes,
        `Tests per Quarter` = tests_per_quarter
      ) %>%
      mutate(
        `Assignment Changed` = ifelse(`Current Assignment` != `Optimized Assignment`, "Yes", "No")
      )
    
    datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = c(3, 4, 5), className = "dt-right"),
          list(targets = 7, className = "dt-center")
        )
      ),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatRound(columns = c('Current Travel Time (min)', 'Optimized Travel Time (min)', 'Time Saved (min)'), digits = 1) %>%
      formatStyle(
        'Assignment Changed',
        backgroundColor = styleEqual('Yes', '#e8f5e8')
      ) %>%
      formatStyle(
        'Time Saved (min)',
        background = styleColorBar(range(display_data$`Time Saved (min)`, na.rm = TRUE), 'lightblue'),
        backgroundSize = '90% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Enhanced interactive map
  output$network_map <- renderLeaflet({
    req(rv$optimization_results, rv$cdst_labs)
    
    # Prepare data for mapping
    plot_data <- rv$optimization_results %>%
      filter(current_assignment != optimized_assignment) %>%
      left_join(rv$cdst_labs %>% select(lab_name, lat, lon), 
                by = c("optimized_assignment" = "lab_name"), 
                suffix = c("_district", "_lab"))
    
    # Create base map
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = mean(rv$district_labs$lon), lat = mean(rv$district_labs$lat), zoom = 7)
    
    # Add shapefile if available
    if (!is.null(rv$shapefile)) {
      map <- map %>% addPolygons(
        data = rv$shapefile,
        fillColor = "lightgray",
        color = "white",
        weight = 1,
        fillOpacity = 0.4,
        group = "State Boundary"
      )
    }
    
    # Add CDST labs (red markers)
    map <- map %>% addMarkers(
      data = rv$cdst_labs,
      lng = ~lon,
      lat = ~lat,
      icon = makeIcon(
        iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png",
        iconWidth = 25, iconHeight = 41
      ),
      popup = ~paste0(
        "<div style='min-width:200px'>",
        "<h4>", lab_name, "</h4>",
        "<b>Address:</b> ", address, "<br>",
        "<b>Capacity:</b> ", capacity, " tests/quarter<br>",
        "<hr>",
        "<b>Current Load:</b> ", rv$lab_utilization$current_load[match(lab_name, rv$lab_utilization$lab_name)], "<br>",
        "<b>Current Utilization:</b> ", round(rv$lab_utilization$current_utilization[match(lab_name, rv$lab_utilization$lab_name)], 1), "%<br>",
        "<b>Optimized Load:</b> ", rv$lab_utilization$optimized_load[match(lab_name, rv$lab_utilization$lab_name)], "<br>",
        "<b>Optimized Utilization:</b> ", round(rv$lab_utilization$optimized_utilization[match(lab_name, rv$lab_utilization$lab_name)], 1), "%",
        "</div>"
      ),
      group = "CDST Labs"
    )
    
    # Add district headquarters (blue circles)
    map <- map %>% addCircleMarkers(
      data = rv$district_labs,
      lng = ~lon,
      lat = ~lat,
      radius = 6,
      color = "blue",
      fillColor = "lightblue",
      fillOpacity = 0.7,
      weight = 2,
      popup = ~paste0(
        "<div style='min-width:200px'>",
        "<h4>", district, "</h4>",
        "<b>Tests per Quarter:</b> ", tests_per_quarter, "<br>",
        "<hr>",
        "<b>Current CDST:</b> ", current_cdst, "<br>",
        "<b>Current Travel Time:</b> ", round(rv$optimization_results$current_travel_time[match(district, rv$optimization_results$district)], 1), " min<br>",
        "<b>Optimized CDST:</b> ", rv$optimization_results$optimized_assignment[match(district, rv$optimization_results$district)], "<br>",
        "<b>Optimized Travel Time:</b> ", round(rv$optimization_results$optimized_travel_time[match(district, rv$optimization_results$district)], 1), " min<br>",
        "<b>Time Saved:</b> ", round(rv$optimization_results$improvement_minutes[match(district, rv$optimization_results$district)], 1), " min",
        "</div>"
      ),
      group = "District HQs"
    )
    
    # Add connection lines for reassigned districts
    if (nrow(plot_data) > 0) {
      for (i in 1:nrow(plot_data)) {
        map <- map %>% addPolylines(
          lng = c(plot_data$district_lon[i], plot_data$lon[i]),
          lat = c(plot_data$district_lat[i], plot_data$lat[i]),
          color = "green",
          weight = 3,
          opacity = 0.8,
          popup = paste0(
            "<b>New Assignment</b><br>",
            plot_data$district[i], " → ", plot_data$optimized_assignment[i], "<br>",
            "Time saved: ", round(plot_data$improvement_minutes[i], 1), " minutes"
          ),
          group = "New Assignments"
        )
      }
    }
    
    # Add layer control
    map <- map %>% addLayersControl(
      overlayGroups = c("State Boundary", "CDST Labs", "District HQs", "New Assignments"),
      options = layersControlOptions(collapsed = FALSE)
    )
    
    # Add legend
    map <- map %>% addLegend(
      position = "bottomright",
      colors = c("red", "lightblue", "green"),
      labels = c("CDST Labs", "District HQs", "New Assignments"),
      opacity = 1,
      title = "Map Legend"
    )
    
    # Add optimization summary
    map <- map %>% addControl(
      position = "topright",
      html = paste0(
        "<div style='background: white; padding: 10px; border: 2px solid #333; border-radius: 5px; font-family: Arial;'>",
        "<h4 style='margin: 0 0 10px 0; color: #333;'>Optimization Summary</h4>",
        "<b>Method:</b> ", rv$optimization_method, "<br>",
        "<b>Distance:</b> ", rv$distance_method, "<br>",
        "<b>Reassigned:</b> ", sum(rv$optimization_results$current_assignment != rv$optimization_results$optimized_assignment), " districts<br>",
        "<b>Total Time Saved:</b> ", round(sum(rv$optimization_results$improvement_minutes), 1), " min/quarter",
        "</div>"
      )
    )
    
    map
  })
  
  # Enhanced isochrone map
  output$isochrone_map <- renderLeaflet({
    req(rv$cdst_labs)
    
    # Create base map
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = mean(rv$cdst_labs$lon), lat = mean(rv$cdst_labs$lat), zoom = 8)
    
    # Add shapefile if available
    if (!is.null(rv$shapefile)) {
      map <- map %>% addPolygons(
        data = rv$shapefile,
        fillColor = "lightgray",
        color = "white",
        weight = 1,
        fillOpacity = 0.3,
        group = "State Boundary"
      )
    }
    
    # Add all CDST labs
    map <- map %>% addMarkers(
      data = rv$cdst_labs,
      lng = ~lon,
      lat = ~lat,
      popup = ~paste0("<b>", lab_name, "</b><br>", address),
      group = "CDST Labs"
    )
    
    # Add district headquarters if available
    if (!is.null(rv$district_labs)) {
      map <- map %>% addCircleMarkers(
        data = rv$district_labs,
        lng = ~lon,
        lat = ~lat,
        radius = 4,
        color = "blue",
        fillOpacity = 0.7,
        popup = ~paste0("<b>", district, "</b><br>Tests/Quarter: ", tests_per_quarter),
        group = "District HQs"
      )
    }
    
    # Add isochrone if generated
    if (!is.null(rv$current_isochrone)) {
      tryCatch({
        map <- map %>% addPolygons(
          data = rv$current_isochrone,
          fillColor = "red",
          color = "red",
          weight = 2,
          fillOpacity = 0.3,
          popup = paste("Travel time:", input$isochrone_time, "minutes"),
          group = "Isochrone"
        )
        
        # Highlight selected lab
        if (!is.null(input$isochrone_lab)) {
          selected_lab <- rv$cdst_labs[rv$cdst_labs$lab_name == input$isochrone_lab, ]
          map <- map %>% addMarkers(
            data = selected_lab,
            lng = ~lon,
            lat = ~lat,
            icon = makeIcon(
              iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png",
              iconWidth = 25, iconHeight = 41
            ),
            popup = ~paste0("<b>SELECTED LAB:</b><br>", lab_name, "<br>", address),
            group = "Selected Lab"
          )
        }
      }, error = function(e) {
        # If there's an error rendering the isochrone, show a message
        print(paste("Error rendering isochrone:", e$message))
      })
    }
    
    # Add layer control
    map <- map %>% addLayersControl(
      overlayGroups = c("State Boundary", "CDST Labs", "District HQs", "Selected Lab", "Isochrone"),
      options = layersControlOptions(collapsed = FALSE)
    )
    
    map
  })
  
  # Enhanced districts within isochrone table
  output$isochrone_districts <- renderDT({
    req(rv$current_isochrone, rv$district_labs)
    
    tryCatch({
      # Convert districts to sf points
      districts_sf <- st_as_sf(rv$district_labs, 
                               coords = c("lon", "lat"), 
                               crs = 4326)
      
      # Ensure both datasets have the same CRS
      districts_sf <- st_transform(districts_sf, st_crs(rv$current_isochrone))
      
      # Find districts within isochrone using st_intersects (more robust than st_within)
      intersects_result <- st_intersects(districts_sf, rv$current_isochrone)
      within_iso <- lengths(intersects_result) > 0  # Convert to logical vector
      districts_in_iso <- rv$district_labs[within_iso, ]
      
      if (nrow(districts_in_iso) > 0) {
        # Create the main data with simple column names first
        districts_summary <- districts_in_iso %>%
          select(
            District = district,
            Current_CDST = current_cdst,
            Tests_per_Quarter = tests_per_quarter
          )
        
        # Create total row with matching structure
        total_row <- data.frame(
          District = "TOTAL",
          Current_CDST = "",
          Tests_per_Quarter = sum(districts_in_iso$tests_per_quarter)
        )
        
        # Combine the data
        final_data <- rbind(districts_summary, total_row)
        
        # Rename columns for display (after rbind to avoid name conflicts)
        names(final_data) <- c("District", "Current CDST", "Tests per Quarter")
        
        datatable(
          final_data,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            dom = 'frtip'
          ),
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = "caption-side: top; text-align: center; font-size: 16px; font-weight: bold;",
            paste0("Districts within ", input$isochrone_time, " minutes of ", input$isochrone_lab, 
                   " (", nrow(districts_in_iso), " districts, ", sum(districts_in_iso$tests_per_quarter), " total tests)")
          )
        ) %>%
          formatStyle(
            'District',
            target = 'row',
            backgroundColor = styleEqual('TOTAL', '#f0f0f0'),
            fontWeight = styleEqual('TOTAL', 'bold')
          )
      } else {
        datatable(
          data.frame(Message = paste("No districts found within", input$isochrone_time, "minutes of", input$isochrone_lab)),
          options = list(dom = 't'),
          rownames = FALSE
        )
      }
    }, error = function(e) {
      # Enhanced error handling with more details
      error_msg <- paste("Error in spatial analysis:", e$message)
      log_message(error_msg, type = "ERROR")
      
      datatable(
        data.frame(Error = error_msg),
        options = list(dom = 't'),
        rownames = FALSE
      )
    })
  })
  
  # Download handlers
  output$download_utilization <- downloadHandler(
    filename = function() {
      paste0("lab_utilization_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 14, height = 10, dpi = 300)
    }
  )
  
  output$download_improvement <- downloadHandler(
    filename = function() {
      paste0("travel_time_improvements_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 14, height = 10, dpi = 300)
    }
  )
  
  # NEW: Download handler for histograms
  output$download_histograms <- downloadHandler(
    filename = function() {
      paste0("travel_time_histograms_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 16, height = 8, dpi = 300)
    }
  )
  
  output$download_assignments <- downloadHandler(
    filename = function() {
      paste0("optimized_assignments_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      if (!is.null(rv$optimization_results)) {
        # Add additional metadata to the export
        export_data <- rv$optimization_results %>%
          mutate(
            optimization_method = rv$optimization_method,
            distance_method = rv$distance_method,
            optimization_date = Sys.Date()
          )
        write.csv(export_data, file, row.names = FALSE)
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)