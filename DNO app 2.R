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

ui <- dashboardPage(
  dashboardHeader(title = "Diagnostic Network Optimization"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "data_input", icon = icon("upload")),
      menuItem("Optimization Results", tabName = "results", icon = icon("chart-bar")),
      menuItem("Interactive Map", tabName = "map", icon = icon("map")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_input",
              h2("Upload Your Network Data"),
              fluidRow(
                box(width = 12,
                    p("Download the template files, fill in your data, and upload them below:"),
                    downloadButton("download_district_template", "Download District Lab Template"),
                    downloadButton("download_cdst_template", "Download CDST Lab Template"),
                    hr(),
                    fileInput("district_file", "Upload District Lab Data (CSV)", accept = c(".csv")),
                    fileInput("cdst_file", "Upload CDST Lab Data (CSV)", accept = c(".csv")),
                    fileInput("shapefile", "Upload State Map (GeoJSON/Shapefile)",
                              accept = c(".geojson", ".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"),
                              multiple = TRUE),
                    actionButton("run_optimization", "Run Optimization", 
                                 icon = icon("play"), class = "btn-success")
                )
              )
      ),
      
      tabItem(tabName = "results",
              h2("Optimization Results"),
              fluidRow(
                box(width = 12, title = "Summary Statistics",
                    tableOutput("summary_table") %>% withSpinner())
              ),
              fluidRow(
                box(width = 6, title = "Lab Utilization Comparison",
                    plotOutput("utilization_plot") %>% withSpinner(),
                    downloadButton("download_utilization", "Download Plot")),
                box(width = 6, title = "Top Travel Time Improvements",
                    plotOutput("improvement_plot") %>% withSpinner(),
                    downloadButton("download_improvement", "Download Plot"))
              ),
              fluidRow(
                box(width = 12, title = "Detailed Assignments",
                    DTOutput("assignments_table") %>% withSpinner(),
                    downloadButton("download_assignments", "Download Data"))
              )
      ),
      
      tabItem(tabName = "map",
              h2("Network Optimization Map"),
              fluidRow(
                box(width = 12,
                    leafletOutput("network_map", height = "700px") %>% withSpinner())
              )
      ),
      
      tabItem(tabName = "about",
              h2("About This Tool"),
              box(width = 12,
                  includeMarkdown("about.md"))
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Set your OpenRouteService API key here
  ORS_API_KEY <- "5b3ce3597851110001cf62481a51a0c1d253455eb67aa25df367bd8d"  # Replace with your actual API key
  
  # Function to get travel time from OpenRouteService
  get_travel_time <- function(start_coords, end_coords, api_key) {
    tryCatch({
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
          encode = "json"
        )
        
        if (status_code(response) == 200) {
          result <- content(response, "parsed")
          # Return travel time in minutes
          return(result$routes[[1]]$summary$duration / 60)
        } else {
          stop("API request failed")
        }
    }, error = function(e) {
      stop("API call failed")
    })
  }
  
  # Download templates
  output$download_district_template <- downloadHandler(
    filename = function() {
      "district_lab_template.csv"
    },
    content = function(file) {
      template <- data.frame(
        district = "District Name",
        current_cdst = "Assigned CDST Lab",
        lat = 0.0,
        lon = 0.0,
        tests_per_quarter = 0,
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
        lab_name = "Lab Name",
        address = "Lab Address",
        lat = 0.0,
        lon = 0.0,
        capacity = 0,
        stringsAsFactors = FALSE
      )
      write.csv(template, file, row.names = FALSE)
    }
  )
  
  # Reactive values for storing data
  rv <- reactiveValues(
    district_labs = NULL,
    cdst_labs = NULL,
    shapefile = NULL,
    optimization_results = NULL,
    lab_utilization = NULL,
    travel_time_matrix = NULL,
    optimization_method = "Not run yet"
  )
  
  # Read uploaded files
  observeEvent(input$district_file, {
    req(input$district_file)
    rv$district_labs <- read_csv(input$district_file$datapath)
    names(rv$district_labs) <- c("district", "current_cdst", "lat", "lon", "tests_per_quarter")
  })
  
  observeEvent(input$cdst_file, {
    req(input$cdst_file)
    rv$cdst_labs <- read_csv(input$cdst_file$datapath)
    names(rv$cdst_labs) <- c("lab_name", "address", "lat", "lon", "capacity")
  })
  
  observeEvent(input$shapefile, {
    req(input$shapefile)
    
    # Handle shapefile upload (multiple files)
    if (length(input$shapefile$datapath) > 1) {
      # Find the .shp file
      shp_file <- input$shapefile$datapath[grep("\\.shp$", input$shapefile$name)]
      if (length(shp_file) == 0) {
        showNotification("No .shp file found in upload", type = "error")
        return()
      }
      
      # Read shapefile
      rv$shapefile <- st_read(shp_file, quiet = TRUE)
    } else {
      # Handle single file upload (GeoJSON)
      if (grepl("\\.geojson$", input$shapefile$name, ignore.case = TRUE)) {
        rv$shapefile <- st_read(input$shapefile$datapath, quiet = TRUE)
      } else {
        showNotification("Please upload a valid GeoJSON or Shapefile", type = "error")
      }
    }
  })
  
  # Linear Programming Optimization function
  optimize_lab_assignment_lp <- function(travel_times, capacities, demands) {
    n_districts <- nrow(travel_times)
    n_labs <- ncol(travel_times)
    
    # Decision variables: x_ij = 1 if district i assigned to lab j, 0 otherwise
    n_vars <- n_districts * n_labs
    
    # Objective function coefficients (minimize total weighted travel time)
    objective_coeffs <- as.vector(travel_times * matrix(demands, nrow = n_districts, ncol = n_labs))
    
    # Constraint matrix setup
    n_constraints <- n_districts + n_labs
    constraint_matrix <- matrix(0, nrow = n_constraints, ncol = n_vars)
    constraint_directions <- character(n_constraints)
    constraint_rhs <- numeric(n_constraints)
    
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
    
    # Constraint 2: Lab capacity limits
    for (j in 1:n_labs) {
      constraint_row <- n_districts + j
      for (i in 1:n_districts) {
        var_index <- (i - 1) * n_labs + j
        constraint_matrix[constraint_row, var_index] <- demands[i]
      }
      constraint_directions[constraint_row] <- "<="
      constraint_rhs[constraint_row] <- capacities[j]
    }
    
    # Check if problem is feasible
    total_demand <- sum(demands)
    total_capacity <- sum(capacities)
    
    if (total_demand > total_capacity) {
      # Allow up to 105% capacity utilization
      for (j in 1:n_labs) {
        constraint_rhs[n_districts + j] <- capacities[j] * 1.05
      }
    }
    
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
    
    if (lp_result$status == 2) {
      # Relax capacity constraints further if infeasible
      for (j in 1:n_labs) {
        constraint_rhs[n_districts + j] <- capacities[j] * 1.2
      }
      
      lp_result <- lp(
        direction = "min",
        objective.in = objective_coeffs,
        const.mat = constraint_matrix,
        const.dir = constraint_directions,
        const.rhs = constraint_rhs,
        all.bin = TRUE
      )
    }
    
    # Extract solution
    solution_vector <- lp_result$solution
    assignment_matrix <- matrix(solution_vector, nrow = n_districts, ncol = n_labs, byrow = TRUE)
    lab_loads <- colSums(assignment_matrix * matrix(demands, nrow = n_districts, ncol = n_labs))
    
    return(list(
      assignment = assignment_matrix,
      lab_loads = lab_loads,
      objective_value = lp_result$objval,
      status = lp_result$status
    ))
  }
  
  # Greedy heuristic using Euclidean distances
  optimize_lab_assignment_greedy <- function(districts, labs) {
    # Calculate all pairwise distances
    distances <- matrix(0, nrow = nrow(districts), ncol = nrow(labs))
    
    for (i in 1:nrow(districts)) {
      for (j in 1:nrow(labs)) {
        # Euclidean distance approximation (in km)
        distances[i, j] <- sqrt((districts$lat[i] - labs$lat[j])^2 + 
                                  (districts$lon[i] - labs$lon[j])^2) * 111
      }
    }
    
    # Initialize assignments and remaining capacities
    assignments <- rep(NA, nrow(districts))
    remaining_capacity <- labs$capacity
    
    # Assign districts to labs in order of demand (highest first)
    demand_order <- order(districts$tests_per_quarter, decreasing = TRUE)
    
    for (i in demand_order) {
      # Find available labs sorted by distance
      lab_order <- order(distances[i, ])
      
      for (j in lab_order) {
        if (remaining_capacity[j] >= districts$tests_per_quarter[i]) {
          assignments[i] <- j
          remaining_capacity[j] <- remaining_capacity[j] - districts$tests_per_quarter[i]
          break
        }
      }
    }
    
    # Create assignment matrix
    assignment_matrix <- matrix(0, nrow = nrow(districts), ncol = nrow(labs))
    for (i in 1:nrow(districts)) {
      if (!is.na(assignments[i])) {
        assignment_matrix[i, assignments[i]] <- 1
      }
    }
    
    # Calculate lab loads
    lab_loads <- colSums(assignment_matrix * matrix(districts$tests_per_quarter, 
                                                    nrow = nrow(districts), 
                                                    ncol = nrow(labs)))
    
    # Calculate objective value (total weighted distance)
    objective_value <- sum(assignment_matrix * distances * 
                             matrix(districts$tests_per_quarter, 
                                    nrow = nrow(districts), 
                                    ncol = nrow(labs)))
    
    return(list(
      assignment = assignment_matrix,
      lab_loads = lab_loads,
      objective_value = objective_value,
      status = ifelse(any(is.na(assignments)), 2, 0)  # 0 = success, 2 = infeasible
    ))
  }
  
  # Run optimization when button is clicked
  observeEvent(input$run_optimization, {
    req(rv$district_labs, rv$cdst_labs)
    
    showNotification("Starting optimization...", type = "message")
    
    # Create progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Calculating travel times", value = 0)
    
    # Try API first
    api_success <- TRUE
    n_districts <- nrow(rv$district_labs)
    n_labs <- nrow(rv$cdst_labs)
    
    travel_time_matrix <- matrix(0, nrow = n_districts, ncol = n_labs)
    rownames(travel_time_matrix) <- rv$district_labs$district
    colnames(travel_time_matrix) <- rv$cdst_labs$lab_name
    
    # Calculate travel times (with progress updates)
    for (i in 1:n_districts) {
      for (j in 1:n_labs) {
        start_coords <- c(rv$district_labs$lat[i], rv$district_labs$lon[i])
        end_coords <- c(rv$cdst_labs$lat[j], rv$cdst_labs$lon[j])
        
        # Try API first
        tt <- tryCatch({
          get_travel_time(start_coords, end_coords, ORS_API_KEY)
        }, error = function(e) {
          api_success <<- FALSE
          # Fallback to Euclidean
          distance_km <- sqrt(sum((start_coords - end_coords)^2)) * 111
          distance_km * 2  # Assume 30 km/h average speed
        })
        
        travel_time_matrix[i, j] <- tt
        
        # Add small delay to respect API rate limits
        if (api_success) Sys.sleep(0.1)
      }
      
      # Update progress
      progress$set(value = i/n_districts, 
                   detail = paste("Processed", i, "of", n_districts, "districts"))
    }
    
    # Set optimization method
    if (!api_success) {
      showNotification("API failed - using Euclidean distance fallback", type = "warning")
      rv$optimization_method <- "Greedy Heuristic (Euclidean Distances)"
      
      # Run greedy heuristic
      optimization_result <- optimize_lab_assignment_greedy(rv$district_labs, rv$cdst_labs)
    } else {
      rv$optimization_method <- "Linear Programming (API Travel Times)"
      
      # Run LP optimization
      optimization_result <- optimize_lab_assignment_lp(
        travel_time_matrix, 
        rv$cdst_labs$capacity, 
        rv$district_labs$tests_per_quarter
      )
    }
    
    rv$travel_time_matrix <- travel_time_matrix
    
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
      stringsAsFactors = FALSE
    )
    
    # Fill in optimized assignments
    for (i in 1:nrow(rv$district_labs)) {
      assigned_lab_idx <- which(optimization_result$assignment[i, ] == 1)
      if (length(assigned_lab_idx) > 0) {
        results$optimized_assignment[i] <- rv$cdst_labs$lab_name[assigned_lab_idx]
        results$optimized_travel_time[i] <- travel_time_matrix[i, assigned_lab_idx]
      }
      
      # Calculate current travel time
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
    
    progress$close()
    showNotification("Optimization completed!", type = "message")
  })
  
  # Summary table
  output$summary_table <- renderTable({
    req(rv$optimization_results, rv$lab_utilization)
    
    data.frame(
      Metric = c(
        "Optimization Method",
        "Total Districts",
        "Districts Reassigned",
        "Average Travel Time Reduction (min)",
        "Total Quarterly Time Saved (hours)",
        "Most Utilized Lab (Current)",
        "Most Utilized Lab (Optimized)",
        "Least Utilized Lab (Current)",
        "Least Utilized Lab (Optimized)"
      ),
      Value = c(
        rv$optimization_method,
        nrow(rv$district_labs),
        sum(rv$optimization_results$current_assignment != rv$optimization_results$optimized_assignment),
        round(mean(rv$optimization_results$improvement_minutes), 2),
        round(sum(rv$optimization_results$improvement_minutes) / 60, 2),
        rv$lab_utilization$lab_name[which.max(rv$lab_utilization$current_utilization)],
        rv$lab_utilization$lab_name[which.max(rv$lab_utilization$optimized_utilization)],
        rv$lab_utilization$lab_name[which.min(rv$lab_utilization$current_utilization)],
        rv$lab_utilization$lab_name[which.min(rv$lab_utilization$optimized_utilization)]
      )
    )
  })
  
  # Utilization plot
  output$utilization_plot <- renderPlot({
    req(rv$lab_utilization)
    
    rv$lab_utilization %>%
      select(lab_name, current_utilization, optimized_utilization) %>%
      pivot_longer(cols = c(current_utilization, optimized_utilization),
                   names_to = "scenario", values_to = "utilization") %>%
      mutate(scenario = ifelse(scenario == "current_utilization", "Current", "Optimized")) %>%
      
      ggplot(aes(x = reorder(lab_name, utilization), y = utilization, fill = scenario)) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 100, linetype = "dashed", color = "red", alpha = 0.7) +
      scale_fill_viridis_d(name = "Scenario") +
      labs(title = "Lab Utilization Comparison",
           subtitle = paste("Current vs Optimized Assignment | Method:", rv$optimization_method),
           x = "CDST Labs", y = "Utilization (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 10)) +
      coord_flip()
  })
  
  # Improvement plot
  output$improvement_plot <- renderPlot({
    req(rv$optimization_results)
    
    rv$optimization_results %>%
      filter(improvement_minutes > 0) %>%
      arrange(desc(improvement_minutes)) %>%
      slice_head(n = 20) %>%
      
      ggplot(aes(x = reorder(district, improvement_minutes), y = improvement_minutes)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      geom_text(aes(label = paste0(round(improvement_minutes, 1), "m")), 
                hjust = -0.1, size = 3) +
      labs(title = "Top 20 Districts with Travel Time Improvements",
           subtitle = paste("Minutes saved per trip | Method:", rv$optimization_method),
           x = "District", y = "Time Saved (minutes)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 10)) +
      coord_flip()
  })
  
  # Assignments table
  output$assignments_table <- renderDT({
    req(rv$optimization_results)
    
    datatable(
      rv$optimization_results,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = c('current_travel_time', 'optimized_travel_time', 'improvement_minutes'), digits = 1)
  })
  
  # Interactive map
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
        fillOpacity = 0.5
      )
    }
    
    # Add CDST labs (triangles)
    map <- map %>% addMarkers(
      data = rv$cdst_labs,
      lng = ~lon,
      lat = ~lat,
      icon = makeIcon(
        iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png",
        iconWidth = 25, iconHeight = 41
      ),
      popup = ~paste("<b>", lab_name, "</b><br>",
                     "Capacity:", capacity, "tests/quarter<br>",
                     "Current Utilization:", round(rv$lab_utilization$current_utilization[match(lab_name, rv$lab_utilization$lab_name)], 1), "%<br>",
                     "Optimized Utilization:", round(rv$lab_utilization$optimized_utilization[match(lab_name, rv$lab_utilization$lab_name)], 1), "%")
    )
    
    # Add district headquarters (circles)
    map <- map %>% addCircleMarkers(
      data = rv$district_labs,
      lng = ~lon,
      lat = ~lat,
      radius = 5,
      color = "blue",
      fillOpacity = 0.7,
      popup = ~paste("<b>", district, "</b><br>",
                     "Current CDST:", current_cdst, "<br>",
                     "Optimized CDST:", rv$optimization_results$optimized_assignment[match(district, rv$optimization_results$district)], "<br>",
                     "Tests/Quarter:", tests_per_quarter)
    )
    
    # Add connection lines for reassigned districts
    for (i in 1:nrow(plot_data)) {
      map <- map %>% addPolylines(
        lng = c(plot_data$district_lon[i], plot_data$lon[i]),
        lat = c(plot_data$district_lat[i], plot_data$lat[i]),
        color = "green",
        weight = 2,
        opacity = 0.7
      )
    }
    
    # Add legend
    map <- map %>% addLegend(
      position = "bottomright",
      colors = c("red", "blue", "green"),
      labels = c("CDST Labs", "District HQs", "New Assignments"),
      opacity = 1
    )
    
    # Add method indicator
    map <- map %>% addControl(
      position = "topright",
      html = paste0("<div style='background: white; padding: 5px; border: 1px solid gray;'>",
                    "<strong>Optimization Method:</strong><br>",
                    rv$optimization_method,
                    "</div>")
    )
    
    map
  })
  
  # Download handlers
  output$download_utilization <- downloadHandler(
    filename = function() {
      "lab_utilization_comparison.png"
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 10, height = 8, dpi = 300)
    }
  )
  
  output$download_improvement <- downloadHandler(
    filename = function() {
      "travel_time_improvements.png"
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 12, height = 6, dpi = 300)
    }
  )
  
  output$download_assignments <- downloadHandler(
    filename = function() {
      "optimized_assignments.csv"
    },
    content = function(file) {
      write.csv(rv$optimization_results, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)