### CINCINNATI ENQUIRER
### Cincinnati Police Department
### Skydio Drone Flight Paths Animated Map
### by David Ferrara

## INITIALIZE

library(sf)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(janitor)
library(lubridate)
library(shiny)
library(shinydashboard)
library(plotly)

## Flight data

flights_sf <- st_read("./data/flight_paths.geojson")

# Convert takeoff and landing to POSIXct
flights_sf$takeoff <- as.POSIXct(flights_sf$takeoff / 1000, origin = "1970-01-01", tz = "America/New_York")
flights_sf$landing <- as.POSIXct(flights_sf$landing / 1000, origin = "1970-01-01", tz = "America/New_York")

flights_sf <- flights_sf %>% 
  clean_names() %>%
  select(
    -vehicle_serial,
    -dock_serial,
    -user_email,
    -external_id,
    -description,
    -operation_id,
    -organization_id
  )

## Animated map

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Cincinnati Police Drone Flights"),
  
  dashboardSidebar(
    h4("Animation Controls"),
    dateRangeInput("date_range", "Date Range:",
                   start = min(as.Date(flights_sf$takeoff)), 
                   end = max(as.Date(flights_sf$takeoff)),
                   min = min(as.Date(flights_sf$takeoff)),
                   max = max(as.Date(flights_sf$takeoff))),
    
    sliderInput("animation_speed", "Speed (days per update):", 
                min = 1, max = 14, value = 1, step = 1),
    
    sliderInput("trail_length", "Trail Length (days):", 
                min = 0, max = 7, value = 1, step = 1),
    
    hr(),
    
    actionButton("play_btn", "Play", class = "btn-success", style = "margin-right: 5px;"),
    actionButton("pause_btn", "Pause", class = "btn-warning", style = "margin-right: 5px;"),
    actionButton("reset_btn", "Reset", class = "btn-info"),
    
    br(), br(),
    
    checkboxInput("show_takeoff_markers", "Show Takeoff Points", FALSE),
    checkboxInput("show_flight_info", "Show Flight Info", TRUE),
    checkboxInput("show_all_flights", "Show All Flights at Once", FALSE),
    
    hr(),
    
    h5("Current Status:"),
    textOutput("current_date"),
    textOutput("flights_shown"),
    textOutput("total_flights")
  ),
  
  dashboardBody(
    fluidRow(
      box(
        title = "Flight Animation Map", 
        status = "primary", 
        solidHeader = TRUE,
        width = 12, 
        height = "600px",
        leafletOutput("animated_map", height = "550px")
      )
    ),
    
    fluidRow(
      box(
        title = "Daily Flight Activity (Click bars to jump to date)", 
        status = "info", 
        solidHeader = TRUE,
        width = 12,
        plotlyOutput("daily_activity", height = "200px")
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive values for animation
  values <- reactiveValues(
    animation_running = FALSE,
    current_date = NULL
  )
  
  # Initialize current date when date range changes
  observeEvent(input$date_range, {
    values$current_date <- input$date_range[1]
    values$animation_running <- FALSE  # Stop animation when date range changes
  })
  
  # Play button
  observeEvent(input$play_btn, {
    if (is.null(values$current_date)) {
      values$current_date <- input$date_range[1]
    }
    if (values$current_date >= input$date_range[2]) {
      values$current_date <- input$date_range[1]  # Reset if at end
    }
    values$animation_running <- TRUE
  })
  
  # Pause button
  observeEvent(input$pause_btn, {
    values$animation_running <- FALSE
  })
  
  # Reset button
  observeEvent(input$reset_btn, {
    values$animation_running <- FALSE
    values$current_date <- input$date_range[1]
  })
  
  # Animation timer - separate from the logic
  observe({
    if (values$animation_running) {
      invalidateLater(1500, session)  # Update every 1.5 seconds
      
      # Advance date
      isolate({
        values$current_date <- values$current_date + days(input$animation_speed)
        
        # Stop if past end date
        if (values$current_date > input$date_range[2]) {
          values$animation_running <- FALSE
        }
      })
    }
  })
  
  # Get current flights to display - this updates when current_date or trail_length changes
  current_flights <- reactive({
    if (input$show_all_flights) {
      # Show all flights in the selected date range
      all_flights_data <- flights_sf %>%
        filter(as.Date(takeoff) >= input$date_range[1],
               as.Date(takeoff) <= input$date_range[2]) %>%
        mutate(
          color = "blue",  # Single color for all flights
          opacity = 0.6,
          weight = 2
        )
      
      if (nrow(all_flights_data) > 0) {
        all_flights_data <- st_transform(all_flights_data, 4326)
      }
      
      return(all_flights_data)
    } else {
      # Original animation logic
      req(values$current_date)
      
      start_date <- values$current_date - days(input$trail_length)
      end_date <- values$current_date
      
      current_flights_data <- flights_sf %>%
        filter(as.Date(takeoff) >= start_date,
               as.Date(takeoff) <= end_date) %>%
        mutate(
          days_ago = as.numeric(values$current_date - as.Date(takeoff)),
          color = case_when(
            days_ago == 0 ~ "red",
            days_ago <= 1 ~ "orange", 
            days_ago <= 3 ~ "yellow",
            TRUE ~ "lightblue"
          ),
          opacity = pmax(0.2, 1 - (days_ago / pmax(input$trail_length, 1))),
          weight = ifelse(days_ago == 0, 4, 2)
        )
      
      if (nrow(current_flights_data) > 0) {
        current_flights_data <- st_transform(current_flights_data, 4326)
      }
      
      return(current_flights_data)
    }
  })
  
  # Initialize map
  output$animated_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -84.5, lat = 39.1, zoom = 11) %>%
      addControl(
        html = "<div style='background: white; padding: 5px; border-radius: 3px;'>
                <h4 style='margin: 0;'>Cincinnati Police Drone Flights</h4>
                <p style='margin: 0; font-size: 12px;'>Red: Today | Orange: Yesterday | Yellow: 2-3 days ago</p>
                </div>",
        position = "topright"
      )
  })
  
  # Update map with current flights - more efficient approach
  observe({
    flights_to_show <- current_flights()
    
    # Clear existing flight paths
    leafletProxy("animated_map") %>%
      clearGroup("flights") %>%
      clearGroup("markers")
    
    # Add flight paths
    if (nrow(flights_to_show) > 0) {
      # If showing all flights, limit to prevent browser hanging
      if (input$show_all_flights) {
        flights_to_show <- flights_to_show %>% slice_head(n = 200)  # Increase limit for "show all"
        message("Displaying ", nrow(flights_to_show), " flights (limited for performance)")
      } else {
        flights_to_show <- flights_to_show %>% slice_head(n = 50)   # Keep original limit for animation
      }
      
      # Add all polylines at once
      leafletProxy("animated_map") %>%
        addPolylines(
          data = flights_to_show,
          group = "flights",
          color = ~color,
          weight = ~weight,
          opacity = ~opacity,
          popup = if (input$show_flight_info) {
            ~paste0(
              "<b>Flight ID:</b> ", object_id, "<br>",
              "<b>Date:</b> ", format(takeoff, "%Y-%m-%d"), "<br>",
              "<b>Takeoff:</b> ", format(takeoff, "%H:%M"), "<br>",
              "<b>Landing:</b> ", format(landing, "%H:%M"), "<br>",
              "<b>Duration:</b> ", round(difftime(landing, takeoff, units = "mins"), 1), " mins"
            )
          } else NULL
        )
      
      # Add takeoff markers if enabled (limit more strictly for "show all")
      marker_limit <- if (input$show_all_flights) 50 else 20
      if (input$show_takeoff_markers && nrow(flights_to_show) <= marker_limit) {
        takeoff_points <- st_startpoint(flights_to_show)
        takeoff_coords <- st_coordinates(takeoff_points)
        
        leafletProxy("animated_map") %>%
          addCircleMarkers(
            lng = takeoff_coords[, 1],
            lat = takeoff_coords[, 2],
            group = "markers",
            radius = 3,  # Smaller markers for "show all"
            color = flights_to_show$color,
            fillOpacity = 0.6,
            popup = paste("Takeoff:", format(flights_to_show$takeoff, "%Y-%m-%d %H:%M"))
          )
      }
    }
  })
  
  # Add arrows along the flight path (simpler version)
  observe({
    flights_to_show <- current_flights()
    
    leafletProxy("animated_map") %>%
      clearGroup("flights") %>%
      clearGroup("markers") %>%
      clearGroup("arrows")
    
    if (nrow(flights_to_show) > 0) {
      # Limit flights for performance
      flights_to_show <- flights_to_show %>% slice_head(n = 50)
      
      # Add polylines
      leafletProxy("animated_map") %>%
        addPolylines(
          data = flights_to_show,
          group = "flights",
          color = ~color,
          weight = ~weight,
          opacity = ~opacity,
          popup = if (input$show_flight_info) {
            ~paste0(
              "<b>Flight ID:</b> ", object_id, "<br>",
              "<b>Date:</b> ", format(takeoff, "%Y-%m-%d"), "<br>",
              "<b>Takeoff:</b> ", format(takeoff, "%H:%M"), "<br>",
              "<b>Landing:</b> ", format(landing, "%H:%M")
            )
          } else NULL
        )
      
      # Add simple directional markers
      if (input$show_direction_arrows %||% FALSE) {
        for (i in 1:min(20, nrow(flights_to_show))) {  # Limit to 20 for performance
          flight <- flights_to_show[i, ]
          coords <- st_coordinates(flight)
          
          if (nrow(coords) > 1) {
            # Add arrow at end point
            end_point <- coords[nrow(coords), ]
            
            leafletProxy("animated_map") %>%
              addMarkers(
                lng = end_point[1],
                lat = end_point[2],
                group = "arrows",
                icon = list(
                  iconUrl = "data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMjAiIGhlaWdodD0iMjAiIHZpZXdCb3g9IjAgMCAyMCAyMCIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj4KPHBhdGggZD0iTTEwIDJMMTggMTBMMTAgMThMMiAxMEwxMCAyWiIgZmlsbD0iIzAwNzNlNiIgc3Ryb2tlPSJ3aGl0ZSIgc3Ryb2tlLXdpZHRoPSIxIi8+Cjwvc3ZnPgo=",
                  iconSize = c(12, 12),
                  iconAnchor = c(6, 6)
                ),
                popup = "Landing point"
              )
          }
        }
      }
    }
  })
  
  # Status outputs
  output$current_date <- renderText({
    if (!is.null(values$current_date)) {
      paste("Current Date:", format(values$current_date, "%Y-%m-%d"))
    } else {
      "Current Date: Not set"
    }
  })
  
  output$flights_shown <- renderText({
    paste("Flights Displayed:", nrow(current_flights()))
  })
  
  output$total_flights <- renderText({
    total_in_range <- flights_sf %>%
      filter(as.Date(takeoff) >= input$date_range[1],
             as.Date(takeoff) <= input$date_range[2]) %>%
      nrow()
    paste("Total in Range:", total_in_range)
  })
  
  # Create daily_counts as a reactive expression
  daily_counts <- reactive({
    req(input$date_range)
    
    flights_sf %>%
      st_drop_geometry() %>%
      filter(as.Date(takeoff) >= input$date_range[1],
             as.Date(takeoff) <= input$date_range[2]) %>%
      mutate(date = as.Date(takeoff)) %>%
      count(date, name = "n") %>%
      complete(date = seq(input$date_range[1], input$date_range[2], by = "day"), fill = list(n = 0))
  })
  
  # Daily activity chart
  output$daily_activity <- renderPlotly({
    daily_data <- daily_counts()
    
    p <- ggplot(daily_data, aes(x = date, y = n, text = paste("Date:", date, "<br>Flights:", n))) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      {if (!is.null(values$current_date)) 
        geom_vline(xintercept = as.numeric(values$current_date), 
                   color = "red", size = 1, alpha = 0.8)} +
      theme_minimal() +
      labs(x = "Date", y = "Number of Flights", 
           title = "Daily Flight Activity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>%
      event_register("plotly_click")
  })
  
  # Updated plotly click handler:
  observeEvent(event_data("plotly_click"), {
    click_data <- event_data("plotly_click")
    
    if (!is.null(click_data)) {
      # Get the daily data
      daily_data <- daily_counts()
      
      # Get clicked date - check if pointNumber is valid
      point_number <- click_data$pointNumber + 1
      if (point_number > 0 && point_number <= nrow(daily_data)) {
        clicked_date <- daily_data$date[point_number]
        
        # Update current date and stop animation
        values$current_date <- clicked_date
        values$animation_running <- FALSE
        
        # Uncheck "show all flights" to see the specific day
        updateCheckboxInput(session, "show_all_flights", value = FALSE)
        
        # Set trail length to 0 to show only that day's flights
        updateSliderInput(session, "trail_length", value = 0)
        
        # Show notification
        showNotification(
          paste("Viewing flights for", format(clicked_date, "%Y-%m-%d")),
          type = "message",
          duration = 3
        )
      }
    }
  })
  
  # Disable animation when "show all" is enabled:
  observe({
    if (input$show_all_flights) {
      values$animation_running <- FALSE
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)