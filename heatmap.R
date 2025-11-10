### CINCINNATI ENQUIRER
### Cincinnati Police Department
### Skydio Drone Flight Paths Heatmap
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

## Create heatmap

# Sample points along flight paths for heatmap density
sample_flight_points <- function(flights_sf, points_per_flight = 50) {
  # Filter to LINESTRING and MULTILINESTRING geometries and valid flights
  flights_lines <- flights_sf %>%
    filter(st_geometry_type(.) %in% c("LINESTRING", "MULTILINESTRING")) %>%
    filter(!st_is_empty(.))
  
  message("Processing ", nrow(flights_lines), " line flights out of ", nrow(flights_sf), " total")
  
  if (nrow(flights_lines) == 0) {
    warning("No valid line geometries found")
    return(data.frame(flight_id = integer(), takeoff = as.POSIXct(character()), lng = numeric(), lat = numeric()))
  }
  
  # Ensure we're in a projected CRS for accurate sampling
  flights_proj <- st_transform(flights_lines, 3857)  # Web Mercator
  
  all_points <- list()
  
  for (i in seq_len(nrow(flights_proj))) {
    flight <- flights_proj[i, ]
    
    tryCatch({
      # Get geometry and cast to LINESTRING if needed
      line_geom <- st_geometry(flight)[[1]]
      
      # If MULTILINESTRING, cast to LINESTRING (combines all parts)
      if (inherits(line_geom, "MULTILINESTRING")) {
        line_geom <- st_cast(line_geom, "LINESTRING")
      }
      
      # Sample points along the line
      sampled_points <- st_line_sample(line_geom, n = points_per_flight)
      points_sf <- st_cast(st_sfc(sampled_points, crs = st_crs(flights_proj)), "POINT")
      
      # Convert back to WGS84 for leaflet
      points_wgs84 <- st_transform(st_sf(geometry = points_sf), 4326)
      coords <- st_coordinates(points_wgs84)
      
      all_points[[i]] <- data.frame(
        flight_id = flight$object_id,
        takeoff = flight$takeoff,
        lng = coords[, 1],
        lat = coords[, 2]
      )
    }, error = function(e) {
      message("Skipping flight ", flight$object_id, ": ", e$message)
    })
  }
  
  # Remove NULL entries and combine
  all_points <- all_points[!sapply(all_points, is.null)]
  
  if (length(all_points) == 0) {
    warning("No points generated from flights")
    return(data.frame(flight_id = integer(), takeoff = as.POSIXct(character()), lng = numeric(), lat = numeric()))
  }
  
  return(bind_rows(all_points))
}

# Generate heatmap points
heatmap_points <- sample_flight_points(flights_sf, points_per_flight = 25)

## SHINY APP WITH HEATMAP

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Cincinnati Police Drone Flight Heatmap"),
  
  dashboardSidebar(
    h4("Heatmap Controls"),
    sliderInput("intensity", "Intensity:", 
                min = 0.1, max = 5, value = 0.1, step = 0.1),
    sliderInput("blur", "Blur:", 
                min = 5, max = 50, value = 15, step = 1),
    sliderInput("max_val", "Max Intensity:", 
                min = 0.01, max = 0.2, value = 0.05, step = 0.01),
    sliderInput("radius", "Radius:", 
                min = 5, max = 40, value = 10, step = 1),
    hr(),
    h5("Flight Stats:"),
    textOutput("flight_stats"),
    br(),
    actionButton("reset", "Reset to Defaults", 
                 class = "btn-primary")
  ),
  
  dashboardBody(
    leafletOutput("heatmap", height = "800px")
  )
)

# Define server
server <- function(input, output, session) {
  
  # Flight statistics
  output$flight_stats <- renderText({
    paste0(
      nrow(heatmap_points), " points from ",
      length(unique(heatmap_points$flight_id)), " flights"
    )
  })
  
  # Create reactive heatmap
  output$heatmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -84.5, lat = 39.1, zoom = 11)
  })
  
  # Update heatmap when sliders change
  observe({
    leafletProxy("heatmap") %>%
      clearHeatmap() %>%
      addHeatmap(
        data = heatmap_points,
        lng = ~lng,
        lat = ~lat,
        intensity = input$intensity,
        blur = input$blur,
        max = input$max_val,
        radius = input$radius
      )
  })
  
  # Reset button functionality
  observeEvent(input$reset, {
    updateSliderInput(session, "intensity", value = 0.1)
    updateSliderInput(session, "blur", value = 15)
    updateSliderInput(session, "max_val", value = 0.05)
    updateSliderInput(session, "radius", value = 10)
  })
}

# Run the app (only if heatmap_points exists and has data)
if (exists("heatmap_points") && nrow(heatmap_points) > 0) {
  shinyApp(ui = ui, server = server)
} else {
  message("No heatmap points available - run the sampling code first")
}