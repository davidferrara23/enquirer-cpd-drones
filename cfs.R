### CINCINNATI ENQUIRER
### Cincinnati Police Department
### Skydio Drone Flight Paths and Calls for Service
### by: David Ferrara

### VARS

cfs_csv <- "./data/cfs_2025_092125.csv"  # replace with path to static CFS CSV file
flight_paths_geojson <- "./data/flight_paths.geojson"  # replace with path to flight paths GeoJSON file
county_centerlines_geojson <- "./data/Countywide_Street_Centerlines.geojson"  # replace with path to centerlines GeoJSON file
neigh_fp <- "./data/Cincinnati_Statistical_Neighborhood_Approximations_(SNA)_2020.geojson"  # replace with path to SNA GeoJSON file

### INITIALIZE

library(sf)
library(tidyverse)
library(lubridate)
library(leaflet)
library(janitor)
library(progress)

if (!dir.exists("./output")) {
  dir.create("./output")
}

### FUNCS

# Simple function to parse CFS address
parse_cfs_address <- function(address) {
  # Check if address is NA or empty
  if(is.na(address) || address == "") {
    return(list(block_num = NA, street_name = NA))
  }
  
  # Pattern for addresses like "63XX CORBLY ST"
  pattern <- "^([0-9]+)XX\\s+(.+)$"
  
  if(!grepl(pattern, address)) {
    return(list(block_num = NA, street_name = NA))
  }
  
  # Extract block number and street name
  block_num <- as.numeric(gsub(pattern, "\\1", address)) * 100
  street_name <- toupper(trimws(gsub(pattern, "\\2", address)))
  
  return(list(block_num = block_num, street_name = street_name))
}

# Simple exact-match function
match_address_to_centerline <- function(address, centerlines_df) {
  # Parse address
  parsed <- parse_cfs_address(address)
  if(is.na(parsed$block_num)) return(NULL)
  
  block_num <- parsed$block_num
  street_name <- parsed$street_name
  
  # Step 1: Find EXACT street name matches only
  matches <- centerlines_df %>%
    filter(street_full == street_name)
  
  # If no exact street match, return NULL - don't try fuzzy matching
  if(nrow(matches) == 0) return(NULL)
  
  # Step 2: Among exact street matches, look for block match or closest block
  block_matches <- matches %>%
    filter(
      (block_num >= left_block_from & block_num <= left_block_to) |
        (block_num >= right_block_from & block_num <= right_block_to)
    )
  
  # If we have block matches, use those
  if(nrow(block_matches) > 0) {
    return(block_matches[1,])
  } else {
    # Otherwise find closest block
    matches <- matches %>%
      mutate(
        distance = pmin(
          abs(block_num - left_block_from),
          abs(block_num - left_block_to),
          abs(block_num - right_block_from),
          abs(block_num - right_block_to)
        )
      ) %>%
      arrange(distance)
    return(matches[1,])
  }
}

# Separate function for matching CFS to centerlines
match_cfs_to_centerlines <- function(cfs_df, centerlines_df) {
  message("Matching CFS addresses to street centerlines...")
  
  # Run matching and store matched segment (may be NULL)
  cfs_with_streets <- cfs_df %>%
    rowwise() %>%
    mutate(
      street_segment = list(match_address_to_centerline(address_x, centerlines_df)),
      has_street_match = !is.null(street_segment)
    ) %>%
    ungroup()
  
  # Build streets SF: take original CFS attributes for matched rows and assign segment geometry safely
  matched_idx <- which(cfs_with_streets$has_street_match)
  if (length(matched_idx) > 0) {
    cfs_streets_sf <- cfs_with_streets[matched_idx, ]
    # create an sfc of geometries extracted from each matched street_segment (safe checks)
    sfc_list <- purrr::map(cfs_with_streets$street_segment[matched_idx], function(s) {
      if (is.null(s) || !inherits(s, "sf")) return(NULL)
      g <- sf::st_geometry(s)
      if (length(g) < 1) return(NULL)
      sf::st_sfc(g[[1]], crs = sf::st_crs(s))
    })
    geom_sfc <- do.call(c, sfc_list)
    sf::st_geometry(cfs_streets_sf) <- geom_sfc
    sf::st_crs(cfs_streets_sf) <- sf::st_crs(centerlines_df)
    cfs_streets_sf <- cfs_streets_sf %>%
      select(-street_segment, -has_street_match) %>%
      mutate(match_type = "street")
  } else {
    # empty sf with same columns as cfs_df and correct CRS
    cfs_streets_sf <- cfs_with_streets[0, ] %>%
      st_as_sf(crs = st_crs(centerlines_df)) %>%
      mutate(match_type = character())
  }
  
  # For unmatched, create an SF object using the original coordinates (lon/lat -> centerline CRS)
  cfs_points_sf <- cfs_with_streets %>%
    filter(!has_street_match) %>%
    st_as_sf(coords = c("longitude_x", "latitude_x"), crs = 4326, remove = FALSE) %>%
    st_transform(st_crs(centerlines_df)) %>%
    select(-street_segment, -has_street_match) %>%
    mutate(match_type = "point")
  
  message(sprintf("Matched %d of %d calls to street segments (%.1f%%)",
                  nrow(cfs_streets_sf), nrow(cfs_df),
                  ifelse(nrow(cfs_df) == 0, 0, nrow(cfs_streets_sf)/nrow(cfs_df)*100)))
  
  # Combine (rbind is sf-aware when CRS matches)
  combined_sf <- if (nrow(cfs_streets_sf) == 0) {
    cfs_points_sf
  } else if (nrow(cfs_points_sf) == 0) {
    cfs_streets_sf
  } else {
    rbind(cfs_streets_sf, cfs_points_sf)
  }
  
  return(list(
    streets = cfs_streets_sf,
    points = cfs_points_sf,
    combined = combined_sf
  ))
}

# Clean, separate function ONLY for matching flights to CFS
match_flights_to_cfs <- function(cfs_sf, flights_sf, buffer_distance = 400, time_window_mins = 10, max_flights = NULL) {
  # CRS alignment
  if (st_crs(cfs_sf) != st_crs(flights_sf)) {
    cfs_sf <- st_transform(cfs_sf, st_crs(flights_sf))
  }
  
  # optional flight limit
  if (!is.null(max_flights) && is.numeric(max_flights) && max_flights > 0 && max_flights < nrow(flights_sf)) {
    flights_sf <- flights_sf[1:max_flights, ]
  }
  
  # drop CFS with missing required times
  cfs_sf <- cfs_sf %>% filter(!is.na(create_time_incident), !is.na(closed_time_incident))
  
  matches_list <- list(); match_count <- 0
  total_flights <- nrow(flights_sf)
  if (total_flights == 0) {
    message("No flights to process.")
    return(NULL)
  }
  
  # progress bar
  pb <- progress::progress_bar$new(
    format = " Matching flights [:bar] :percent (:current/:total) ETA: :eta",
    total = total_flights,
    clear = FALSE,
    width = 60
  )
  
  for (i in seq_len(total_flights)) {
    pb$tick()
    
    flight <- flights_sf[i, ]
    if (is.na(flight$takeoff) || is.na(flight$landing)) next
    
    # use POSIXct takeoff/landing directly
    ft <- flight$takeoff
    fl <- flight$landing
    window_start <- ft - as.difftime(time_window_mins, units = "mins")
    
    # TEMPORAL FILTER
    temporally_possible_calls <- cfs_sf %>%
      filter(
        create_time_incident <= ft,
        create_time_incident <= fl,
        create_time_incident >= window_start,
        closed_time_incident >= ft
      ) %>%
      filter(!is.na(create_time_incident), !is.na(closed_time_incident))
    
    if (nrow(temporally_possible_calls) == 0) next
    
    # spatial matching
    temporally_possible_buffers <- st_buffer(temporally_possible_calls, buffer_distance)
    intersects <- st_intersects(flight, temporally_possible_buffers, sparse = FALSE)[1, ]
    if (!any(intersects)) next
    
    intersecting_calls <- temporally_possible_calls[intersects, ]
    intersecting_buffers <- temporally_possible_buffers[intersects, ]
    
    for (j in seq_len(nrow(intersecting_calls))) {
      call <- intersecting_calls[j, ]
      call_buffer <- intersecting_buffers[j, ]
      
      call_create <- call$create_time_incident
      time_diff <- as.numeric(difftime(ft, call_create, units = "mins"))
      
      if (call_create > ft) next
      if (call$closed_time_incident < ft) next
      
      dist <- as.numeric(st_distance(flight, call))
      flight_intersection <- st_intersection(flight, call_buffer)
      hover_factor <- if (length(flight_intersection) > 0 && !st_is_empty(flight_intersection)) {
        min(as.numeric(st_length(flight_intersection)) / as.numeric(st_length(flight)), 1)
      } else 0
      
      match_type <- if (!is.null(call$match_type)) call$match_type else "point"
      match_count <- match_count + 1
      
      matches_list[[match_count]] <- data.frame(
        flight_id = flight$object_id,
        call_id = call$event_number,
        call_create_time = call$create_time_incident,
        takeoff_time = flight$takeoff,
        landing_time = flight$landing,
        call_closed_time = call$closed_time_incident,
        time_diff_mins = time_diff,
        distance_m = dist,
        hover_factor = hover_factor,
        incident_type = call$incident_type_id,
        disposition = call$disposition_text,
        address = call$address_x,
        priority = call$priority,
        beat = call$beat,
        district = call$district,
        cpd_neighborhood = call$cpd_neighborhood,
        time_to_close_mins = call$time_to_close_mins,
        time_before_arrival = call$time_before_arrival,
        time_before_dispatch = call$time_before_dispatch,
        cfs_match_type = match_type,
        match_score = (1 - pmin(abs(time_diff) / time_window_mins, 1)) * 0.4 +
          (1 - pmin(dist / buffer_distance, 1)) * 0.3 +
          hover_factor * 0.3,
        stringsAsFactors = FALSE
      )
    }
  }
  
  pb$terminate()
  
  if (length(matches_list) == 0) {
    message("No matches found.")
    return(NULL)
  }
  
  matches_df <- bind_rows(matches_list) %>%
    group_by(flight_id) %>%
    mutate(match_type = ifelse(match_score == max(match_score), "primary", "secondary")) %>%
    ungroup() %>%
    arrange(flight_id, desc(match_score))
  
  return(matches_df)
}

# plot_flight(oid, matches, flights_sf, cfs_sf)
plot_flight <- function(oid, matches_df = NULL, flights_sf_in = NULL, cfs_sf_in = NULL, expand = 0.01) {
  matches_df <- if (is.null(matches_df)) matches else matches_df
  flights_sf <- if (is.null(flights_sf_in)) flights_sf else flights_sf_in
  cfs_sf <- if (is.null(cfs_sf_in)) cfs_sf else cfs_sf_in
  
  # Filter for the selected flight
  single_flight <- matches_df %>% filter(flight_id == oid)
  if (nrow(single_flight) == 0) {
    stop(paste("No flight found for ObjectId", oid))
  }
  single_call <- single_flight %>% 
    left_join(cfs_sf %>% st_drop_geometry(), by = c("call_id" = "event_number")) %>%
    select(latitude_x, longitude_x) %>%
    distinct()
  if (nrow(single_call) == 0) {
    stop(paste("No call coordinates found for ObjectId", oid))
  }
  
  # Convert call coordinates to sf point (in lon/lat)
  call_point <- st_as_sf(single_call, coords = c("longitude_x", "latitude_x"), crs = 4326)
  # Add popup text column
  call_point$popup_text <- paste("Lat:", call_point$latitude_x, "| Lon:", call_point$longitude_x)
  
  # Transform flight to lon/lat for leaflet
  flight_geom <- flights_sf %>% filter(object_id == oid)
  single_flight_ll <- st_transform(flight_geom, 4326)
  
  # Create interactive map
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolylines(
      data = single_flight_ll,
      color = "blue",
      weight = 4,
      label = ~paste(
        "Takeoff:", format(takeoff, "%Y-%m-%d %H:%M:%S"),
        "| Landing:", format(landing, "%Y-%m-%d %H:%M:%S")
      )
    ) %>%
    addCircleMarkers(
      data = call_point,
      color = "red",
      fillColor = "yellow",
      radius = 8,
      label = ~paste(
        "Incident Type ID:", single_flight$incident_type[1], " |",
        "Disposition:", single_flight$disposition[1]
      ),
      popup = ~paste(
        popup_text, "<br>",
        "Incident Type ID:", single_flight$incident_type[1], " |",
        "Disposition:", single_flight$disposition[1], " |",
        "Address:", single_flight$address_x[1]
      )
    ) %>%
    addControl(
      html = paste0("<h3>Flight #", oid, ": ", single_flight$incident_type[1], "</h3><p>", single_flight$call_id[1], "</p>"),
      position = "topright"
    ) %>%
    setView(lng = single_call$longitude_x[1], lat = single_call$latitude_x[1], zoom = 14)
}

## Add: Plot flights by disposition type and incident type

### IMPORT AND CLEAN DATA

## Calls for service
cfs <- read_csv(
  cfs_csv,
  col_types = cols(
    ADDRESS_X = col_character(),
    AGENCY = col_character(),
    DISPOSITION_TEXT = col_character(),
    EVENT_NUMBER = col_character(),
    INCIDENT_TYPE_ID = col_character(),
    INCIDENT_TYPE_DESC = col_character(),
    PRIORITY = col_integer(),
    PRIORITY_COLOR = col_character(),
    CREATE_TIME_INCIDENT = col_datetime(format = "%Y %b %d %H:%M:%S %p"),
    ARRIVAL_TIME_PRIMARY_UNIT = col_datetime(format = "%Y %b %d %H:%M:%S %p"),
    CLOSED_TIME_INCIDENT = col_datetime(format = "%Y %b %d %H:%M:%S %p"),
    DISPATCH_TIME_PRIMARY_UNIT = col_datetime(format = "%Y %b %d %H:%M:%S %p"),
    BEAT = col_character(),
    DISTRICT = col_character(),
    SNA_NEIGHBORHOOD = col_character(),
    CPD_NEIGHBORHOOD = col_character(),
    COMMUNITY_COUNCIL_NEIGHBORHOOD = col_character(),
    LATITUDE_X = col_double(),
    LONGITUDE_X = col_double()
  )
)

cfs <- cfs %>%
  clean_names() %>%
  mutate(
    create_time_incident      = lubridate::force_tz(create_time_incident,      "America/New_York"),
    arrival_time_primary_unit = lubridate::force_tz(arrival_time_primary_unit, "America/New_York"),
    closed_time_incident      = lubridate::force_tz(closed_time_incident,      "America/New_York"),
    dispatch_time_primary_unit= lubridate::force_tz(dispatch_time_primary_unit,"America/New_York")
  )

# Load most recent CAD calls via Socrata API
# Find most recent date of create_time_incident
latest_date <- format(max(cfs$create_time_incident, na.rm = TRUE), "%Y-%m-%dT%H:%M:%S")
base_url <- "https://data.cincinnati-oh.gov/resource/gexm-h6bt.csv"
limit <- 1000
offset <- 0
all_new_rows <- list()

repeat {
  # build the where value and URL-encode only the value (not the ? or &)
  where_value <- paste0("create_time_incident>'", latest_date, "'")
  socrata_url <- paste0(base_url, "?$where=", URLencode(where_value, reserved = TRUE), "&$limit=", limit, "&$offset=", offset)
  message("Requesting: ", socrata_url)

  batch <- tryCatch(
    read.csv(socrata_url, stringsAsFactors = FALSE, na.strings = c("", "NA")),
    error = function(e) {
      message("Error fetching URL: ", e$message)
      return(data.frame())
    }
  )

  if (nrow(batch) == 0) {
    message("No rows returned for offset=", offset, " (nrow=0).")
    break
  }

  all_new_rows[[length(all_new_rows) + 1]] <- batch
  offset <- offset + limit
  if (nrow(batch) < limit) break
}
new_rows <- dplyr::bind_rows(all_new_rows)

# If new rows exists...
if (nrow(new_rows) > 0) {
  # Rename rows to match cfs
  new_rows <- new_rows %>%
    clean_names() %>%
    # Set column types to match cfs
    mutate(
      address_x = as.character(address_x),
      agency = as.character(agency),
      disposition_text = as.character(disposition_text),
      event_number = as.character(event_number),
      incident_type_id = as.character(incident_type_id),
      incident_type_desc = as.character(incident_type_desc),
      priority = as.integer(priority),
      priority_color = as.character(priority_color),
      create_time_incident = as.POSIXct(create_time_incident, format = "%Y-%m-%dT%H:%M:%S", tz = "America/New_York"),
      arrival_time_primary_unit = as.POSIXct(arrival_time_primary_unit, format = "%Y-%m-%dT%H:%M:%S", tz = "America/New_York"),
      closed_time_incident = as.POSIXct(closed_time_incident, format = "%Y-%m-%dT%H:%M:%S", tz = "America/New_York"),
      dispatch_time_primary_unit = as.POSIXct(dispatch_time_primary_unit, format = "%Y-%m-%dT%H:%M:%S", tz = "America/New_York"),
      beat = as.character(beat),
      district = as.character(district),
      sna_neighborhood = as.character(sna_neighborhood),
      cpd_neighborhood = as.character(cpd_neighborhood),
      community_council_neighborhood = as.character(community_council_neighborhood),
      latitude_x = as.numeric(latitude_x),
      longitude_x = as.numeric(longitude_x)
    )
  
  # Combine new rows with existing static data
  cfs <- bind_rows(cfs, new_rows)
}

cfs <- cfs %>%
  mutate(
    create_time_incident      = lubridate::force_tz(create_time_incident,      "America/New_York"),
    arrival_time_primary_unit = lubridate::force_tz(arrival_time_primary_unit, "America/New_York"),
    closed_time_incident      = lubridate::force_tz(closed_time_incident,      "America/New_York"),
    dispatch_time_primary_unit= lubridate::force_tz(dispatch_time_primary_unit,"America/New_York")
  )

cfs <- cfs %>% 
  select(
    -sna_neighborhood,
    -community_council_neighborhood,
    -priority_color,
    -agency,
    -incident_type_desc,
  )

# Add columns
cfs <- cfs %>%
  mutate(
    time_to_close_mins = as.numeric(difftime(closed_time_incident, create_time_incident, units = "mins")),
    time_before_arrival = as.numeric(difftime(arrival_time_primary_unit, create_time_incident, units = "mins")),
    time_before_dispatch = as.numeric(difftime(dispatch_time_primary_unit, create_time_incident, units = "mins"))
  )

# Remove CAD calls of non-significance
cfs <- cfs %>%
  filter(
    !(disposition_text %in% c(
      "USED CLEAR BUTTON",
      "CAN:CANCEL",
      "DUP: DUPLICATE", 
      "TC:TRANSFERRED CALL",
      "26: AVAIL/DETAIL COMPLETED",
      "TEST: TEST",
      "ERR: ERROR INCIDENT",
      "NOT NOT A DISPOSITION",
      "UCPD DETAIL COMPLETE",
      "UCPD NO SERVICES RE",
      "NR:NO REPORT"
    ))
  )

# Filter CFS to calls since Skydio launch (July 28, 2025)
cfs_since_launch <- cfs %>%  
  filter(
    (create_time_incident >= as.POSIXct("2025-07-28 00:00:00", tz = "America/New_York")) |
      (arrival_time_primary_unit >= as.POSIXct("2025-07-28 00:00:00", tz = "America/New_York")) |
      (dispatch_time_primary_unit >= as.POSIXct("2025-07-28 00:00:00", tz = "America/New_York")) |
      (closed_time_incident >= as.POSIXct("2025-07-28 00:00:00", tz = "America/New_York"))
  ) %>%
  arrange(create_time_incident)

# Convert CFS coordinates to sf points
cfs_sf <- st_as_sf(
  cfs_since_launch,
  coords = c("longitude_x", "latitude_x"),
  crs = 4326,
  remove = FALSE
)

# Project to UTM for accurate buffering (Cincinnati is UTM zone 16N, EPSG:32616)
cfs_sf <- st_transform(cfs_sf, 32616)

## Flight data

flights_sf <- st_read(flight_paths_geojson)

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

# Project to UTM for accurate buffering (Cincinnati is UTM zone 16N, EPSG:32616)
flights_sf <- st_transform(flights_sf, 32616)

## Centerlines data

centerlines <- st_read(county_centerlines_geojson)

# Prepare centerlines data with standardized street names
centerlines_clean <- centerlines %>%
  janitor::clean_names() %>%
  mutate(
    # Combine to create full street name
    street_full = case_when(
      !is.na(prefix) & prefix != "" ~ paste(toupper(trimws(prefix)), toupper(trimws(name)), toupper(trimws(suffix))),
      TRUE ~ paste(toupper(trimws(name)), toupper(trimws(suffix)))
    ),
    street_full = trimws(street_full),
    # Store address ranges
    left_from = as.numeric(l_f_add),
    left_to = as.numeric(l_t_add),
    right_from = as.numeric(r_f_add),
    right_to = as.numeric(r_t_add),
    # Calculate block numbers
    left_block_from = floor(left_from/100) * 100,
    left_block_to = floor(left_to/100) * 100,
    right_block_from = floor(right_from/100) * 100,
    right_block_to = floor(right_to/100) * 100
  ) %>%
  # Filter out segments with no address ranges or street names
  filter(!is.na(street_full), street_full != "",
         (!is.na(left_from) | !is.na(right_from)))

# Match centerlines
cfs_with_centerlines <- match_cfs_to_centerlines(cfs_sf, centerlines_clean)
st_write(cfs_with_centerlines$combined, "./output/cfs_with_centerlines.gpkg", delete_dsn = TRUE)

# Load centerlines file if already created
# cfs_with_centerlines <- sf::st_read("./cfs_with_centerlines.gpkg")

### MATCH FLIGHTS TO CFS

# Match all flights (may take a while)
matches <- match_flights_to_cfs(
  cfs_with_centerlines$combined, 
  flights_sf, 
  buffer_distance = 50, 
  time_window_mins = 10
)

# Save matches to CSV
write_csv(matches, "./output/flight_cfs_matches.csv")

### TABLES

# Filter CFS to calls handled by drone
cfs_hbd <- cfs_since_launch %>%
  filter(
    disposition_text == "HBD: HANDLED BY DRONE"
  ) %>%
  arrange(create_time_incident)
 
# Incident types handled by drone, with total
cfs_hbd %>%
  tabyl(incident_type_id) %>%
  arrange(desc(n))

# Incident types in all CFS since launch
cfs_since_launch %>%
  tabyl(incident_type_id) %>%
  arrange(desc(n))

# Disposition codes in all CFS since launch
cfs_since_launch %>% 
  tabyl(disposition_text) %>%
  arrange(desc(n))

# Hour-of-day distribution of calls
cfs_since_launch %>% 
  mutate(hour = hour(create_time_incident)) %>% 
  tabyl(hour) %>% 
  arrange(hour)

# Hour-of-day distribution of HBD calls
cfs_hbd %>% 
  mutate(hour = hour(create_time_incident)) %>% 
  tabyl(hour) %>% 
  arrange(hour)

# Hour-of-day distribution of flights
flights_sf %>% 
  st_drop_geometry() %>% 
  mutate(hour = hour(takeoff)) %>% 
  tabyl(hour) %>% 
  arrange(hour)

# Flights per day
flights_sf %>% 
  st_drop_geometry() %>% 
  mutate(date = as.Date(takeoff)) %>% 
  tabyl(date) %>% 
  arrange(date)

# Flight duration buckets (typical mission lengths)
flights_sf %>% 
  st_drop_geometry() %>% 
  mutate(duration_m = as.numeric(difftime(landing, takeoff, units="mins"))) %>%
  mutate(dur_bin = cut(duration_m, breaks = c(0,5,10,20,30,60, Inf), right=FALSE)) %>% 
  tabyl(dur_bin)

# Most common addresses in matches
matches %>% 
  tabyl(address) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 50)

# Top incident types in all matches
top_matches <- matches %>%
  tabyl(incident_type) %>%
  arrange(desc(n))
write.csv(top_matches, "./output/top_matches.csv")

# Average times to close call when HBD vs. not HBD
top10_incident_types <- cfs_hbd %>%
  tabyl(incident_type_id) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  pull(incident_type_id)

avg_times <- data.frame(
  incident_type_id = character(),
  avg_time_hbd = numeric(),
  avg_time_not_hbd = numeric(),
  percent_change = numeric(),
  total_calls_hbd = integer(),
  total_calls_not_hbd = integer()
)

for (itype in top10_incident_types) {
  avg_time_hbd <- cfs_since_launch %>%
    filter(incident_type_id == itype, disposition_text == "HBD: HANDLED BY DRONE") %>%
    mutate(time_to_close_mins = as.numeric(difftime(closed_time_incident, create_time_incident, units = "mins"))) %>%
    filter(!is.na(time_to_close_mins), time_to_close_mins > 0) %>%
    summarize(avg_time = mean(time_to_close_mins, na.rm = TRUE)) %>%
    pull(avg_time)
  
  avg_time_not_hbd <- cfs_since_launch %>%
    filter(incident_type_id == itype, disposition_text != "HBD: HANDLED BY DRONE") %>%
    mutate(time_to_close_mins = as.numeric(difftime(closed_time_incident, create_time_incident, units = "mins"))) %>%
    filter(!is.na(time_to_close_mins), time_to_close_mins > 0) %>%
    summarize(avg_time = mean(time_to_close_mins, na.rm = TRUE)) %>%
    pull(avg_time)
  
  percent_change <- if (!is.na(avg_time_hbd) && !is.na(avg_time_not_hbd) && avg_time_not_hbd != 0) {
    ((avg_time_hbd - avg_time_not_hbd) / avg_time_hbd) * 100
  } else {
    NA
  }
  
  total_calls_not_hbd <- cfs_since_launch %>%
    filter(incident_type_id == itype) %>%
    nrow()
  
  total_calls_hbd <- cfs_hbd %>%
    filter(incident_type_id == itype) %>%
    nrow()
  
  avg_times <- rbind(avg_times, data.frame(
    incident_type_id = itype,
    avg_time_hbd = avg_time_hbd,
    avg_time_not_hbd = avg_time_not_hbd,
    percent_change = percent_change,
    total_calls_hbd = total_calls_hbd,
    total_calls_not_hbd = total_calls_not_hbd
  ))
}
write_csv(avg_times, "./output/avg_times.csv")

# Average time to close call for incident_type 'BURG RESIDENTIAL (IP)'
avg_burg_close_time <- cfs_since_launch %>%
  filter(incident_type_id %in% c("BURG RESIDENTIAL (IP)", "RBURG", "NRBURG")) %>%
  mutate(time_to_close_mins = as.numeric(difftime(closed_time_incident, create_time_incident, units = "mins"))) %>%
  filter(!is.na(time_to_close_mins), time_to_close_mins > 0) %>%
  summarize(avg_time = mean(time_to_close_mins, na.rm = TRUE)) %>%
  pull(avg_time)

avg_burg_close_time_match <- matches %>%
  filter(incident_type %in% c("BURG RESIDENTIAL (IP)", "RBURG", "NRBURG")) %>%
  mutate(time_to_close_mins = as.numeric(difftime(call_closed_time, call_create_time, units = "mins"))) %>%
  filter(!is.na(time_to_close_mins), time_to_close_mins > 0) %>%
  summarize(avg_time = mean(time_to_close_mins, na.rm = TRUE)) %>%
  pull(avg_time)

# Neighborhoods by number of flight paths crossing into them
if (file.exists(neigh_fp)) {
  neigh <- st_read(neigh_fp, quiet = TRUE)

  # pick a likely name field from the neighborhoods dataset (prefer SNA_NAME)
  possible_names <- c("SNA_NAME", "SNA_NEIGHBORHOOD", "CPD_NEIGHBORHOOD", "name","NAME","neighborhood","Neighborhood","neigh_name","NTAName","NAME10")
  name_field <- names(neigh)[names(neigh) %in% possible_names][1]
  if (is.null(name_field) || is.na(name_field)) name_field <- names(neigh)[1]

  # transform to flights CRS and ensure valid geometries
  neigh <- st_transform(neigh, st_crs(flights_sf))
  flights_valid <- st_make_valid(flights_sf)
  neigh_valid <- st_make_valid(neigh)

  # Count unique flights that intersect each neighborhood (one count per flight per neighborhood)
  ints_list <- st_intersects(neigh_valid, flights_valid, sparse = TRUE)
  neigh_counts <- neigh_valid %>%
    mutate(flight_count = lengths(ints_list)) %>%
    st_drop_geometry() %>%
    transmute(
      neighborhood = .[[name_field]],
      flight_count = flight_count
    ) %>%
    arrange(desc(flight_count))

  write_csv(neigh_counts, "./output/neighborhood_flight_counts.csv")
  message("Wrote ./output/neighborhood_flight_counts.csv (neighborhood + number of flights intersecting)")
} else {
  message("Neighborhood file not found: ", neigh_fp)
}

# Average number of calls for service per day
cfs_since_launch %>%
  mutate(date = as.Date(create_time_incident)) %>%
  tabyl(date) %>%
  summarize(
    avg_calls_per_day = mean(n, na.rm = TRUE),
    total_days = n(),
    total_calls = sum(n, na.rm = TRUE)
  )

# Average priority of calls for service
cfs_since_launch %>%
  summarize(
    avg_priority = mean(priority, na.rm = TRUE)
  )

# Median priority of calls for service
cfs_since_launch %>%
  summarize(
    median_priority = median(priority, na.rm = TRUE)
  )

# Average priority of matches
matches %>%
  summarize(
    avg_priority = mean(priority, na.rm = TRUE)
  )

# Number of priority "1" and "2" calls for service out of total calls
cfs_since_launch %>%
  summarize(
    total_calls = n(),
    priority_1_calls = sum(priority == 1, na.rm = TRUE),
    priority_2_calls = sum(priority == 2, na.rm = TRUE),
    percent_priority_1 = (priority_1_calls / total_calls) * 100,
    percent_priority_2 = (priority_2_calls / total_calls) * 100
  )

### CLEAN UP

## Clean up API calls
# rm(all_new_rows, batch, new_rows, socrata_url, where_value, base_url, limit, offset)

## Clean up centerlines
# rm(centerlines, centerlines_clean)