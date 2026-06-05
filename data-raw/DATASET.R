devtools::load_all()
library(magrittr)

# --- Load ---
lacmta_gtfs_raw <- tidytransit::read_gtfs(".\\data-raw\\lacmta_gtfs_rail_05-27-26.zip")
lacmta_tides_raw <- read.csv(".\\data-raw\\lacmta_tides_rail_05-27-26.csv")

# Filtering parameters
# Keep E Line trips starting between 8 am and 12 pm
keep_route <- c("801", "804") # A & E Lines
time_lims <- c(as.POSIXct("1970-01-01 06:00:00", tz = "UTC"),
               as.POSIXct("1970-01-01 08:00:00", tz = "UTC"))

# --- GTFS ---
# Add agency ID to routes
lacmta_gtfs_raw$routes <- lacmta_gtfs_raw$routes %>%
  dplyr::mutate(agency_id = "LACMTA_Rail")

# Filter
gtfs_filt <- filter_by_route(gtfs = lacmta_gtfs_raw,
                             route_ids = keep_route)
gtfs_filt2 <- tidytransit::filter_feed_by_date(gtfs_obj = gtfs_filt,
                                               extract_date = "2026-05-27")

# Find trip IDs to keep
keep_trips <- gtfs_filt2$stop_times %>%
  dplyr::mutate(departure_time = as.POSIXct(departure_time)) %>%
  dplyr::group_by(trip_id) %>%
  dplyr::summarize(start_time = min(departure_time)) %>%
  dplyr::filter((start_time >= time_lims[1]) &
                  start_time <= time_lims[2]) %>%
  dplyr::pull(trip_id)

# --- TIDES ---
tides_sel <- lacmta_tides_raw %>%
  # Select only desired columns
  dplyr::select(location_ping_id,
                service_date,
                trip_id_performed,
                latitude, longitude,
                speed,
                vehicle_id,
                event_timestamp) %>%
  # Adjust data types
  dplyr::mutate(trip_id_performed = as.character(trip_id_performed),
                event_timestamp = as.POSIXct(event_timestamp,
                                             tz = "America/Los_Angeles"))

# Filter to keep trips
tides_filt <- tides_sel %>%
  dplyr::filter(trip_id_performed %in% keep_trips)

# Join direction & shape ID
# Not standard part of TIDES vehicle_locations, but will be helpful to the user
tides_join <- tides_filt %>%
  dplyr::left_join(y = (gtfs_filt$trips %>% dplyr::select(trip_id,
                                                          direction_id,
                                                          shape_id)),
                   by = c("trip_id_performed" = "trip_id")) %>%
  # Get route ID -- stored in shape_id
  dplyr::mutate(route_id = substr(shape_id,
                                  start = 1, stop = 3)) %>%
  # Order by time
  dplyr::arrange(trip_id_performed,
                 event_timestamp)


# --- Save ---
# Rename
lacmta_avl <- tides_join
lacmta_gtfs <- gtfs_filt2

# Save
usethis::use_data(lacmta_avl,
                  overwrite = TRUE)
usethis::use_data(lacmta_gtfs,
                  overwrite = TRUE)


