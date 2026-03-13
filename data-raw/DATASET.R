## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

# Import raw data from this folder
wmata_gtfs_full <- tidytransit::read_gtfs(".\\data-raw\\wmata_gtfs.zip")
wmata_gtfs_rt <- read.csv(".\\data-raw\\wmata_gtfsrt_021626.csv")

# Filter GTFS to only desired routes
keep_routes <- unique(wmata_gtfs_rt$vehicle.trip.route_id)
wmata_gtfs <- filter_by_route(gtfs = wmata_gtfs_full,
                                   route_ids = keep_routes)

# --- Exported Data ---
# Format GTFS-RT into TIDES AVL
wmata_avl <- wmata_gtfs_rt %>%
  # Rename to TIDES
  dplyr::rename(location_ping_id = X,
         trip_id_performed = vehicle.trip.trip_id,
         vehicle_id = id,
         service_date = vehicle.trip.start_date,
         route_id = vehicle.trip.route_id,
         direction_id = vehicle.trip.direction_id,
         latitude = vehicle.position.latitude,
         longitude = vehicle.position.longitude,
         speed = vehicle.position.speed,
         trip_stop_sequence = vehicle.current_stop_sequence,
         event_timestamp = vehicle.timestamp,
         stop_id = vehicle.stop_id) %>%
  # Remove columns we won't use
  dplyr::select(-c(vehicle.trip.start_time,
                   vehicle.trip.schedule_relationship,
                   vehicle.position.bearing,
                   vehicle.occupancy_status,
                   vehicle.current_status,
                   vehicle.vehicle.id,
                   vehicle.vehicle.label,
                   vehicle.occupancy_status)) %>%
  # Change datatype to TIDES
  dplyr::mutate(event_timestamp = as.POSIXct(event_timestamp,
                                             tz = "America/New_York"),
                service_date = as.Date(as.character(service_date), format = "%Y%m%d"),
                trip_id_performed = as.character(trip_id_performed),
                vehicle_id = as.character(vehicle_id),
                location_ping_id = as.character(location_ping_id))

# Verify TIDES compliance
wmata_tides <- validate_tides(avl_df = wmata_avl)

# Save to package
usethis::use_data(wmata_avl)
usethis::use_data(wmata_gtfs)



# --- Internal Data ---

# Will first narrow down the original data
# - Setup -
c53 <- "C53"
c53_dir <- 0 # 0 is NB, 1 is SB
c53_avl <- wmata_avl %>%
  dplyr::filter((route_id == c53) & (direction_id == c53_dir)) %>%
  dplyr::filter(trip_id_performed %in% c("13300100", "13437100"))
c53_gtfs <- filter_by_route(gtfs = wmata_gtfs,
                            route_ids = c53,
                            dir_id = c53_dir)
c53_NB_shape_id <- "C53:04"
dc_CRS <- 32618
c53_shape <- get_shape_geometry(gtfs = c53_gtfs,
                                shape = c53_NB_shape_id,
                                project_crs = dc_CRS)

# - Cleaning Workflow -
c53_buffer = 50 # meters
c53_distances <- get_linear_distances(avl_df = c53_avl,
                                      shape_geometry = c53_shape,
                                      project_crs = dc_CRS,
                                      clip_buffer = c53_buffer)
c53_cleaned_subtrips <- clean_overlapping_subtrips(
  distance_df = c53_distances,
  check_operator = FALSE,
  remove_single_observations = TRUE,
  remove_non_overlapping = FALSE
)
c53_max_jump <- 20 # meters
c53_min_jump <- -1 * c53_max_jump # meters
c53_no_jumps <- clean_jumps(distance_df = c53_cleaned_subtrips,
                            max_median_deviation = c53_max_jump,
                            min_median_deviation = c53_min_jump,
                            t_cutoff = Inf)
c53_min_dist <- 500 # meters
c53_min_time <- 90 # seconds
c53_max_gap <- 500 # meters
c53_cleaned_incompletes <- clean_incomplete_trips(
  distance_df = c53_no_jumps,
  min_trip_distance = c53_min_dist,
  min_trip_duration = c53_min_time,
  max_distance_gap = c53_max_gap
)
c53_trimmed <- trim_trips(distance_df = c53_cleaned_incompletes,
                          trim_type = "both")
c53_dist_error = 0.001
c53_mono <- make_monotonic(distance_df = c53_trimmed,
                           correct_speed = TRUE,
                           add_distance_error = c53_dist_error)
c53_traj <- get_trajectory_fun(distance_df = c53_mono)
c53_traj_singles <- get_trajectory_fun(distance_df = c53_mono,
                                       return_group_function = FALSE)

# - Save internal data -
usethis::use_data(c53_gtfs,
                  c53_shape,
                  c53_avl,
                  c53_distances,
                  c53_cleaned_subtrips,
                  c53_no_jumps,
                  c53_cleaned_incompletes,
                  c53_trimmed,
                  c53_mono,
                  c53_traj,
                  c53_traj_singles,
                  internal = TRUE)
