# --- plot_traj_input_vaoidation() ---
test_that("plot_traj_input_validation: input validation", {

  lineE_mono <- new_transittraj_data("make_monotonic")
  lineE_traj <- get_trajectory_fun(distance_df = lineE_mono,
                                 find_inverse_function = TRUE)
  lineE_traj_noinv <- get_trajectory_fun(distance_df = lineE_mono,
                                       find_inverse_function = FALSE)

  # --- General ---
  # No inputs
  expect_error(
    plot_trajectory(),
    class = "error_plottraj_input"
  )
  # Too many inputs
  expect_error(
    plot_trajectory(trajectory = lineE_traj,
                    distance_df = lineE_mono),
    class = "error_plottraj_input"
  )

  # --- Traj ---
  # Wrong input: traj
  expect_error(
    plot_trajectory(trajectory = lineE_mono),
    class = "error_plottraj_input"
  )
  # No inv
  expect_message(
    plot_trajectory(trajectory = lineE_traj_noinv),
    class = "inform_plottraj_input"
  )

  # --- Dist ---
  # Wrong input: dist
  expect_error(
    plot_trajectory(distance_df = lineE_traj),
    class = "error_plottraj_input"
  )
  # Missing event_timestamp
  expect_error(
    plot_trajectory(distance_df = (lineE_mono %>% dplyr::select(-event_timestamp))),
    class = "error_plottraj_input"
  )
  # Missing trip_id_performed
  expect_error(
    plot_trajectory(distance_df = (lineE_mono %>% dplyr::select(-distance))),
    class = "error_plottraj_input"
  )
  # Missing distance
  expect_error(
    plot_trajectory(distance_df = (lineE_mono %>% dplyr::select(-distance))),
    class = "error_plottraj_input"
  )
  # Data type distance
  # Missing distance
  expect_error(
    plot_trajectory(distance_df = (lineE_mono %>%
                                     dplyr::mutate(distance = as.character(distance)))),
    class = "error_plottraj_input"
  )
})

# --- plot_traj_df_setup() ---
test_that("plot_traj_df_setup: range validation", {

  lineE_mono <- new_transittraj_data("make_monotonic")
  lineE_traj_noinv <- get_trajectory_fun(distance_df = lineE_mono,
                                         find_inverse_function = FALSE)
  lineE_traj_inv <- get_trajectory_fun(distance_df = lineE_mono,
                                       find_inverse_function = TRUE)

  # --- has_inv ---
  # bad lims w/ inverse will be caught by predict() validators
  # no distance lims
  df_1 <- plot_traj_df_setup(trajectory = lineE_traj_inv,
                             has_inv = TRUE,
                             plot_trips = unique(lineE_mono$trip_id_performed)[1],
                             timestep = 120,
                             distance_lims = NULL)
  obs_range <- lineE_mono %>%
    dplyr::filter(trip_id_performed == unique(lineE_mono$trip_id_performed)[1]) %>%
    dplyr::summarize(min_dist = min(distance),
                     max_dist = max(distance))
  expect_s3_class(
    df_1,
    class = "data.frame"
  )
  expect_equal(
    min(df_1$distance),
    expected = obs_range$min_dist, tolerance = 1
  )
  expect_equal(
    max(df_1$distance),
    expected = obs_range$max_dist, tolerance = 1000
  )

  # distance lims
  test_lims <- c(500, 1000)
  df_2 <- plot_traj_df_setup(trajectory = lineE_traj_inv,
                             has_inv = TRUE,
                             plot_trips = unique(lineE_mono$trip_id_performed)[1],
                             timestep = 10,
                             distance_lims = test_lims)
  expect_equal(
    min(df_2$distance),
    expected = test_lims[1], tolerance = 100
  )
  expect_equal(
    max(df_2$distance),
    expected = test_lims[2], tolerance = 100
  )

  # --- no inv ---
  # bad lims
  expect_error(
    suppressMessages(plot_traj_df_setup(trajectory = lineE_traj_noinv,
                                        has_inv = FALSE,
                                        plot_trips = unique(lineE_mono$trip_id_performed)[1],
                                        timestep = 5,
                                        distance_lims = c(50000, 50200))),
    class = "error_plottraj_input"
  )

  # ok lims
  df_3 <- plot_traj_df_setup(trajectory = lineE_traj_noinv,
                             has_inv = FALSE,
                             plot_trips = unique(lineE_mono$trip_id_performed)[1],
                             timestep = 10,
                             distance_lims = test_lims)
  expect_equal(
    min(df_3$distance),
    expected = test_lims[1], tolerance = 100
  )
  expect_equal(
    max(df_3$distance),
    expected = test_lims[2], tolerance = 100
  )
})

# --- plot_trips_df_setup() ---
test_that("plot_trips_df_setup: range validation", {

  # problems w/ traj object will be caught by plot_traj_df_setup,
  # so will only check distance_df here
  lineE_mono <- new_transittraj_data("make_monotonic")
  lineE_traj <- get_trajectory_fun(distance_df = lineE_mono)

  # bad distance range
  expect_error(
    plot_trips_df_setup(distance_df = lineE_mono,
                        trajectory = NULL, plot_trips = NULL,
                        center_vehicles = FALSE, convert_to_timezone = FALSE,
                        distance_lims = c(50000, 50200)),
    class = "error_plottraj_inputdata"
  )

  # bad trips
  expect_error(
    plot_trips_df_setup(distance_df = lineE_mono,
                        trajectory = NULL, plot_trips = c("a", "b"),
                        center_vehicles = FALSE, convert_to_timezone = FALSE,
                        distance_lims = NULL),
    class = "error_plottraj_inputdata"
  )

  # - timezone -
  # distance_df
  expect_warning(
    plot_trips_df_setup(distance_df = (lineE_mono %>% dplyr::mutate(event_timestamp = as.numeric(event_timestamp))),
                        trajectory = NULL, plot_trips = NULL,
                        center_vehicles = FALSE, convert_to_timezone = TRUE,
                        distance_lims = NULL),
    class = "warn_plottraj_inputtz"
  )
  df_1 <- plot_trips_df_setup(distance_df = lineE_mono,
                              trajectory = NULL, plot_trips = NULL,
                              center_vehicles = FALSE, convert_to_timezone = TRUE,
                              distance_lims = NULL)
  expect_equal(
    attr(df_1$event_timestamp, which = "tz"),
    expected = "America/Los_Angeles"
  )

  # traj
  df_2 <- plot_trips_df_setup(trajectory = lineE_traj,
                      distance_df = NULL, plot_trips = NULL, timestep = 120,
                      center_vehicles = FALSE, convert_to_timezone = TRUE,
                      distance_lims = NULL)
  expect_equal(
    attr(df_2$event_timestamp, which = "tz"),
    expected = "America/Los_Angeles"
  )

  # - centering -
  # distance_df
  df_3 <- plot_trips_df_setup(distance_df = lineE_mono,
                              trajectory = NULL, plot_trips = NULL,
                              center_vehicles = TRUE, convert_to_timezone = FALSE,
                              distance_lims = NULL) %>%
    dplyr::group_by(trip_id_performed) %>%
    dplyr::summarize(start_time = min(event_timestamp))
  expect_all_equal(
    df_3$start_time,
    expected = 0
  )

  # traj
  df_4 <- plot_trips_df_setup(trajectory = lineE_traj,
                              distance_df = NULL, plot_trips = NULL, timestep = 120,
                              center_vehicles = TRUE, convert_to_timezone = FALSE,
                              distance_lims = NULL) %>%
    dplyr::group_by(trip_id_performed) %>%
    dplyr::summarize(start_time = min(event_timestamp))
  expect_all_equal(
    df_4$start_time,
    expected = 0
  )
})
