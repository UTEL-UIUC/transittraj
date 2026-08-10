# --- plot_traj_input_validation() ---
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
})
test_that("plot_trips_df_setup: timezones", {

  lineE_mono <- new_transittraj_data("make_monotonic")
  lineE_traj <- get_trajectory_fun(distance_df = lineE_mono)

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
})
test_that("plot_trips_df_setup: vehicle centering", {

  lineE_mono <- new_transittraj_data("make_monotonic")
  lineE_traj <- get_trajectory_fun(distance_df = lineE_mono)
  lineE_traj_s <- get_trajectory_fun(distance_df = lineE_mono,
                                     return_group_function = FALSE)

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

  # traj group
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

  # traj single
  df_5 <- plot_trips_df_setup(trajectory = lineE_traj_s[[1]],
                              distance_df = NULL, plot_trips = NULL, timestep = 120,
                              center_vehicles = TRUE, convert_to_timezone = FALSE,
                              distance_lims = NULL) %>%
    dplyr::group_by(trip_id_performed) %>%
    dplyr::summarize(start_time = min(event_timestamp))
  expect_all_equal(
    df_5$start_time,
    expected = 0
  )
})

# --- plot_feature_df_setup() ---
test_that("plot_feature_df_setup: range validation", {

  test_features <- data.frame(name = c("a", "b", "c"),
                              distance = c(100, 500, 1000))

  # bad lims
  expect_error(
    plot_feature_df_setup(feature_distances = test_features,
                          distance_lims = c(0, 10)),
    class = "error_plottraj_inputdata"
  )

  # ok lims
  feats <- plot_feature_df_setup(feature_distances = test_features,
                                 distance_lims = c(50, 150))
  expect_equal(
    dim(feats)[1],
    expected = 1
  )
  expect_equal(
    feats$name[1],
    expected = "a"
  )
})
test_that("plot_feature_df_setup: input validation", {

  test_features <- data.frame(name = c("a", "b", "c"),
                              distance = c(100, 500, 1000))

  # bad data type
  expect_error(
    plot_feature_df_setup(feature_distances = test_features$distance,
                          distance_lims = NULL),
    class = "error_plottraj_features"
  )

  # bad col name
  expect_error(
    plot_feature_df_setup(feature_distances = test_features %>% dplyr::rename(dist = distance),
                          distance_lims = NULL),
    class = "error_plottraj_features"
  )

  # bad col type
  expect_error(
    plot_feature_df_setup(feature_distances = test_features %>% dplyr::mutate(distance = as.character(distance)),
                          distance_lims = NULL),
    class = "error_plottraj_features"
  )
})

# --- plot_format_setup() ---
test_that("plot_format_setup: input validation", {

  # ok inputs
  plot_df <- data.frame(name = c("a", "b", "c"),
                        name2 = c("1", "2", "3"),
                        val = c(1, 2, 3))
  format_input <- data.frame(color = c("blue"),
                             name = c("a"))
  format_type <- "color"
  format_name <- "p_color"

  # bad input column names
  expect_error(
    plot_format_setup(plotting_df = plot_df,
                      attribute_input = (format_input %>% dplyr::rename(col = color)),
                      attribute_type = format_type,
                      attribute_name = format_name,
                      user_show_legend = FALSE),
    class = "error_plottraj_format"
  )

  # no match between format & plotting data
  expect_error(
    plot_format_setup(plotting_df = plot_df,
                      attribute_input = (format_input %>% dplyr::rename(n = name)),
                      attribute_type = format_type,
                      attribute_name = format_name,
                      user_show_legend = FALSE),
    class = "error_plottraj_format"
  )

  # multiple matches between format & plotting data
  expect_error(
    plot_format_setup(plotting_df = plot_df,
                      attribute_input = (format_input %>% dplyr::mutate(name2 = "1")),
                      attribute_type = format_type,
                      attribute_name = format_name,
                      user_show_legend = FALSE),
    class = "error_plottraj_format"
  )


})
test_that("plot_format_setup: vector outputs", {

  plot_df <- data.frame(name = c("a", "b", "c"),
                        name2 = c("1", "2", "3"),
                        val = c(1, 2, 3))
  format_input_df <- data.frame(color = c("blue"),
                                name = c("a"))
  format_input_vec <- "red"
  format_type <- "color"
  format_name <- "p_color"

  # - vector input-
  f_1 <- plot_format_setup(plotting_df = plot_df,
                           attribute_input = format_input_vec,
                           attribute_type = format_type,
                           attribute_name = format_name,
                           user_show_legend = FALSE)
  # attribute column name
  expect_true(
    f_1[[3]] %in% names(f_1[[1]])
  )
  # attribute column dummy value
  expect_all_equal(
    f_1[[1]][,f_1[[3]]],
    expected = names(f_1[[4]])
  )
  # format value
  expect_equal(
    unname(f_1[[4]]),
    expected = format_input_vec
  )
  # legend status
  expect_equal(
    f_1[[2]],
    expected = "none"
  )

  f_2 <- plot_format_setup(plotting_df = plot_df,
                           attribute_input = format_input_vec,
                           attribute_type = format_type,
                           attribute_name = format_name,
                           user_show_legend = TRUE)
  # legend status
  expect_equal(
    f_2[[2]],
    expected = "legend"
  )
})
test_that("plot_format_setup: df outputs", {

  plot_df <- data.frame(name = c("a", "b", "c"),
                        name2 = c("1", "2", "3"),
                        val = c(1, 2, 3))
  format_input_df <- data.frame(color = c("blue", "red", "green"),
                                name = c("a", "b", "c"))
  format_input_vec <- "red"
  format_type <- "color"
  format_name <- "p_color"

  f_1 <- plot_format_setup(plotting_df = plot_df,
                           attribute_input = format_input_df,
                           attribute_type = format_type,
                           attribute_name = format_name,
                           user_show_legend = FALSE)

  # names of attribute values vector
  expect_setequal(
    names(f_1[[4]]),
    expected = plot_df$name
  )
  # values of attribute values vector
  expect_setequal(
    unname(f_1[[4]]),
    expected = format_input_df$color
  )
  # name of attribute column
  expect_all_true(
    c(f_1[[3]] %in% names(format_input_df),
      f_1[[3]] %in% names(plot_df))
  )
  # legend status
  expect_equal(
    f_1[[2]],
    expected = "none"
  )

  f_2 <- plot_format_setup(plotting_df = plot_df,
                           attribute_input = format_input_df,
                           attribute_type = format_type,
                           attribute_name = format_name,
                           user_show_legend = TRUE)
  # legend status
  expect_equal(
    f_2[[2]],
    expected = "legend"
  )
})
