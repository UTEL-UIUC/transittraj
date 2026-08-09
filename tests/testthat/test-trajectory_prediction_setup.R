# --- predict_traj_input_validation() ---
test_that("predict_traj_input_validation: input combo validation", {

  lineE_traj <- new_transittraj_data("get_trajectory_fun")

  # new dist & new times
  expect_error(
    predict(object = lineE_traj,
            new_distances = c(0, 1),
            new_times = c(0, 1)),
    class = "error_trajpredict_input"
  )
  # new dist or times w/ dist lims & timestep
  expect_error(
    predict(object = lineE_traj,
            new_distances = c(0, 1),
            distance_lims = c(0, 1),
            timestep = 1),
    class = "error_trajpredict_input"
  )
  # timestep w/out distance_lims
  expect_error(
    predict(object = lineE_traj,
            timestep = 1),
    class = "error_trajpredict_input"
  )
  # distance_lims w/out timestep
  expect_error(
    predict(object = lineE_traj,
            distance_lims = c(0, 1)),
    class = "error_trajpredict_input"
  )
  # nothing
  expect_error(
    predict(object = lineE_traj),
    class = "error_trajpredict_input"
  )
})
test_that("predict_traj_input_validation: derivative validation", {

  lineE_traj <- new_transittraj_data("get_trajectory_fun")

  # Larger than allowed
  expect_error(
    predict(object = lineE_traj,
            new_times = c(0, 1),
            deriv = 5),
    class = "error_trajpredict_input"
  )
  # Negative
  expect_error(
    predict(object = lineE_traj,
            new_times = c(0, 1),
            deriv = -1),
    class = "error_trajpredict_input"
  )
  # Deriv w/ new_distances
  expect_error(
    predict(object = lineE_traj,
            new_distance = c(100, 200),
            deriv = 1),
    class = "error_trajpredict_input"
  )
})
test_that("predict_traj_input_validation: inverse validation", {

  lineE_mono <- new_transittraj_data("make_monotonic")
  lineE_traj_noinv <- get_trajectory_fun(distance_df = lineE_mono,
                                       find_inverse_function = FALSE)

  # No inv w/ new_distances
  expect_error(
    predict(object = lineE_traj_noinv,
            new_distances = c(100, 200)),
    class = "error_trajpredict_input"
  )

  # No inv w/ distance_lims
  expect_error(
    predict(object = lineE_traj_noinv,
            distance_lims = c(100, 200),
            timestep = 10),
    class = "error_trajpredict_input"
  )
})

# --- predict_traj_setup_dist_lims () ---
test_that("predict_traj_setup_dist_lims: input validation", {

  distance_df <- data.frame(
    distance = c(0, 10,
                 5, 15),
    event_timestamp = rep(as.POSIXct(c(0, 10)),
                          2),
    trip_id_performed = c("a", "a",
                          "b", "b")
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  traj <- get_trajectory_fun(distance_df = distance_df,
                             use_speeds = FALSE,
                             return_group_function = TRUE,
                             interp_method = "linear")

  # bad range
  expect_error(
    predict_traj_setup_dist_lims(
      trajectory = traj,
      trip_extremes = get_trip_extremes(traj),
      distance_lims = c(100, 200),
      timestep = 10, deriv = 0),
    class = "error_trajpredict_lims"
  )

  # ok range
  expect_no_error(
    predict_traj_setup_dist_lims(
      trajectory = traj,
      trip_extremes = get_trip_extremes(traj),
      distance_lims = c(10, 15),
      timestep = 1, deriv = 0)
  )
})
test_that("predict_traj_setup_dist_lims: output, one deriv", {

  distance_df <- data.frame(
    distance = c(0, 10,
                 5, 15),
    event_timestamp = rep(as.POSIXct(c(0, 10)),
                          2),
    trip_id_performed = c("a", "a",
                          "b", "b")
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  traj <- get_trajectory_fun(distance_df = distance_df,
                             use_speeds = FALSE,
                             return_group_function = TRUE,
                             interp_method = "linear")

  # one deriv
  df <- predict_traj_setup_dist_lims(
    trajectory = traj,
    trip_extremes = get_trip_extremes(traj),
    distance_lims = c(11, 15),
    timestep = 1, deriv = 0
  )

  # trips -- only b
  expect_equal(
    unique(df$trip_id_performed),
    expected = c("b")
  )

  # range
  expect_equal(
    c(min(df$event_timestamp), max(df$event_timestamp)),
    expected = c(6, 10)
  )

  # deriv
  expect_equal(
    unique(df$deriv),
    expected = c(0)
  )

  # timestep
  avg_step <- (max(df$event_timestamp) - min(df$event_timestamp) + 1) /
    length(df$event_timestamp)
  expect_equal(
    avg_step,
    expected = 1
  )
})
test_that("predict_traj_setup_dist_lims: output, multiple derivs", {

  distance_df <- data.frame(
    distance = c(0, 10,
                 5, 15),
    event_timestamp = rep(as.POSIXct(c(0, 10)),
                          2),
    trip_id_performed = c("a", "a",
                          "b", "b")
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  traj <- get_trajectory_fun(distance_df = distance_df,
                             use_speeds = FALSE,
                             return_group_function = TRUE,
                             interp_method = "linear")

  # multiple derivs
  df <- predict_traj_setup_dist_lims(
    trajectory = traj,
    trip_extremes = get_trip_extremes(traj),
    distance_lims = c(11, 15),
    timestep = 1, deriv = c(0, 1)
  )

  # dims
  expect_equal(
    dim(df)[1],
    expected = (15 - 11 + 1) * 2
  )

  # deriv
  expect_equal(
    unique(df$deriv),
    expected = c(0, 1)
  )

  # each timestep twice
  exp_steps <- data.frame(
    event_timestamp = floor(seq(6, 10.5, by = 0.5)),
    deriv = rep(c(0, 1), 5)
  )
  expect_equal(
    as.data.frame(df %>% dplyr::select(event_timestamp, deriv)),
    expected = exp_steps
  )
})

# --- predict_traj_setup_new_times () ---
test_that("predict_traj_setup_new_times: input validation", {

  lineE_traj <- new_transittraj_data("get_trajectory_fun")

  # Dataframe, wrong cols
  expect_error(
    predict(object = lineE_traj,
            new_times = data.frame(timestamp = c(0, 1))),
    class = "error_trajpredict_input"
  )

  # Not DF nor vector
  expect_error(
    predict(object = lineE_traj,
            new_times = lineE_traj),
    class = "error_trajpredict_input"
  )

  # bad time range
  expect_error(
    predict(object = lineE_traj,
            new_times = c(0, 1)),
    class = "error_trajpredict_range"
  )
})
test_that("predict_traj_setup_new_times: sequence input", {

  distance_df <- data.frame(
    distance = c(0, 10,
                 5, 15),
    event_timestamp = rep(as.POSIXct(c(0, 10)),
                          2),
    trip_id_performed = c("a", "a",
                          "b", "b")
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  traj <- get_trajectory_fun(distance_df = distance_df,
                             use_speeds = FALSE,
                             return_group_function = TRUE,
                             interp_method = "linear")

  nt <- seq(from = 0, to = 3, by = 1)
  df <- predict_traj_setup_new_times(new_times = nt,
                                     trip_extremes = get_trip_extremes(traj),
                                     deriv = 0)

  # trips -- both
  expect_equal(
    unique(df$trip_id_performed),
    expected = c("a", "b")
  )

  # deriv
  expect_equal(
    unique(df$deriv),
    expected = c(0)
  )

  # step values
  exp_steps <- data.frame(
    event_timestamp = c(nt, nt),
    trip_id_performed = c(rep("a", 4),
                          rep("b", 4))
  ) %>%
    dplyr::arrange(event_timestamp, trip_id_performed) %>%
    dplyr::mutate(deriv = 0)
  expect_equal(
    df,
    expected = exp_steps
  )
})
test_that("predict_traj_setup_new_times: df input", {

  distance_df <- data.frame(
    distance = c(0, 10,
                 5, 15),
    event_timestamp = rep(as.POSIXct(c(0, 10)),
                          2),
    trip_id_performed = c("a", "a",
                          "b", "b")
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  traj <- get_trajectory_fun(distance_df = distance_df,
                             use_speeds = FALSE,
                             return_group_function = TRUE,
                             interp_method = "linear")

  # no trip ID
  nt <- data.frame(
    event_timestamp = seq(from = 0, to = 3, by = 1)
  ) %>%
    dplyr::mutate(other_col = "c")
  df <- predict_traj_setup_new_times(new_times = nt,
                                     trip_extremes = get_trip_extremes(traj),
                                     deriv = 0)

  # trips -- both
  expect_equal(
    unique(df$trip_id_performed),
    expected = c("a", "b")
  )

  # other col retained
  expect_true(
    "other_col" %in% names(df)
  )
  expect_equal(
    unique(df$other_col),
    expected = c("c")
  )

  # deriv
  expect_equal(
    unique(df$deriv),
    expected = c(0)
  )

  # step values
  exp_steps <- data.frame(
    event_timestamp = c(nt$event_timestamp, nt$event_timestamp),
    trip_id_performed = c(rep("a", 4),
                          rep("b", 4))
  ) %>%
    dplyr::arrange(event_timestamp, trip_id_performed) %>%
    dplyr::mutate(deriv = 0)
  expect_equal(
    df %>% dplyr::select(-other_col),
    expected = exp_steps
  )

  # with trip ID
  nt_2 <- data.frame(
    event_timestamp = seq(from = 0, to = 3, by = 1)
  ) %>%
    dplyr::mutate(trip_id_performed = "a")
  df_2 <- predict_traj_setup_new_times(new_times = nt_2,
                                     trip_extremes = get_trip_extremes(traj),
                                     deriv = 0)

  # trips
  expect_equal(
    unique(df_2$trip_id_performed),
    expected = c("a")
  )

  # deriv
  expect_equal(
    unique(df_2$deriv),
    expected = c(0)
  )

  # steps
  expect_equal(
    df_2$event_timestamp,
    expected = nt_2$event_timestamp
  )
})
test_that("predict_traj_setup_new_times: multiple derivs", {

  distance_df <- data.frame(
    distance = c(0, 10,
                 5, 15),
    event_timestamp = rep(as.POSIXct(c(0, 10)),
                          2),
    trip_id_performed = c("a", "a",
                          "b", "b")
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  traj <- get_trajectory_fun(distance_df = distance_df,
                             use_speeds = FALSE,
                             return_group_function = TRUE,
                             interp_method = "linear")

  nt <- seq(from = 0, to = 3, by = 1)
  df <- predict_traj_setup_new_times(new_times = nt,
                                     trip_extremes = get_trip_extremes(traj),
                                     deriv = c(0, 1))

  # trips -- both
  expect_equal(
    unique(df$trip_id_performed),
    expected = c("a", "b")
  )

  # deriv
  expect_equal(
    unique(df$deriv),
    expected = c(0, 1)
  )

  # step values
  exp_steps <- data.frame(
    event_timestamp = c(nt, nt, nt, nt),
    trip_id_performed = c(rep("a", 8),
                          rep("b", 8)),
    deriv = c(rep(0, 4), rep(1, 4),
              rep(0, 4), rep(1, 4))
  ) %>%
    dplyr::arrange(event_timestamp, trip_id_performed, deriv)
  expect_equal(
    df,
    expected = exp_steps
  )
})

# --- predict_traj_setup_new_dists () ---
test_that("predict_traj_setup_new_dists: input validation", {

  lineE_traj <- new_transittraj_data("get_trajectory_fun")

  # Dataframe, wrong cols
  expect_error(
    predict(object = lineE_traj,
            new_distances = data.frame(dist = c(0, 1))),
    class = "error_trajpredict_input"
  )

  # Not DF nor vector
  expect_error(
    predict(object = lineE_traj,
            new_distances = lineE_traj),
    class = "error_trajpredict_input"
  )

  # vector not numeric
  expect_error(
    predict(object = lineE_traj,
            new_distances = c("100", "200")),
    class = "error_trajpredict_input"
  )

  # bad dist range
  expect_error(
    predict(object = lineE_traj,
            new_distances = c(50000, 50200)),
    class = "error_trajpredict_range"
  )
})
test_that("predict_traj_setup_new_dists: sequence input", {

  distance_df <- data.frame(
    distance = c(0, 10,
                 5, 15),
    event_timestamp = rep(as.POSIXct(c(0, 10)),
                          2),
    trip_id_performed = c("a", "a",
                          "b", "b")
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  traj <- get_trajectory_fun(distance_df = distance_df,
                             use_speeds = FALSE,
                             return_group_function = TRUE,
                             interp_method = "linear")

  nd <- seq(from = 4, to = 6, by = 1)
  df <- predict_traj_setup_new_dists(new_distances = nd,
                                     trip_extremes = get_trip_extremes(traj))

  # trips -- both
  expect_equal(
    unique(df$trip_id_performed),
    expected = c("a", "b")
  )

  # step values
  exp_steps <- data.frame(
    distance = c(4, 5, 5, 6, 6),
    trip_id_performed = c("a",
                          rep(c("a", "b"), 2))
  )
  expect_equal(
    df,
    expected = exp_steps
  )
})
test_that("predict_traj_setup_new_dists: df input", {

  distance_df <- data.frame(
    distance = c(0, 10,
                 5, 15),
    event_timestamp = rep(as.POSIXct(c(0, 10)),
                          2),
    trip_id_performed = c("a", "a",
                          "b", "b")
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  traj <- get_trajectory_fun(distance_df = distance_df,
                             use_speeds = FALSE,
                             return_group_function = TRUE,
                             interp_method = "linear")

  # no trip ID
  nd <- data.frame(
    distance = seq(from = 4, to = 6, by = 1)
  ) %>%
    dplyr::mutate(other_col = "c")
  df <- predict_traj_setup_new_dists(new_distances = nd,
                                     trip_extremes = get_trip_extremes(traj))

  # trips -- both
  expect_equal(
    unique(df$trip_id_performed),
    expected = c("a", "b")
  )

  # other col retained
  expect_true(
    "other_col" %in% names(df)
  )
  expect_equal(
    unique(df$other_col),
    expected = c("c")
  )

  # step values
  exp_steps <- data.frame(
    distance = c(4, 5, 5, 6, 6),
    trip_id_performed = c("a",
                          rep(c("a", "b"), 2))
  )
  expect_equal(
    df %>% dplyr::select(-other_col),
    expected = exp_steps
  )

  # with trip ID
  nd_2 <- data.frame(
    distance = seq(from = 4, to = 6, by = 1)
  ) %>%
    dplyr::mutate(trip_id_performed = "a")
  df_2 <- predict_traj_setup_new_dists(new_distances = nd_2,
                                       trip_extremes = get_trip_extremes(traj))

  # trips
  expect_equal(
    unique(df_2$trip_id_performed),
    expected = c("a")
  )

  # steps
  expect_equal(
    df_2$distance,
    expected = nd_2$distance
  )
})
