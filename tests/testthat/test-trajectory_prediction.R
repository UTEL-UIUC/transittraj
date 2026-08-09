# --- predict() ---
# setup & calculation are thoroughly tested elsewhere; simply check that
# correct dispatching occurs here
test_that("predict: group", {

  distance_df <- data.frame(
    distance = c(0, 2),
    event_timestamp = as.POSIXct(c(0, 1)),
    trip_id_performed = rep("a", 2),
    location_ping_id = c("1", "2")
  )

  traj <- get_trajectory_fun(distance_df = distance_df,
                             interp_method = "linear",
                             use_speeds = FALSE,
                             return_group_function = TRUE)

  # new times
  t_1 <- stats::predict(object = traj,
                        new_times = c(0.5),
                        deriv = 0)
  exp_1 <- data.frame(event_timestamp = 0.5,
                      trip_id_performed = "a",
                      deriv = 0,
                      interp = 1)
  expect_equal(
    t_1,
    expected = as.data.frame(exp_1)
  )

  # new distances
  t_2 <- stats::predict(object = traj,
                        new_distances = c(1))
  exp_2 <- data.frame(distance = 1,
                      trip_id_performed = "a",
                      interp = 0.5)
  expect_equal(
    t_2,
    expected = as.data.frame(exp_2)
  )

  # dist lims & timestep
  t_3 <- stats::predict(object = traj,
                        distance_lims = c(0, 1), timestep = 1,
                        deriv = 0)
  exp_3 <- data.frame(trip_id_performed = "a",
                      event_timestamp = 0,
                      deriv = 0,
                      interp = 0)
  expect_equal(
    as.data.frame(t_3),
    expected = as.data.frame(exp_3)
  )
})
test_that("predict: single", {

  distance_df <- data.frame(
    distance = c(0, 2),
    event_timestamp = as.POSIXct(c(0, 1)),
    trip_id_performed = rep("a", 2),
    location_ping_id = c("1", "2")
  )

  traj <- get_trajectory_fun(distance_df = distance_df,
                             interp_method = "linear",
                             use_speeds = FALSE,
                             return_group_function = FALSE)

  # new times
  t_1 <- stats::predict(object = traj[[1]],
                        new_times = c(0.5),
                        deriv = 0)
  exp_1 <- data.frame(event_timestamp = 0.5,
                      trip_id_performed = "a",
                      deriv = 0,
                      interp = 1)
  expect_equal(
    t_1,
    expected = as.data.frame(exp_1)
  )

  # new distances
  t_2 <- stats::predict(object = traj[[1]],
                        new_distances = c(1))
  exp_2 <- data.frame(distance = 1,
                      trip_id_performed = "a",
                      interp = 0.5)
  expect_equal(
    t_2,
    expected = as.data.frame(exp_2)
  )

  # dist lims & timestep
  t_3 <- stats::predict(object = traj[[1]],
                        distance_lims = c(0, 1), timestep = 1,
                        deriv = 0)
  exp_3 <- data.frame(trip_id_performed = "a",
                      event_timestamp = 0,
                      deriv = 0,
                      interp = 0)
  expect_equal(
    as.data.frame(t_3),
    expected = as.data.frame(exp_3)
  )
})

# --- get_trip_extremes() ---
test_that("get_trip_extremes: input validation", {

  lineE_traj <- new_transittraj_data("get_trajectory_fun")

  # --- trajectory ---
  expect_error(
    get_trip_extremes(trajectory = "abc"),
    class = "error_trajextremes_input"
  )

  # --- filter_trips ---
  expect_error(
    get_trip_extremes(trajectory = lineE_traj,
                      filter_trips = c("a", "b")),
    class = "error_trajextremes_input"
  )
})
test_that("get_trip_extremes: no filter", {

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
                             interp_method = "linear",
                             use_speeds = FALSE,
                             return_group_function = TRUE)

  t <- get_trip_extremes(trajectory = traj)

  # trips
  expect_equal(
    unique(t$trip_id_performed),
    expect = unique(distance_df$trip_id_performed)
  )

  # min dists
  expect_equal(
    t$min_dist,
    expected = c(0, 5)
  )

  # max dists
  expect_equal(
    t$max_dist,
    expected = c(10, 15)
  )

  # min times
  expect_equal(
    t$min_time,
    expected = c(0, 0)
  )

  # max times
  expect_equal(
    t$max_time,
    expected = c(10, 10)
  )
})
test_that("get_trip_extremes: filter", {

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
                             interp_method = "linear",
                             use_speeds = FALSE,
                             return_group_function = TRUE)

  t <- get_trip_extremes(trajectory = traj,
                         filter_trips = "a")

  # trips
  expect_equal(
    unique(t$trip_id_performed),
    expect = "a"
  )

  # min dists
  expect_equal(
    t$min_dist,
    expected = c(0)
  )

  # max dists
  expect_equal(
    t$max_dist,
    expected = c(10)
  )

  # min times
  expect_equal(
    t$min_time,
    expected = c(0)
  )

  # max times
  expect_equal(
    t$max_time,
    expected = c(10)
  )
})
