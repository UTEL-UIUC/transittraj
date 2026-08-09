# --- summary() ---
test_that("summary: group", {

  distance_df <- rbind(
    data.frame(
      event_timestamp = as.POSIXct(c(0, 10)),
      distance = c(0, 5),
      trip_id_performed = c("a", "a")),
    data.frame(
      event_timestamp = as.POSIXct(c(0, 10)),
      distance = c(0, 5),
      trip_id_performed = c("b", "b"))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  traj <- get_trajectory_fun(distance_df = distance_df,
                             interp_method = "linear",
                             use_speeds = FALSE,
                             return_group_function = TRUE)

  exp_summ <- list(
    num_trips = length(unique(distance_df$trip_id_performed)),
    min_dist = min(distance_df$distance),
    max_dist = max(distance_df$distance),
    min_time = min(distance_df$event_timestamp),
    max_time = max(distance_df$event_timestamp),
    is_traj = TRUE,
    traj_type = "linear",
    max_deriv = 0,
    is_inv = TRUE,
    inv_tol = 0.01,
    used_speeds = FALSE
  )
  actual_summ <- summary(traj)

  exp_print <- "------
AVL Group Trajectory Object
------
Number of trips: 2
Total distance range: 0 to 5
Total time range: 0 to 10
------
Trajectory function present: TRUE
   --> Trajectory interpolation method: linear
   --> Maximum derivative: 0
   --> Fit with speeds: FALSE
Inverse function present: TRUE
   --> Inverse function tolerance: 0.01
------"

  expect_equal(
    unclass(actual_summ),
    expected = exp_summ
  )
  expect_output(
    print(actual_summ),
    expected = exp_print
  )
})
test_that("summary: single", {

  distance_df <- rbind(
    data.frame(
      event_timestamp = as.POSIXct(c(0, 10)),
      distance = c(0, 5),
      trip_id_performed = c("a", "a")),
    data.frame(
      event_timestamp = as.POSIXct(c(0, 10)),
      distance = c(0, 5),
      trip_id_performed = c("b", "b"))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  traj <- get_trajectory_fun(distance_df = distance_df,
                             interp_method = "linear",
                             use_speeds = FALSE,
                             return_group_function = FALSE)

  exp_summ <- list(
    trip_id = "a",
    min_dist = min(distance_df %>%
                     dplyr::filter(trip_id_performed == "a") %>%
                     dplyr::pull(distance)),
    max_dist = max(distance_df %>%
                     dplyr::filter(trip_id_performed == "a") %>%
                     dplyr::pull(distance)),
    min_time = min(distance_df %>%
                     dplyr::filter(trip_id_performed == "a") %>%
                     dplyr::pull(event_timestamp)),
    max_time = max(distance_df %>%
                     dplyr::filter(trip_id_performed == "a") %>%
                     dplyr::pull(event_timestamp)),
    is_traj = TRUE,
    traj_type = "linear",
    max_deriv = 0,
    is_inv = TRUE,
    inv_tol = 0.01,
    used_speeds = FALSE
  )
  actual_summ <- summary(traj[[1]])

  exp_print <- "------
AVL Single Trajectory Object
------
Trip ID: a
Trip distance range: 0 to 5
Trip time range: 0 to 10
------
Trajectory function present: TRUE
   --> Trajectory interpolation method: linear
   --> Maximum derivative: 0
   --> Fit with speeds: FALSE
Inverse function present: TRUE
   --> Inverse function tolerance: 0.01
------"

  expect_equal(
    unclass(actual_summ),
    expected = exp_summ
  )
  expect_output(
    print(actual_summ),
    expected = exp_print
  )
})

# --- print() ---
test_that("print: group", {

  distance_df <- rbind(
    data.frame(
      event_timestamp = as.POSIXct(c(0, 10)),
      distance = c(0, 5),
      trip_id_performed = c("a", "a")),
    data.frame(
      event_timestamp = as.POSIXct(c(0, 10)),
      distance = c(0, 5),
      trip_id_performed = c("b", "b"))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  traj <- get_trajectory_fun(distance_df = distance_df,
                             interp_method = "linear",
                             use_speeds = FALSE,
                             return_group_function = TRUE)

  exp_print <- "AVL group trajectory with 2 trips"

  expect_output(
    print(traj),
    regexp = exp_print
  )
})
test_that("print: single", {

  distance_df <- rbind(
    data.frame(
      event_timestamp = as.POSIXct(c(0, 10)),
      distance = c(0, 5),
      trip_id_performed = c("a", "a")),
    data.frame(
      event_timestamp = as.POSIXct(c(0, 10)),
      distance = c(0, 5),
      trip_id_performed = c("b", "b"))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  traj <- get_trajectory_fun(distance_df = distance_df,
                             interp_method = "linear",
                             use_speeds = FALSE,
                             return_group_function = FALSE)

  exp_print <- "AVL single trajectory for trip ID a"

  expect_output(
    print(traj[[1]]),
    regexp = exp_print
  )
})

# --- plot() ---
test_that("plot: trip warnings", {

  distance_df <- data.frame(
    distance = rep(c(0, 10),
                   51),
    event_timestamp = rep(as.POSIXct(c(0, 10)),
                          51)
  ) %>%
    dplyr::mutate(location_ping_id = dplyr::row_number() - 1,
                  trip_id_performed = floor(location_ping_id / 2),
                  location_ping_id = as.character(location_ping_id),
                  trip_id_performed = as.character(trip_id_performed))

  traj <- get_trajectory_fun(distance_df = distance_df,
                             interp_method = "linear",
                             use_speeds = FALSE,
                             return_group_function = TRUE)

  # 51 trips, throw warning
  expect_warning(
    plot(traj),
    class = "warn_plotting_groupnum"
  )

  # should be 50 plotted
  p <- suppressWarnings(plot(traj))
  expect_equal(
    length(unique(p$data$trip_id_performed)),
    expected = 50
  )

  traj_2 <- get_trajectory_fun(distance_df = distance_df %>%
                                 dplyr::filter(as.numeric(trip_id_performed) < 50),
                               interp_method = "linear",
                               use_speeds = FALSE,
                               return_group_function = TRUE)

  # 50 trips, no warning
  expect_no_warning(
    plot(traj_2)
  )
})
test_that("plot: group layers", {

  distance_df <- rbind(
    data.frame(
      event_timestamp = as.POSIXct(c(0, 20)),
      distance = c(0, 5),
      trip_id_performed = c("a", "a")),
    data.frame(
      event_timestamp = as.POSIXct(c(10, 30)),
      distance = c(0, 5),
      trip_id_performed = c("b", "b"))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  traj <- get_trajectory_fun(distance_df = distance_df,
                             interp_method = "linear",
                             use_speeds = FALSE,
                             return_group_function = TRUE)

  p <- plot(traj)

  # class
  expect_s3_class(
    p,
    class = "ggplot2::ggplot"
  )
  # layers: 1
  expect_equal(
    length(p$layers),
    expected = 1
  )
})
test_that("plot: single layers", {

  distance_df <- rbind(
    data.frame(
      event_timestamp = as.POSIXct(c(0, 20)),
      distance = c(0, 5),
      trip_id_performed = c("a", "a")),
    data.frame(
      event_timestamp = as.POSIXct(c(10, 30)),
      distance = c(0, 5),
      trip_id_performed = c("b", "b"))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  traj <- get_trajectory_fun(distance_df = distance_df,
                             interp_method = "linear",
                             use_speeds = FALSE,
                             return_group_function = FALSE)

  p <- plot(traj[[1]])

  # class
  expect_s3_class(
    p,
    class = "ggplot2::ggplot"
  )
  # layers: 1
  expect_equal(
    length(p$layers),
    expected = 1
  )
})
