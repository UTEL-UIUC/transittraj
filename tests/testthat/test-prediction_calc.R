# --- interpolate_distances() ---
test_that("interpolate_distances: single", {

  # build DF
  distance_df <- rbind(
    data.frame(
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      distance = seq(from = 0, by = 1, length.out = 7),
      speed = rep(1, 7),
      trip_id_performed = rep("a", 7)
    ),
    data.frame(
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      distance = seq(from = 0, by = 1, length.out = 7),
      speed = rep(1, 7),
      trip_id_performed = rep("b", 7)
    )
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # build traj
  traj_H <- get_trajectory_fun(distance_df,
                               interp_method = "monoH.FC",
                               return_group_function = FALSE)
  traj_l <- get_trajectory_fun(distance_df,
                               interp_method = "linear",
                               use_speeds = FALSE,
                               return_group_function = FALSE)

  # df to interp over
  new_times_0 <- data.frame(
    event_timestamp = distance_df %>%
      dplyr::filter(trip_id_performed == "a") %>%
      dplyr::mutate(event_timestamp = as.numeric(event_timestamp)) %>%
      dplyr::pull(event_timestamp),
    trip_id_performed = distance_df %>%
      dplyr::filter(trip_id_performed == "a") %>%
      dplyr::pull(trip_id_performed),
    deriv = rep(0, 7)
  )
  new_times_1 <- new_times_0 %>%
    dplyr::mutate(deriv = 1)

  # interp
  interp_H_0 <- interpolate_distances(
    trajectory = traj_H[[1]],
    new_times_trips = new_times_0
  )
  interp_H_1 <- interpolate_distances(
    trajectory = traj_H[[1]],
    new_times_trips = new_times_1
  )
  interp_l <- interpolate_distances(
    trajectory = traj_l[[1]],
    new_times_trips = new_times_0
  )

  # check results
  expect_equal(
    interp_H_0$interp,
    expected = distance_df %>%
      dplyr::filter(trip_id_performed == "a") %>%
      dplyr::pull(distance)
  )
  expect_equal(
    interp_H_1$interp,
    expected = distance_df %>%
      dplyr::filter(trip_id_performed == "a") %>%
      dplyr::pull(speed)
  )
  expect_equal(
    interp_l$interp,
    expected = distance_df %>%
      dplyr::filter(trip_id_performed == "a") %>%
      dplyr::pull(distance)
  )
})
test_that("interpolate_distances: group", {

  # build DF
  distance_df <- rbind(
    data.frame(
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      distance = seq(from = 0, by = 1, length.out = 7),
      speed = rep(1, 7),
      trip_id_performed = rep("a", 7)
    ),
    data.frame(
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      distance = seq(from = 0, by = 1, length.out = 7),
      speed = rep(1, 7),
      trip_id_performed = rep("b", 7)
    )
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # build traj
  traj_H <- get_trajectory_fun(distance_df,
                               interp_method = "monoH.FC",
                               return_group_function = TRUE)
  traj_l <- get_trajectory_fun(distance_df,
                               interp_method = "linear",
                               use_speeds = FALSE,
                               return_group_function = TRUE)

  # df to interp over
  new_times_0 <- distance_df %>%
    dplyr::mutate(event_timestamp = as.numeric(event_timestamp),
                  deriv = 0) %>%
    dplyr::select(-c(speed, location_ping_id, distance))
  new_times_1 <- new_times_0 %>%
    dplyr::mutate(deriv = 1)

  # interp
  interp_H_0 <- interpolate_distances(
    trajectory = traj_H,
    new_times_trips = new_times_0
  )
  interp_H_1 <- interpolate_distances(
    trajectory = traj_H,
    new_times_trips = new_times_1
  )
  interp_l <- interpolate_distances(
    trajectory = traj_l,
    new_times_trips = new_times_0
  )

  # check results
  expect_equal(
    interp_H_0$interp,
    expected = distance_df %>%
      dplyr::pull(distance)
  )
  expect_equal(
    interp_H_1$interp,
    expected = distance_df %>%
      dplyr::pull(speed)
  )
  expect_equal(
    interp_l$interp,
    expected = distance_df %>%
      dplyr::pull(distance)
  )
})

# --- interpolate_times() ---
test_that("interpolate_times: single", {

  # build DF
  distance_df <- rbind(
    data.frame(
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      distance = seq(from = 0, by = 1, length.out = 7),
      speed = rep(1, 7),
      trip_id_performed = rep("a", 7)
    ),
    data.frame(
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      distance = seq(from = 0, by = 1, length.out = 7),
      speed = rep(1, 7),
      trip_id_performed = rep("b", 7)
    )
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # build traj
  traj_H <- get_trajectory_fun(distance_df,
                               interp_method = "monoH.FC",
                               return_group_function = FALSE)
  traj_l <- get_trajectory_fun(distance_df,
                               interp_method = "linear",
                               use_speeds = FALSE,
                               return_group_function = FALSE)

  # df to interp over
  new_dists <- data.frame(
    distance = distance_df %>%
      dplyr::filter(trip_id_performed == "a") %>%
      dplyr::pull(distance),
    trip_id_performed = distance_df %>%
      dplyr::filter(trip_id_performed == "a") %>%
      dplyr::pull(trip_id_performed)
  )

  # interp
  interp_H <- interpolate_times(
    trajectory = traj_H[[1]],
    new_dist_trips = new_dists
  )
  interp_l <- interpolate_times(
    trajectory = traj_l[[1]],
    new_dist_trips = new_dists
  )

  # check results
  expect_equal(
    interp_H$interp,
    expected = distance_df %>%
      dplyr::filter(trip_id_performed == "a") %>%
      dplyr::mutate(event_timestamp = as.numeric(event_timestamp)) %>%
      dplyr::pull(event_timestamp)
  )
  expect_equal(
    interp_l$interp,
    expected = distance_df %>%
      dplyr::filter(trip_id_performed == "a") %>%
      dplyr::mutate(event_timestamp = as.numeric(event_timestamp)) %>%
      dplyr::pull(event_timestamp)
  )
})
test_that("interpolate_times: single", {

  # build DF
  distance_df <- rbind(
    data.frame(
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      distance = seq(from = 0, by = 1, length.out = 7),
      speed = rep(1, 7),
      trip_id_performed = rep("a", 7)
    ),
    data.frame(
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      distance = seq(from = 0, by = 1, length.out = 7),
      speed = rep(1, 7),
      trip_id_performed = rep("b", 7)
    )
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # build traj
  traj_H <- get_trajectory_fun(distance_df,
                               interp_method = "monoH.FC",
                               return_group_function = TRUE)
  traj_l <- get_trajectory_fun(distance_df,
                               interp_method = "linear",
                               use_speeds = FALSE,
                               return_group_function = TRUE)

  # df to interp over
  new_dists <- distance_df %>%
    dplyr::select(trip_id_performed, distance)

  # interp
  interp_H <- interpolate_times(
    trajectory = traj_H,
    new_dist_trips = new_dists
  )
  interp_l <- interpolate_times(
    trajectory = traj_l,
    new_dist_trips = new_dists
  )

  # check results
  expect_equal(
    interp_H$interp,
    expected = distance_df %>%
      dplyr::mutate(event_timestamp = as.numeric(event_timestamp)) %>%
      dplyr::pull(event_timestamp)
  )
  expect_equal(
    interp_l$interp,
    expected = distance_df %>%
      dplyr::mutate(event_timestamp = as.numeric(event_timestamp)) %>%
      dplyr::pull(event_timestamp)
  )
})

