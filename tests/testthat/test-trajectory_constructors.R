# --- get_trajectory_fun() ---
test_that("get_trajectory_fun: monotonicity validation", {

  distance_df <- data.frame(
    trip_id_performed = rep("a", 6),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()),
                  distance = c(0, 1, 2, 2, 2, 3),
                  speed = c(0.2, 0.2, 0, 100, 0, 0.2))

  # weak, not strict, no speeds
  expect_error(
    get_trajectory_fun(distance_df = distance_df,
                       use_speeds = FALSE),
    class = "error_tidesval_mono"
  )

  # weak, not strict, not speeds
  expect_error(
    get_trajectory_fun(distance_df = distance_df,
                       use_speeds = TRUE),
    class = "error_tidesval_mono"
  )

  # strict, not speeds
  mono_df <- make_monotonic(distance_df = distance_df,
                            correct_speed = FALSE,
                            add_distance_error = 0.01)
  expect_error(
    get_trajectory_fun(distance_df = mono_df,
                       use_speeds = TRUE),
    class = "error_tidesval_mono"
  )

  # not finding inverse
  expect_warning(
    get_trajectory_fun(distance_df = mono_df,
                       use_speeds = TRUE,
                       find_inverse_function = FALSE),
    class = "warn_tidesval_mono"
  )
})
test_that("get_trajectory_fun: input type warnings", {

  distance_df <- data.frame(
    trip_id_performed = rep("a", 7),
    distance = seq(from = 0, by = 1, length.out = 7),
    event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
    speed = rep(1, 7)
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # linear, with speeds
  expect_warning(
    get_trajectory_fun(distance_df = distance_df,
                       interp_method = "linear",
                       use_speeds = TRUE),
    class = "warn_traj_type"
  )

  # non-monoH.FC with speeds
  expect_warning(
    get_trajectory_fun(distance_df = distance_df,
                       interp_method = "fmm",
                       use_speeds = TRUE),
    class = "warn_traj_type"
  )
})
test_that("get_trajectory_fun: group object", {

  distance_df <- data.frame(
    trip_id_performed = rep("a", 7),
    distance = seq(from = 0, by = 1, length.out = 7),
    event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
    speed = rep(1, 7)
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # linear
  t <- get_trajectory_fun(distance_df = distance_df,
                          interp_method = "linear",
                          use_speeds = FALSE,
                          find_inverse_function = TRUE,
                          return_group_function = TRUE)
  t_att <- attributes(t)
  expect_identical(
    t_att$class,
    expected = "avltrajectory_group"
  )
  expect_identical(
    t_att$traj_type,
    expected = "linear"
  )
  expect_identical(
    t_att$max_deriv,
    expected = 0
  )
  expect_identical(
    t_att$used_speeds,
    expected = FALSE
  )
  expect_equal(
    t_att$min_dist,
    expected = min(distance_df$distance)
  )
  expect_equal(
    t_att$max_dist,
    expected = max(distance_df$distance)
  )
  expect_equal(
    t_att$min_time,
    expected = as.numeric(min(distance_df$event_timestamp))
  )
  expect_equal(
    t_att$max_time,
    expected = as.numeric(max(distance_df$event_timestamp))
  )
  expect_equal(
    t_att$traj_fun[[1]](3),
    expected = 3
  )
  expect_equal(
    t_att$inv_traj_fun[[1]](3),
    expected = 3
  )

  # spline monoH.FC
  t2 <- get_trajectory_fun(distance_df = distance_df,
                          interp_method = "monoH.FC",
                          use_speeds = TRUE,
                          find_inverse_function = TRUE,
                          return_group_function = TRUE)
  t2_att <- attributes(t2)
  expect_identical(
    t2_att$traj_type,
    expected = "monoH.FC"
  )
  expect_identical(
    t2_att$max_deriv,
    expected = 3
  )
  expect_identical(
    t2_att$used_speeds,
    expected = TRUE
  )
  expect_equal(
    t2_att$traj_fun[[1]](3),
    expected = 3
  )
  expect_equal(
    t2_att$traj_fun[[1]](3, deriv = 1),
    expected = 1
  )
  expect_equal(
    t2_att$inv_traj_fun[[1]](3),
    expected = 3
  )

  # spline hyman
  t3 <- get_trajectory_fun(distance_df = distance_df,
                           interp_method = "hyman",
                           use_speeds = FALSE,
                           find_inverse_function = TRUE,
                           return_group_function = TRUE)
  t3_att <- attributes(t3)
  expect_identical(
    t3_att$traj_type,
    expected = "hyman"
  )
  expect_identical(
    t3_att$max_deriv,
    expected = 3
  )
  expect_identical(
    t3_att$used_speeds,
    expected = FALSE
  )
  expect_equal(
    t3_att$traj_fun[[1]](3),
    expected = 3
  )
  expect_equal(
    t3_att$traj_fun[[1]](3, deriv = 1),
    expected = 1
  )
  expect_equal(
    t3_att$inv_traj_fun[[1]](3),
    expected = 3
  )
})
test_that("get_trajectory_fun: singles object", {

  distance_df <- data.frame(
    trip_id_performed = rep("a", 7),
    distance = seq(from = 0, by = 1, length.out = 7),
    event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
    speed = rep(1, 7)
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # spline monoH.FC
  t2 <- get_trajectory_fun(distance_df = distance_df,
                           interp_method = "monoH.FC",
                           use_speeds = TRUE,
                           find_inverse_function = TRUE,
                           return_group_function = FALSE)

  expect_identical(
    class(t2),
    expected = "list"
  )
  expect_equal(
    length(t2),
    expected = 1
  )

  t2_att <- attributes(t2[[1]])
  expect_identical(
    t2_att$class,
    expected = c("avltrajectory_single", "avltrajectory_group")
  )
  expect_identical(
    t2_att$traj_type,
    expected = "monoH.FC"
  )
  expect_identical(
    t2_att$max_deriv,
    expected = 3
  )
  expect_identical(
    t2_att$used_speeds,
    expected = TRUE
  )
  expect_equal(
    t2_att$traj_fun(3),
    expected = 3
  )
  expect_equal(
    t2_att$traj_fun(3, deriv = 1),
    expected = 1
  )
  expect_equal(
    t2_att$inv_traj_fun(3),
    expected = 3
  )

})
test_that("get_trajectory_fun: no inverse", {

  distance_df <- data.frame(
    trip_id_performed = rep("a", 7),
    distance = seq(from = 0, by = 1, length.out = 7),
    event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
    speed = rep(1, 7)
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # spline monoH.FC
  t2 <- get_trajectory_fun(distance_df = distance_df,
                           interp_method = "monoH.FC",
                           use_speeds = TRUE,
                           find_inverse_function = FALSE,
                           return_group_function = TRUE)
  t2_att <- attributes(t2)

  expect_true(
    is.null(t2_att$inv_tol)
  )
  expect_true(
    is.null(t2_att$traj_inv_functions)
  )
})

# --- get_gtfs_trajectory_fun() ---
test_that("get_gtfs_trajectory_fun: use stop time", {

  gtfs <- tidytransit::filter_feed_by_trips(
    gtfs_obj = lacmta_gtfs,
    trip_ids = "63383905"
  )
  geom <- new_transittraj_data("get_shape_geometry")

  # invalid input
  expect_error(
    get_gtfs_trajectory_fun(gtfs = gtfs,
                            shape_geometry = geom,
                            project_crs = 32611,
                            use_stop_time = "middle"),
    class = "error_gtfstraj_stoptime"
  )

  # arrival
  t_arr <- get_gtfs_trajectory_fun(gtfs = gtfs,
                               shape_geometry = geom,
                               project_crs = 32611,
                               use_stop_time = "arrival",
                               date_min = as.Date("2026-05-27"),
                               date_max = as.Date("2026-05-27"))
  t_arr_att <- attributes(t_arr)
  expect_identical(
    t_arr_att$class,
    expected = "avltrajectory_group"
  )
  expect_equal(
    length(t_arr),
    expected = 1
  )
  expect_equal(
    t_arr_att$traj_fun[[1]](as.numeric(as.POSIXct("2026-05-27 04:44:00", tz = "America/Los_Angeles"))),
    expected = unclass(sf::st_length(geom))[1],
    tolerance = 500
  )

  # departure
  t_dep <- get_gtfs_trajectory_fun(gtfs = gtfs,
                                   shape_geometry = geom,
                                   project_crs = 32611,
                                   use_stop_time = "departure",
                                   date_min = as.Date("2026-05-27"),
                                   date_max = as.Date("2026-05-27"))
  t_dep_att <- attributes(t_dep)
  expect_identical(
    t_dep_att$class,
    expected = "avltrajectory_group"
  )
  expect_equal(
    length(t_dep),
    expected = 1
  )
  expect_equal(
    t_dep_att$traj_fun[[1]](as.numeric(as.POSIXct("2026-05-27 04:44:00", tz = "America/Los_Angeles"))),
    expected = unclass(sf::st_length(geom))[1],
    tolerance = 500
  )
})
test_that("get_gtfs_trajectory_fun: added dwells", {

  gtfs <- tidytransit::filter_feed_by_trips(
    gtfs_obj = lacmta_gtfs,
    trip_ids = "63383905"
  )
  geom <- new_transittraj_data("get_shape_geometry")

  # both, no dwells
  expect_error(
    get_gtfs_trajectory_fun(gtfs = gtfs,
                            shape_geometry = geom,
                            project_crs = 32611,
                            use_stop_time = "both",
                            date_min = as.Date("2026-05-27"),
                            date_max = as.Date("2026-05-27"),
                            add_stop_dwell = 0,
                            add_distance_error = 1),
    class = "error_gtfstraj_inputdata"
  )

  # proper dwells
  t <- get_gtfs_trajectory_fun(gtfs = gtfs,
                                   shape_geometry = geom,
                                   project_crs = 32611,
                                   use_stop_time = "both",
                                   date_min = as.Date("2026-05-27"),
                                   date_max = as.Date("2026-05-27"),
                                   add_stop_dwell = 5,
                                   add_distance_error = 0.01)
  t_att <- attributes(t)
  expect_identical(
    t_att$class,
    expected = "avltrajectory_group"
  )
  expect_equal(
    length(t),
    expected = 1
  )
  expect_equal(
    t_att$traj_fun[[1]](as.numeric(as.POSIXct("2026-05-27 04:44:00", tz = "America/Los_Angeles"))),
    expected = unclass(sf::st_length(geom))[1],
    tolerance = 500
  )
  expect_equal(
    t_att$traj_fun[[1]](as.numeric(as.POSIXct("2026-05-27 04:44:00", tz = "America/Los_Angeles"))),
    expected = t_att$traj_fun[[1]](as.numeric(as.POSIXct("2026-05-27 04:44:05", tz = "America/Los_Angeles"))),
    tolerance = 1
  )
})
test_that("get_gtfs_trajectory_fun: multiple shapes", {

  # filter gtfs to sample date
  gtfs <- suppressWarnings(tidytransit::filter_feed_by_date(gtfs_obj = lacmta_gtfs,
                                           extract_date = "2026-05-27"))
  # override default service IDs, so all will pass through
  gtfs$trips <- gtfs$trips %>%
    dplyr::mutate(service_id = "RDEC25-801-1_Weekday-47")

  t <- suppressMessages(get_gtfs_trajectory_fun(gtfs = gtfs,
                                                use_stop_time = "departure",
                                                date_min = as.Date("2026-05-27"),
                                                date_max = as.Date("2026-05-27")))
  num_trips <- length(unique(gtfs$trips$trip_id))

  # total trip count
  expect_equal(
    length(unclass(t)),
    expected = num_trips
  )

  # check trip ending points - each route has different lengths
  shapes <- get_shape_geometry(gtfs = gtfs, project_crs = 32611)
  shape_lengths <- data.frame(
    shape_id = shapes$shape_id,
    len = unclass(sf::st_length(shapes))[1:4]
  )
  trip_routes <- gtfs$trips %>%
    dplyr::distinct(trip_id, shape_id) %>%
    dplyr::left_join(y = shape_lengths, by = "shape_id") %>%
    dplyr::rename(trip_id_performed = trip_id)
  t_ex <- get_trip_extremes(t) %>%
    dplyr::mutate(trip_id_performed = substr(trip_id_performed, start = 12,
                                             stop = length(trip_id_performed))) %>%
    dplyr::left_join(y = trip_routes, by = "trip_id_performed") %>%
    dplyr::group_by(shape_id) %>%
    dplyr::summarize(max_traj = max(max_dist),
                     max_shape = max(len))
  expect_equal(
    t_ex$max_traj,
    expected = t_ex$max_shape,
    tolerance = 500
  )
})

# --- group_trajectories() ---
test_that("group_trajectories: input validation", {

  distance_df <- rbind(
    data.frame(
      trip_id_performed = rep("a", 7),
      distance = seq(from = 0, by = 1, length.out = 7),
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      speed = rep(1, 7)
    ) %>%
      dplyr::mutate(location_ping_id = as.character(dplyr::row_number())),
    data.frame(
      trip_id_performed = rep("b", 7),
      distance = seq(from = 0, by = 1, length.out = 7),
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      speed = rep(1, 7)
    ) %>%
      dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))
  )
  group_traj <- get_trajectory_fun(distance_df = distance_df)
  single_traj <- get_trajectory_fun(distance_df = distance_df,
                                   return_group_function = FALSE)

  # invalid grouping
  expect_error(
    group_trajectories(trajectories = group_traj,
                       grouping = "abc"),
    class = "error_trajgrouping_input"
  )
  expect_error(
    group_trajectories(trajectories = group_traj,
                       grouping = c("abc", "123")),
    class = "error_trajgrouping_input"
  )

  # not trajectory
  expect_error(
    group_trajectories(trajectories = "a",
                       grouping = "split"),
    class = "error_trajgrouping_input"
  )
  expect_error(
    group_trajectories(trajectories = list("a", "b"),
                       grouping = "split"),
    class = "error_trajgrouping_input"
  )

  # group already grouped
  expect_error(
    group_trajectories(trajectories = group_traj,
                       grouping = "group"),
    class = "error_trajgrouping_input"
  )

  # split already split
  expect_error(
    group_trajectories(trajectories = single_traj,
                       grouping = "split"),
    class = "error_trajgrouping_input"
  )

  # if only one single
  expect_error(
    group_trajectories(trajectories = list(single_traj[[1]]),
                       grouping = "split"),
    class = "error_trajgrouping_input"
  )
})
test_that("group_trajectories: grouping validation", {

  distance_df <- rbind(
    data.frame(
      trip_id_performed = rep("a", 7),
      distance = seq(from = 0, by = 1, length.out = 7),
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      speed = rep(1, 7)
    ) %>%
      dplyr::mutate(location_ping_id = as.character(dplyr::row_number())),
    data.frame(
      trip_id_performed = rep("b", 7),
      distance = seq(from = 0, by = 1, length.out = 7),
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      speed = rep(1, 7)
    ) %>%
      dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))
  )

  # multiple same
  t1 <- get_trajectory_fun(distance_df = distance_df %>% dplyr::filter(trip_id_performed == "a"),
                           return_group_function = FALSE)
  expect_error(
    group_trajectories(trajectories = list(t1[[1]], t1[[1]]),
                       grouping = "group"),
    class = "error_trajgrouping_dup"
  )

  # diff type
  t2a <- get_trajectory_fun(distance_df = distance_df %>% dplyr::filter(trip_id_performed == "a"),
                           return_group_function = FALSE,
                           interp_method = "monoH.FC")
  t2b <- get_trajectory_fun(distance_df = distance_df %>% dplyr::filter(trip_id_performed == "b"),
                            return_group_function = FALSE,
                            interp_method = "linear",
                            use_speeds = FALSE)
  expect_error(
    group_trajectories(trajectories = list(t2a[[1]], t2b[[1]]),
                       grouping = "group"),
    class = "error_trajgrouping_constants"
  )

  # diff inverse tolerance
  t3a <- get_trajectory_fun(distance_df = distance_df %>% dplyr::filter(trip_id_performed == "a"),
                            return_group_function = FALSE,
                            inv_tol = 0.01)
  t3b <- get_trajectory_fun(distance_df = distance_df %>% dplyr::filter(trip_id_performed == "b"),
                            return_group_function = FALSE,
                            inv_tol = 1)
  expect_error(
    group_trajectories(trajectories = list(t3a[[1]], t3b[[1]]),
                       grouping = "group"),
    class = "error_trajgrouping_constants"
  )
})
test_that("group_trajectories: splitting", {

  distance_df <- rbind(
    data.frame(
      trip_id_performed = rep("a", 7),
      distance = seq(from = 0, by = 1, length.out = 7),
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      speed = rep(1, 7)
    ) %>%
      dplyr::mutate(location_ping_id = as.character(dplyr::row_number())),
    data.frame(
      trip_id_performed = rep("b", 7),
      distance = seq(from = 0, by = 1, length.out = 7),
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      speed = rep(1, 7)
    ) %>%
      dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))
  )
  group_traj <- get_trajectory_fun(distance_df = distance_df)
  single_traj <- get_trajectory_fun(distance_df = distance_df,
                                    return_group_function = FALSE)

  split_traj <- group_trajectories(trajectories = group_traj,
                                   grouping = "split")

  expect_equal(
    split_traj,
    expected = single_traj
  )
})
test_that("group_trajectories: grouping", {

  distance_df <- rbind(
    data.frame(
      trip_id_performed = rep("a", 7),
      distance = seq(from = 0, by = 1, length.out = 7),
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      speed = rep(1, 7)
    ) %>%
      dplyr::mutate(location_ping_id = as.character(dplyr::row_number())),
    data.frame(
      trip_id_performed = rep("b", 7),
      distance = seq(from = 0, by = 1, length.out = 7),
      event_timestamp = as.POSIXct(seq(from = 0, by = 1, length.out = 7)),
      speed = rep(1, 7)
    ) %>%
      dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))
  )
  group_traj <- get_trajectory_fun(distance_df = distance_df)
  single_traj <- get_trajectory_fun(distance_df = distance_df,
                                    return_group_function = FALSE)

  combo_traj <- group_trajectories(trajectories = single_traj,
                                   grouping = "group")

  expect_equal(
    combo_traj,
    expected = group_traj
  )
})

