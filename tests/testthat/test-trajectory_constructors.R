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
