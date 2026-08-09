# --- new_transittraj_data() ---
test_that("new_transittraj_data: steps validation", {

  # no steps
  exp <- c("filter_by_route",
           "lineE_avl",
           "get_shape_geometry",
           "get_linear_distances",
           "clean_overlapping_subtrips",
           "clean_jumps",
           "clean_incomplete_trips",
           "trim_trips",
           "make_monotonic",
           "get_trajectory_fun",
           "get_trajectory_fun_single")
  expect_equal(
    new_transittraj_data(),
    expected = exp
  )

  # invalid step
  expect_error(
    new_transittraj_data("oops"),
    class = "error_datahelper_input"
  )
})
test_that("new_transittraj_data: step-by-step", {

  gtfs <- new_transittraj_data("filter_by_route")
  avl <- new_transittraj_data("lineE_avl")

  # get_shape_geometry
  geom <- get_shape_geometry(gtfs = gtfs,
                             shape = "804EB_RC_221121",
                             project_crs = 32611)
  t_geom <- new_transittraj_data("get_shape_geometry")
  expect_equal(
    t_geom,
    expected = geom
  )

  # get_linear_distances
  distance_df <- get_linear_distances(avl_df = avl,
                                      shape_geometry = geom,
                                      clip_buffer = 50,
                                      project_crs = 32611)
  t_distance_df <- new_transittraj_data("get_linear_distances")
  expect_equal(
    t_distance_df,
    expected = distance_df
  )

  # clean_overlapping_subtrips
  cleaned_subtrips <- clean_overlapping_subtrips(distance_df = distance_df,
                                                 remove_single_observations = TRUE,
                                                 remove_non_overlapping = FALSE)
  t_cleaned_subtrips <- new_transittraj_data("clean_overlapping_subtrips")
  expect_equal(
    t_cleaned_subtrips,
    expected = cleaned_subtrips
  )

  # clean_jumps
  cleaned_jumps <- clean_jumps(distance_df = cleaned_subtrips,
                               max_median_deviation = 80,
                               min_median_deviation = -80,
                               t_cutoff = Inf)
  t_cleaned_jumps <- new_transittraj_data("clean_jumps")
  expect_equal(
    t_cleaned_jumps,
    expected = cleaned_jumps
  )

  # clean_incomplete_trips
  clean_trips <- clean_incomplete_trips(distance_df = cleaned_jumps,
                                        min_trip_distance = 100,
                                        min_trip_duration = 120,
                                        max_distance_gap = 1000)
  t_clean_trips <- new_transittraj_data("clean_incomplete_trips")
  expect_equal(
    t_clean_trips,
    expected = clean_trips
  )

  # trim_trips
  trimmed_trips <- trim_trips(distance_df = clean_trips,
                              trim_type = "both")
  t_trimmed_trips <- new_transittraj_data("trim_trips")
  expect_equal(
    t_trimmed_trips,
    expected = trimmed_trips
  )

  # make_monotonic
  mono_df <- make_monotonic(distance_df = trimmed_trips,
                            correct_speed = TRUE,
                            add_distance_error = 0.001)
  t_mono_df <- new_transittraj_data("make_monotonic")
  expect_equal(
    t_mono_df,
    expected = mono_df
  )

  # get_trajectory_fun
  traj <- get_trajectory_fun(distance_df = mono_df)
  t_traj <- new_transittraj_data("get_trajectory_fun")
  expect_equal(
    t_traj,
    expected = traj
  )

  # get_trajectory_fun_single
  traj_s <- get_trajectory_fun(distance_df = mono_df,
                               return_group_function = FALSE)
  t_traj_s <- new_transittraj_data("get_trajectory_fun_single")
  expect_equal(
    t_traj_s,
    expected = traj_s
  )
})
