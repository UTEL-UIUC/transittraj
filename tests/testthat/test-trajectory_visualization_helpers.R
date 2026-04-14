test_that("plot_traj_input_validation: input validation", {

  c53_mono <- new_transittraj_data("make_monotonic")
  c53_traj <- get_trajectory_fun(distance_df = c53_mono,
                                 find_inverse_function = TRUE)
  c53_traj_noinv <- get_trajectory_fun(distance_df = c53_mono,
                                       find_inverse_function = FALSE)

  # --- General ---
  # No inputs
  expect_error(
    plot_trajectory(),
    class = "error_plottraj_input"
  )
  # Too many inputs
  expect_error(
    plot_trajectory(trajectory = c53_traj,
                    distance_df = c53_mono),
    class = "error_plottraj_input"
  )

  # --- Traj ---
  # Wrong input: traj
  expect_error(
    plot_trajectory(trajectory = c53_mono),
    class = "error_plottraj_input"
  )
  # No inv
  expect_message(
    plot_trajectory(trajectory = c53_traj_noinv),
    class = "inform_plottraj_input"
  )

  # --- Dist ---
  # Wrong input: dist
  expect_error(
    plot_trajectory(distance_df = c53_traj),
    class = "error_plottraj_input"
  )
  # Missing event_timestamp
  expect_error(
    plot_trajectory(distance_df = (c53_mono %>% dplyr::select(-event_timestamp))),
    class = "error_plottraj_input"
  )
  # Missing trip_id_performed
  expect_error(
    plot_trajectory(distance_df = (c53_mono %>% dplyr::select(-distance))),
    class = "error_plottraj_input"
  )
  # Missing distance
  expect_error(
    plot_trajectory(distance_df = (c53_mono %>% dplyr::select(-distance))),
    class = "error_plottraj_input"
  )
  # Data type distance
  # Missing distance
  expect_error(
    plot_trajectory(distance_df = (c53_mono %>%
                                     dplyr::mutate(distance = as.character(distance)))),
    class = "error_plottraj_input"
  )
})

test_that("plot_traj_df_setup: range validation", {

  c53_mono <- new_transittraj_data("make_monotonic")
  c53_traj_noinv <- get_trajectory_fun(distance_df = c53_mono,
                                       find_inverse_function = FALSE)

  # wrong distance lims w/ non-inverse
  # bad lims w/ inverse will be caught by predict() validators
  expect_error(
    suppressMessages(plot_trajectory(trajectory = c53_traj_noinv,
                                     distance_lims = c(50000, 50200))),
    class = "error_plottraj_input"
  )
})

test_that("plot_trips_df_setup: range validation", {

  # problems w/ traj object will be caught by plot_traj_df_setup,
  # so will only check distance_df here
  c53_mono <- new_transittraj_data("make_monotonic")

  # bad distance range
  expect_error(
    plot_trajectory(distance_df = c53_mono,
                    distance_lims = c(50000, 50200)),
    class = "error_plottraj_inputdata"
  )

  # bad trips
  expect_error(
    plot_trajectory(distance_df = c53_mono,
                    plot_trips = c("a", "b")),
    class = "error_plottraj_inputdata"
  )
})
