test_that("predict_traj_input_setup: input combo validation", {

  c53_traj <- new_transittraj_data("get_trajectory_fun")

  # new dist & new times
  expect_error(
    predict(object = c53_traj,
            new_distances = c(0, 1),
            new_times = c(0, 1)),
    class = "error_trajpredict_input"
  )
  # new dist or times w/ dist lims & timestep
  expect_error(
    predict(object = c53_traj,
            new_distances = c(0, 1),
            distance_lims = c(0, 1),
            timestep = 1),
    class = "error_trajpredict_input"
  )
  # timestep w/out distance_lims
  expect_error(
    predict(object = c53_traj,
            timestep = 1),
    class = "error_trajpredict_input"
  )
  # distance_lims w/out timestep
  expect_error(
    predict(object = c53_traj,
            distance_lims = 1),
    class = "error_trajpredict_input"
  )
  # nothing
  expect_error(
    predict(object = c53_traj),
    class = "error_trajpredict_input"
  )
})
