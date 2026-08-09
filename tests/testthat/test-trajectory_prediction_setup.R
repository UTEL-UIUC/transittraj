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

  lineE_traj <- new_transittraj_data("get_trajectory_fun")

  # Correct range
  expect_error(
    predict(object = lineE_traj,
            distance_lims = c(50000, 50200),
            timestep = 10),
    class = "error_trajpredict_lims"
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
