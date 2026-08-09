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
