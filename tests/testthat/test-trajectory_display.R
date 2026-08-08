# --- summary() ---
test_that("summary: group", {
#
#   distance_df <- rbind(
#     data.frame(
#       event_timestamp = as.POSIXct(c(0, 10)),
#       distance = c(0, 5),
#       trip_id_performed = c("a", "a")),
#     data.frame(
#       event_timestamp = as.POSIXct(c(0, 10)),
#       distance = c(0, 5),
#       trip_id_performed = c("b", "b"))
#   ) %>%
#     dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))
#
#   traj <- get_trajectory_fun(distance_df = distance_df,
#                              interp_method = "linear",
#                              use_speeds = FALSE,
#                              return_group_function = FALSE)
#
#   exp_summ <- "------
# AVL Group Trajectory Object
# ------
# Number of trips: 2
# Total distance range: 0 to 5
# Total time range: 0 to 10
# ------
# Trajectory function present: TRUE
#    --> Trajectory interpolation method: linear
#    --> Maximum derivative: 0
#    --> Fit with speeds: FALSE
# Inverse function present: TRUE
#    --> Inverse function tolerance: 0.01
# ------"
#   actual_summ <- summary(traj)
#
#   expect_equal(
#     actual_summ,
#     expected = exp_summ
#   )

})
