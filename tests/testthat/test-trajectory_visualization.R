# --- plot_trajectory() ---
test_that("plot_trajectory: label validation", {

  # data setup & validation already checked in helpers
  # main goal is to check labeling

  mono_df <- new_transittraj_data("make_monotonic")
  traj <- get_trajectory_fun(mono_df)
  feat_df <- data.frame(name = c("a"),
                        distance = c(10000))

  # label input val
  expect_error(
    plot_trajectory(trajectory = traj,
                    feature_distances = feat_df,
                    label_field = "missing"),
    class = "error_plottraj_labels"
  )
  expect_error(
    plot_trajectory(trajectory = traj,
                    feature_distances = feat_df,
                    label_field = "name",
                    label_pos = "upside down"),
    class = "error_plottraj_labels"
  )
})
test_that("plot_trajectory: plot layers", {

  mono_df <- new_transittraj_data("make_monotonic")
  traj <- get_trajectory_fun(mono_df)
  feat_df <- data.frame(name = c("a"),
                        distance = c(10000))

  # OK label & features
  p_1 <- plot_trajectory(trajectory = traj,
                         feature_distances = feat_df,
                         label_field = "name")
  # class
  expect_s3_class(
    p_1,
    class = "ggplot2::ggplot"
  )
  # layers: traj, feature line, feature label
  expect_equal(
    length(p_1$layers),
    expected = 3
  )

  # no features
  p_2 <- plot_trajectory(trajectory = traj)
  # class
  expect_s3_class(
    p_2,
    class = "ggplot2::ggplot"
  )
  # layers: traj
  expect_equal(
    length(p_2$layers),
    expected = 1
  )
})




















#
