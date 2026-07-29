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

# --- plot_animated_line() ---
test_that("plot_animated_line: label validation", {

  mono_df <- new_transittraj_data("make_monotonic")
  traj <- get_trajectory_fun(mono_df)
  feat_df <- data.frame(name = c("a"),
                        distance = c(10000))

  # label input val
  expect_error(
    plot_animated_line(trajectory = traj,
                    feature_distances = feat_df,
                    label_field = "missing"),
    class = "error_plottraj_labels"
  )
  expect_error(
    plot_animated_line(trajectory = traj,
                    feature_distances = feat_df,
                    label_field = "name",
                    label_pos = "upside down"),
    class = "error_plottraj_labels"
  )
})
test_that("plot_animated_line: plot layers", {

  mono_df <- new_transittraj_data("make_monotonic")
  traj <- get_trajectory_fun(mono_df)
  feat_df <- data.frame(name = c("a"),
                        distance = c(10000))

  # OK label & features
  p_1 <- plot_animated_line(trajectory = traj,
                         feature_distances = feat_df,
                         label_field = "name")
  # class
  expect_s3_class(
    p_1,
    class = "gganim"
  )
  # layers: route, feature line, feature label, vehicles
  expect_equal(
    length(p_1$layers),
    expected = 4
  )
  # distance lims
  expect_equal(
    min(p_1$layers[[1]]$data$distance),
    expected = min(mono_df$distance),
    tolerance = 0.01
  )
  expect_equal(
    max(p_1$layers[[1]]$data$distance),
    expected = max(mono_df$distance),
    tolerance = 0.01
  )

  # with distance lims
  test_lims <- c(5000, 15000)
  p_2 <- plot_animated_line(trajectory = traj,
                            feature_distances = feat_df,
                            label_field = "name",
                            distance_lims = test_lims)
  # distance lims
  expect_equal(
    min(p_2$layers[[1]]$data$distance),
    expected = min(test_lims),
    tolerance = 0.01
  )
  expect_equal(
    max(p_2$layers[[1]]$data$distance),
    expected = max(test_lims),
    tolerance = 0.01
  )

  # no label or features
  p_3 <- plot_animated_line(trajectory = traj)
  # layers: route, vehicles
  expect_equal(
    length(p_3$layers),
    expected = 2
  )
})

# --- plot_animated_map() ---
test_that("plot_animated_map: label validation", {

  mono_df <- new_transittraj_data("make_monotonic")
  traj <- get_trajectory_fun(mono_df)
  feat_df <- data.frame(name = c("a"),
                        distance = c(10000))
  geom <- new_transittraj_data("get_shape_geometry")

  expect_error(
    plot_animated_map(trajectory = traj,
                       feature_distances = feat_df,
                       label_field = "missing",
                      shape_geometry = geom),
    class = "error_plottraj_labels"
  )
  expect_error(
    plot_animated_map(trajectory = traj,
                       feature_distances = feat_df,
                       label_field = "name",
                       label_pos = "upside down",
                      shape_geometry = geom),
    class = "error_plottraj_labels"
  )
})
test_that("plot_animated_map: plot layers", {

  mono_df <- new_transittraj_data("make_monotonic")
  traj <- get_trajectory_fun(mono_df)
  feat_df <- data.frame(name = c("a"),
                        distance = c(10000))
  geom <- new_transittraj_data("get_shape_geometry")

  # OK label & features
  p_1 <- plot_animated_map(trajectory = traj,
                            feature_distances = feat_df,
                            label_field = "name",
                           shape_geometry = geom,
                           background_zoom = -3)
  # class
  expect_s3_class(
    p_1,
    class = "gganim"
  )
  # layers: basemap, 2xroute, feature point, feature label, vehicles
  expect_equal(
    length(p_1$layers),
    expected = 6
  )

  # no label & features
  p_2 <- plot_animated_map(trajectory = traj,
                           shape_geometry = geom)
  # class
  expect_s3_class(
    p_2,
    class = "gganim"
  )
  # layers: basemap, 2xroute, feature point, feature label, vehicles
  expect_equal(
    length(p_2$layers),
    expected = 4
  )
})

# --- export_animation() ---
test_that("export_animation: name validation", {

  expect_error(
    export_animation(anim_object = "a",
                     path = "incorrect ending"),
    class = "error_trajplot_inputdata"
  )
})



















#
