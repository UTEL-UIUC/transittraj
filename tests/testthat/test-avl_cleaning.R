# --- get_linear_distances() ---
test_that("get_linear_distances: test clipped outputs", {

  avl_df <- new_transittraj_data("lineE_avl")
  geom <- new_transittraj_data("get_shape_geometry")

  # clip
  df_2 <- get_linear_distances(avl_df = avl_df,
                               shape_geometry = geom,
                               project_crs = 32611,
                               clip_buffer = 20)
  expect_s3_class(
    df_2,
    class = "data.frame"
  )
  expect_true(
    "distance" %in% names(df_2)
  )
  expect_type(
    df_2$distance,
    type = "double"
  )
  expect_true(
    dim(df_2)[1] < dim(avl_df)[1]
  )
  expect_equal(
    max(df_2$distance),
    expected = as.numeric(sf::st_length(geom)),
    tolerance = 200
  )
  expect_equal(
    min(df_2$distance),
    expected = 0,
    tolerance = 200
  )
})
test_that("get_linear_distances: test unclipped outputs", {

  avl_df <- new_transittraj_data("lineE_avl")
  geom <- new_transittraj_data("get_shape_geometry")

  # no clip
  df_1 <- get_linear_distances(avl_df = avl_df,
                               shape_geometry = geom,
                               project_crs = 32611)
  expect_s3_class(
    df_1,
    class = "data.frame"
  )
  expect_true(
    "distance" %in% names(df_1)
  )
  expect_type(
    df_1$distance,
    type = "double"
  )
  expect_equal(
    dim(df_1)[1],
    expected = dim(avl_df)[1]
  )
  expect_equal(
    max(df_1$distance),
    expected = as.numeric(sf::st_length(geom)),
    tolerance = 200
  )
  expect_equal(
    min(df_1$distance),
    expected = 0,
    tolerance = 200
  )
})

# --- clean_overlapping_subtrips() ---
test_that("clean_overlapping_subtrips: missing operator", {

  distance_df <- data.frame(
    trip_id_performed = c("a", "a", "a", "a"),
    vehicle_id = c("a", "a", "b", "b"),
    # operator_id = c("a", "a", "b", "b"),
    distance = c(100, 500, 450, 1000)
  )

  expect_error(
    clean_overlapping_subtrips(distance_df,
                               check_operator = TRUE),
    class = "error_tidesval_missing_fields"
  )
})
test_that("clean_overlapping_subtrips: overlap with operator", {

  # overlap veh & op
  distance_df <- data.frame(
    trip_id_performed = c("a", "a", "a", "a"),
    vehicle_id = c("a", "a", "b", "b"),
    operator_id = c("a", "a", "b", "b"),
    distance = c(100, 500, 450, 1000),
    event_timestamp = as.POSIXct(c(10, 20, 15, 30))
  )
  t <- clean_overlapping_subtrips(distance_df = distance_df,
                                  check_operator = TRUE,
                                  remove_non_overlapping = FALSE)
  r <- clean_overlapping_subtrips(distance_df = distance_df,
                                  check_operator = TRUE,
                                  remove_non_overlapping = FALSE,
                                  return_removals = TRUE)
  expect_equal(
    dim(t)[1],
    expected = 0
  )
  expect_equal(
    dim(r)[1],
    expected = 1
  )
  expect_equal(
    r$reason[1],
    expected = "overlapping subtrips"
  )
  t2 <- clean_overlapping_subtrips(distance_df = distance_df,
                                  check_operator = TRUE,
                                  remove_non_overlapping = TRUE)
  r2 <- clean_overlapping_subtrips(distance_df = distance_df,
                                   check_operator = TRUE,
                                   remove_non_overlapping = TRUE,
                                   return_removals = TRUE)
  expect_equal(
    dim(t2)[1],
    expected = 0
  )
  expect_equal(
    dim(r2)[1],
    expected = 1
  )
  expect_equal(
    r2$reason,
    expected = "multiple operators or vehicles"
  )

  # overlap veh only
  distance_df2 <- data.frame(
    trip_id_performed = c("a", "a", "a", "a"),
    vehicle_id = c("a", "a", "b", "b"),
    operator_id = c("a", "a", "a", "a"),
    distance = c(100, 500, 450, 1000),
    event_timestamp = as.POSIXct(c(10, 20, 15, 30))
  )
  t3 <- clean_overlapping_subtrips(distance_df = distance_df2,
                                   check_operator = TRUE,
                                   remove_non_overlapping = FALSE)
  r3 <- clean_overlapping_subtrips(distance_df = distance_df2,
                                   check_operator = TRUE,
                                   remove_non_overlapping = FALSE,
                                   return_removals = TRUE)
  expect_equal(
    dim(t3)[1],
    0
  )
  expect_equal(
    dim(r3)[1],
    expected = 1
  )
  expect_equal(
    r3$reason[1],
    expected = "overlapping subtrips"
  )
  t4 <- clean_overlapping_subtrips(distance_df = distance_df2,
                                   check_operator = TRUE,
                                   remove_non_overlapping = TRUE)
  r4 <- clean_overlapping_subtrips(distance_df = distance_df2,
                                   check_operator = TRUE,
                                   remove_non_overlapping = TRUE,
                                   return_removals = TRUE)
  expect_equal(
    dim(t4)[1],
    0
  )
  expect_equal(
    dim(r4)[1],
    expected = 1
  )
  expect_equal(
    r4$reason[1],
    expected = "multiple operators or vehicles"
  )

  # overlap op only
  distance_df3 <- data.frame(
    trip_id_performed = c("a", "a", "a", "a"),
    vehicle_id = c("a", "a", "a", "a"),
    operator_id = c("a", "a", "b", "b"),
    distance = c(100, 500, 450, 1000),
    event_timestamp = as.POSIXct(c(10, 20, 15, 30))
  )
  t5 <- clean_overlapping_subtrips(distance_df = distance_df3,
                                   check_operator = TRUE,
                                   remove_non_overlapping = FALSE)
  r5 <- clean_overlapping_subtrips(distance_df = distance_df3,
                                   check_operator = TRUE,
                                   remove_non_overlapping = FALSE,
                                   return_removals = TRUE)
  expect_equal(
    dim(t5)[1],
    0
  )
  expect_equal(
    dim(r5)[1],
    expected = 1
  )
  expect_equal(
    r5$reason[1],
    expected = "overlapping subtrips"
  )
  t6 <- clean_overlapping_subtrips(distance_df = distance_df3,
                                   check_operator = TRUE,
                                   remove_non_overlapping = TRUE)
  r6 <- clean_overlapping_subtrips(distance_df = distance_df3,
                                   check_operator = TRUE,
                                   remove_non_overlapping = TRUE,
                                   return_removals = TRUE)
  expect_equal(
    dim(t6)[1],
    0
  )
  expect_equal(
    dim(r6)[1],
    expected = 1
  )
  expect_equal(
    r6$reason[1],
    expected = "multiple operators or vehicles"
  )

  # no overlap
  distance_df4 <- data.frame(
    trip_id_performed = c("a", "a", "a", "a"),
    vehicle_id = c("a", "a", "b", "b"),
    operator_id = c("a", "a", "b", "b"),
    distance = c(100, 500, 550, 1000),
    event_timestamp = as.POSIXct(c(10, 20, 25, 30))
  )
  t7 <- clean_overlapping_subtrips(distance_df = distance_df4,
                                   check_operator = TRUE,
                                   remove_non_overlapping = FALSE)
  r7 <- clean_overlapping_subtrips(distance_df = distance_df4,
                                   check_operator = TRUE,
                                   remove_non_overlapping = FALSE,
                                   return_removals = TRUE)
  expect_equal(
    t7,
    expected = distance_df4
  )
  expect_equal(
    dim(r7)[1],
    expected = 0
  )
  t8 <- clean_overlapping_subtrips(distance_df = distance_df4,
                                   check_operator = TRUE,
                                   remove_non_overlapping = TRUE)
  r8 <- clean_overlapping_subtrips(distance_df = distance_df4,
                                   check_operator = TRUE,
                                   remove_non_overlapping = TRUE,
                                   return_removals = TRUE)
  expect_equal(
    dim(t8)[1],
    expected = 0
  )
  expect_equal(
    dim(r8)[1],
    expected = 1
  )
  expect_equal(
    r8$reason[1],
    expected = "multiple operators or vehicles"
  )
})
test_that("clean_overlapping_subtrips: overlap no operator", {

  # overlap veh
  distance_df <- data.frame(
    trip_id_performed = c("a", "a", "a", "a"),
    vehicle_id = c("a", "a", "b", "b"),
    # operator_id = c("a", "a", "b", "b"),
    distance = c(100, 500, 450, 1000),
    event_timestamp = as.POSIXct(c(10, 20, 15, 30))
  )
  t <- clean_overlapping_subtrips(distance_df = distance_df,
                                  check_operator = FALSE,
                                  remove_non_overlapping = FALSE)
  r <- clean_overlapping_subtrips(distance_df = distance_df,
                                  check_operator = FALSE,
                                  remove_non_overlapping = FALSE,
                                  return_removals = TRUE)
  expect_equal(
    dim(t)[1],
    expected = 0
  )
  expect_equal(
    dim(r)[1],
    expected = 1
  )
  expect_equal(
    r$reason[1],
    expected = "overlapping subtrips"
  )
  t2 <- clean_overlapping_subtrips(distance_df = distance_df,
                                   check_operator = FALSE,
                                   remove_non_overlapping = TRUE)
  r2 <- clean_overlapping_subtrips(distance_df = distance_df,
                                   check_operator = FALSE,
                                   remove_non_overlapping = TRUE,
                                   return_removals = TRUE)
  expect_equal(
    dim(t2)[1],
    expected = 0
  )
  expect_equal(
    dim(r2)[1],
    expected = 1
  )
  expect_equal(
    r2$reason[1],
    expected = "multiple operators or vehicles"
  )

  # no overlap
  distance_df4 <- data.frame(
    trip_id_performed = c("a", "a", "a", "a"),
    vehicle_id = c("a", "a", "b", "b"),
    # operator_id = c("a", "a", "b", "b"),
    distance = c(100, 500, 550, 1000),
    event_timestamp = as.POSIXct(c(10, 20, 25, 30))
  )
  t7 <- clean_overlapping_subtrips(distance_df = distance_df4,
                                   check_operator = FALSE,
                                   remove_non_overlapping = FALSE)
  r7 <- clean_overlapping_subtrips(distance_df = distance_df4,
                                   check_operator = FALSE,
                                   remove_non_overlapping = FALSE,
                                   return_removals = TRUE)
  expect_equal(
    t7,
    expected = distance_df4
  )
  expect_equal(
    dim(r7)[1],
    expected = 0
  )
  t8 <- clean_overlapping_subtrips(distance_df = distance_df4,
                                   check_operator = FALSE,
                                   remove_non_overlapping = TRUE)
  r8 <- clean_overlapping_subtrips(distance_df = distance_df4,
                                   check_operator = FALSE,
                                   remove_non_overlapping = TRUE,
                                   return_removals = TRUE)
  expect_equal(
    dim(t8)[1],
    expected = 0
  )
  expect_equal(
    dim(r8)[1],
    expected = 1
  )
  expect_equal(
    r8$reason[1],
    expected = "multiple operators or vehicles"
  )
})
test_that("clean_overlapping_subtrips: remove single obseravations", {

  # with operator, overlapping
  distance_df <- data.frame(
    trip_id_performed = c("a", "a", "a"),
    vehicle_id = c("a", "a", "b"),
    operator_id = c("a", "a", "b"),
    distance = c(100, 500, 450),
    event_timestamp = as.POSIXct(c(10, 20, 15))
  )

  t <- clean_overlapping_subtrips(distance_df = distance_df,
                                  check_operator = TRUE,
                                  remove_single_observations = TRUE,
                                  remove_non_overlapping = FALSE)
  r <- clean_overlapping_subtrips(distance_df = distance_df,
                                  check_operator = TRUE,
                                  remove_single_observations = TRUE,
                                  remove_non_overlapping = FALSE,
                                  return_removals = TRUE)
  expect_equal(
    t,
    expected = distance_df[1:2,]
  )
  expect_equal(
    dim(r)[1],
    expected = 1
  )
  expect_equal(
    r$reason[1],
    expected = "single observation"
  )
  t2 <- clean_overlapping_subtrips(distance_df = distance_df,
                                  check_operator = TRUE,
                                  remove_single_observations = FALSE,
                                  remove_non_overlapping = FALSE)
  r2 <- clean_overlapping_subtrips(distance_df = distance_df,
                                   check_operator = TRUE,
                                   remove_single_observations = FALSE,
                                   remove_non_overlapping = FALSE,
                                   return_removals = TRUE)
  expect_equal(
    t2,
    expected = distance_df
  )
  expect_equal(
    dim(r2)[1],
    expected = 0
  )
  t3 <- clean_overlapping_subtrips(distance_df = distance_df,
                                   check_operator = TRUE,
                                   remove_single_observations = TRUE,
                                   remove_non_overlapping = TRUE)
  r3 <- clean_overlapping_subtrips(distance_df = distance_df,
                                   check_operator = TRUE,
                                   remove_single_observations = TRUE,
                                   remove_non_overlapping = TRUE,
                                   return_removals = TRUE)
  expect_equal(
    dim(t3)[1],
    expected = 0
  )
  expect_equal(
    dim(r3)[1],
    expected = 1
  )
  expect_equal(
    r3$reason[1],
    expected = "multiple operators or vehicles"
  )

  # with operator, non-overlapping
  distance_df2 <- data.frame(
    trip_id_performed = c("a", "a", "a"),
    vehicle_id = c("a", "a", "b"),
    operator_id = c("a", "a", "b"),
    distance = c(100, 500, 550),
    event_timestamp = as.POSIXct(c(10, 20, 25))
  )

  t4 <- clean_overlapping_subtrips(distance_df = distance_df2,
                                  check_operator = TRUE,
                                  remove_single_observations = TRUE,
                                  remove_non_overlapping = TRUE)
  r4 <- clean_overlapping_subtrips(distance_df = distance_df2,
                                   check_operator = TRUE,
                                   remove_single_observations = TRUE,
                                   remove_non_overlapping = TRUE,
                                   return_removals = TRUE)
  expect_equal(
    dim(t4)[1],
    expected = 0
  )
  expect_equal(
    dim(r4)[1],
    expected = 1
  )
  expect_equal(
    r4$reason[1],
    expected = "multiple operators or vehicles"
  )
  t5 <- clean_overlapping_subtrips(distance_df = distance_df2,
                                   check_operator = TRUE,
                                   remove_single_observations = TRUE,
                                   remove_non_overlapping = FALSE)
  r5 <- clean_overlapping_subtrips(distance_df = distance_df2,
                                   check_operator = TRUE,
                                   remove_single_observations = TRUE,
                                   remove_non_overlapping = FALSE,
                                   return_removals = TRUE)
  expect_equal(
    t5,
    expected = distance_df2[1:2,]
  )
  expect_equal(
    dim(r5)[1],
    expected = 1
  )
  expect_equal(
    r5$reason,
    expected = "single observation"
  )
  t6 <- clean_overlapping_subtrips(distance_df = distance_df2,
                                   check_operator = TRUE,
                                   remove_single_observations = FALSE,
                                   remove_non_overlapping = FALSE)
  r6 <- clean_overlapping_subtrips(distance_df = distance_df2,
                                   check_operator = TRUE,
                                   remove_single_observations = FALSE,
                                   remove_non_overlapping = FALSE,
                                   return_removals = TRUE)
  expect_equal(
    t6,
    expected = distance_df2
  )
  expect_equal(
    dim(r6)[1],
    expected = 0
  )

  # no operator, overlapping
  distance_df3 <- data.frame(
    trip_id_performed = c("a", "a", "a"),
    vehicle_id = c("a", "a", "b"),
    # operator_id = c("a", "a", "b"),
    distance = c(100, 500, 450),
    event_timestamp = as.POSIXct(c(10, 20, 15))
  )

  t7 <- clean_overlapping_subtrips(distance_df = distance_df3,
                                  check_operator = FALSE,
                                  remove_single_observations = TRUE,
                                  remove_non_overlapping = FALSE)
  r7 <- clean_overlapping_subtrips(distance_df = distance_df3,
                                   check_operator = FALSE,
                                   remove_single_observations = TRUE,
                                   remove_non_overlapping = FALSE,
                                   return_removals = TRUE)
  expect_equal(
    t7,
    expected = distance_df3[1:2,]
  )
  expect_equal(
    dim(r7)[1],
    expected = 1
  )
  expect_equal(
    r7$reason[1],
    expected = "single observation"
  )
  t8 <- clean_overlapping_subtrips(distance_df = distance_df3,
                                   check_operator = FALSE,
                                   remove_single_observations = FALSE,
                                   remove_non_overlapping = FALSE)
  r8 <- clean_overlapping_subtrips(distance_df = distance_df3,
                                   check_operator = FALSE,
                                   remove_single_observations = FALSE,
                                   remove_non_overlapping = FALSE,
                                   return_removals = TRUE)
  expect_equal(
    t8,
    expected = distance_df3
  )
  expect_equal(
    dim(r8)[1],
    expected = 0
  )
  t9 <- clean_overlapping_subtrips(distance_df = distance_df3,
                                   check_operator = FALSE,
                                   remove_single_observations = TRUE,
                                   remove_non_overlapping = TRUE)
  r9 <- clean_overlapping_subtrips(distance_df = distance_df3,
                                   check_operator = FALSE,
                                   remove_single_observations = TRUE,
                                   remove_non_overlapping = TRUE,
                                   return_removals = TRUE)
  expect_equal(
    dim(t9)[1],
    expected = 0
  )
  expect_equal(
    dim(r9)[1],
    expected = 1
  )
  expect_equal(
    r9$reason[1],
    expected = "multiple operators or vehicles"
  )

  # no operator, non-overlapping
  distance_df4 <- data.frame(
    trip_id_performed = c("a", "a", "a"),
    vehicle_id = c("a", "a", "b"),
    # operator_id = c("a", "a", "b"),
    distance = c(100, 500, 550),
    event_timestamp = as.POSIXct(c(10, 20, 25))
  )

  t10 <- clean_overlapping_subtrips(distance_df = distance_df4,
                                   check_operator = FALSE,
                                   remove_single_observations = TRUE,
                                   remove_non_overlapping = TRUE)
  r10 <- clean_overlapping_subtrips(distance_df = distance_df4,
                                    check_operator = FALSE,
                                    remove_single_observations = TRUE,
                                    remove_non_overlapping = TRUE,
                                    return_removals = TRUE)
  expect_equal(
    dim(t10)[1],
    expected = 0
  )
  expect_equal(
    dim(r10)[1],
    expected = 1
  )
  expect_equal(
    r10$reason[1],
    expected = "multiple operators or vehicles"
  )
  t11 <- clean_overlapping_subtrips(distance_df = distance_df4,
                                   check_operator = FALSE,
                                   remove_single_observations = TRUE,
                                   remove_non_overlapping = FALSE)
  r11 <- clean_overlapping_subtrips(distance_df = distance_df4,
                                    check_operator = FALSE,
                                    remove_single_observations = TRUE,
                                    remove_non_overlapping = FALSE,
                                    return_removals = TRUE)
  expect_equal(
    t11,
    expected = distance_df4[1:2,]
  )
  expect_equal(
    dim(r11)[1],
    expected = 1
  )
  expect_equal(
    r11$reason[1],
    expected = "single observation"
  )
  t12 <- clean_overlapping_subtrips(distance_df = distance_df4,
                                   check_operator = FALSE,
                                   remove_single_observations = FALSE,
                                   remove_non_overlapping = FALSE)
  r12 <- clean_overlapping_subtrips(distance_df = distance_df4,
                                    check_operator = FALSE,
                                    remove_single_observations = FALSE,
                                    remove_non_overlapping = FALSE,
                                    return_removals = TRUE)
  expect_equal(
    t12,
    expected = distance_df4
  )
  expect_equal(
    dim(r12)[1],
    expected = 0
  )
})

# --- clean_jumps() ---
test_that("clean_jumps: standard", {

  distance_df = data.frame(
    trip_id_performed = rep("a", 9),
    distance = c(0, 1, 2, 3, 100, 4, 5, 6, 7), # index 5 is outlier
    event_timestamp = as.POSIXct(seq(from = 5, by = 5, length.out = 9))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # t cutoff
  t <- clean_jumps(distance_df = distance_df)
  r <- clean_jumps(distance_df = distance_df,
                   return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t),
    expected = data.table::as.data.table(distance_df[-5,])
  )
  expect_equal(
    dim(r)[1],
    expected = 1
  )
  expect_equal(
    r$location_ping_id[1],
    expected = "5"
  )

  # dist cutoff
  t2 <- clean_jumps(distance_df = distance_df,
                    t_cutoff = Inf,
                    min_median_deviation = -10, max_median_deviation = 10)
  r2 <- clean_jumps(distance_df = distance_df,
                    t_cutoff = Inf,
                    min_median_deviation = -10, max_median_deviation = 10,
                   return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t2),
    expected = data.table::as.data.table(distance_df[-5,])
  )
  expect_equal(
    dim(r2)[1],
    expected = 1
  )
  expect_equal(
    r2$location_ping_id[1],
    expected = "5"
  )

  # neither cutoff
  t3 <- clean_jumps(distance_df = distance_df,
                    t_cutoff = Inf)
  r3 <- clean_jumps(distance_df = distance_df,
                    t_cutoff = Inf,
                    return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t3),
    expected = data.table::as.data.table(distance_df)
  )
  expect_equal(
    dim(r3)[1],
    expected = 0
  )
})
test_that("clean_jumps: replacement", {

  distance_df = data.frame(
    trip_id_performed = rep("a", 9),
    distance = c(0, 1, 2, 3, 100, 4, 5, 6, 7), # index 5 is outlier
    event_timestamp = as.POSIXct(seq(from = 5, by = 5, length.out = 9))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # t cutoff
  t <- clean_jumps(distance_df = distance_df,
                   replace_outliers = TRUE)
  r <- clean_jumps(distance_df = distance_df,
                   replace_outliers = TRUE,
                   return_removals = TRUE)
  expect_equal(
    t$distance[5],
    expected = 4
  )
  expect_equal(
    dim(r)[1],
    expected = 1
  )
  expect_equal(
    r$location_ping_id[1],
    expected = "5"
  )
})
test_that("clean_jumps: implosion", {

  distance_df = data.frame(
    trip_id_performed = rep("a", 9),
    distance = c(0, 0, 0, 0, 100, 0, 10, 15, 20), # index 5 is outlier
    event_timestamp = as.POSIXct(seq(from = 5, by = 5, length.out = 9))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # do not check if implosion
  t <- clean_jumps(distance_df = distance_df,
                   evaluate_implosions = FALSE)
  r <- clean_jumps(distance_df = distance_df,
                   evaluate_implosions = FALSE,
                   return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t),
    expected = data.table::as.data.table(distance_df)
  )
  expect_equal(
    dim(r)[1],
    expected = 0
  )

  # check if implosion
  t2 <- clean_jumps(distance_df = distance_df,
                   evaluate_implosions = TRUE)
  r2 <- clean_jumps(distance_df = distance_df,
                   evaluate_implosions = TRUE,
                   return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t2),
    expected = data.table::as.data.table(distance_df[-c(4,5),])
  )
  expect_equal(
    dim(r2)[1],
    expected = 2
  )
  expect_equal(
    r2$location_ping_id,
    expected = c("4", "5")
  )
})
test_that("clean_jumps: tails", {

  distance_df = data.frame(
    trip_id_performed = rep("a", 9),
    distance = c(0, 100, 2, 3, 4, 5, 6, 7, 8), # index 2 is outlier
    event_timestamp = as.POSIXct(seq(from = 5, by = 5, length.out = 9))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # do not check tails
  t <- clean_jumps(distance_df = distance_df,
                   evaluate_tails = FALSE)
  r <- clean_jumps(distance_df = distance_df,
                   evaluate_tails = FALSE,
                   return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t),
    expected = data.table::as.data.table(distance_df)
  )
  expect_equal(
    dim(r)[1],
    expected = 0
  )

  # check tails
  t2 <- clean_jumps(distance_df = distance_df,
                   evaluate_tails = TRUE)
  r2 <- clean_jumps(distance_df = distance_df,
                   evaluate_tails = TRUE,
                   return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t2),
    expected = data.table::as.data.table(distance_df[-2,])
  )
  expect_equal(
    dim(r2)[1],
    expected = 1
  )
  expect_equal(
    r2$location_ping_id,
    expected = "2"
  )
})

# --- clean_incomplete_trips() ---
test_that("clean_incomplete_trips: distance filters", {

  distance_df <- data.frame(
    trip_id_performed = c("a", "a", "b", "b"),
    distance = c(0, 100, 0, 50),
    event_timestamp = as.POSIXct(c(0, 60, 0, 30))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # min dist
  t <- clean_incomplete_trips(distance_df = distance_df,
                              min_trip_distance = 50)
  r <- clean_incomplete_trips(distance_df = distance_df,
                              min_trip_distance = 50,
                              return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t),
    expected = data.table::as.data.table(distance_df)
  )
  expect_equal(
    dim(r)[1],
    expected = 0
  )

  t2 <- clean_incomplete_trips(distance_df = distance_df,
                              min_trip_distance = 51)
  r2 <- clean_incomplete_trips(distance_df = distance_df,
                              min_trip_distance = 51,
                              return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t2),
    expected = data.table::as.data.table(distance_df %>% dplyr::filter(trip_id_performed == "a"))
  )
  expect_equal(
    dim(r2)[1],
    expected = 1
  )
  expect_equal(
    r2$trip_id_performed,
    expected = "b"
  )

  # dist gap
  t3 <- clean_incomplete_trips(distance_df = distance_df,
                               max_distance_gap = 100)
  r3 <- clean_incomplete_trips(distance_df = distance_df,
                               max_distance_gap = 100,
                               return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t3),
    expected = data.table::as.data.table(distance_df)
  )
  expect_equal(
    dim(r3)[1],
    expected = 0
  )

  t4 <- clean_incomplete_trips(distance_df = distance_df,
                               max_distance_gap = 99)
  r4 <- clean_incomplete_trips(distance_df = distance_df,
                               max_distance_gap = 99,
                               return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t4),
    expected = data.table::as.data.table(distance_df %>% dplyr::filter(trip_id_performed == "b"))
  )
  expect_equal(
    dim(r4)[1],
    expected = 1
  )
  expect_equal(
    r4$trip_id_performed,
    expected = "a"
  )
})
test_that("clean_incomplete_trips: time filters", {

  distance_df <- data.frame(
    trip_id_performed = c("a", "a", "b", "b"),
    distance = c(0, 100, 0, 50),
    event_timestamp = as.POSIXct(c(0, 60, 0, 30))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # min duration
  t <- clean_incomplete_trips(distance_df = distance_df,
                              min_trip_duration = 30)
  r <- clean_incomplete_trips(distance_df = distance_df,
                              min_trip_duration = 30,
                              return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t),
    expected = data.table::as.data.table(distance_df)
  )
  expect_equal(
    dim(r)[1],
    expected = 0
  )

  t2 <- clean_incomplete_trips(distance_df = distance_df,
                               min_trip_duration = 31)
  r2 <- clean_incomplete_trips(distance_df = distance_df,
                               min_trip_duration = 31,
                               return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t2),
    expected = data.table::as.data.table(distance_df %>% dplyr::filter(trip_id_performed == "a"))
  )
  expect_equal(
    dim(r2)[1],
    expected = 1
  )
  expect_equal(
    r2$trip_id_performed,
    expected = "b"
  )

  # time gap
  t3 <- clean_incomplete_trips(distance_df = distance_df,
                               max_time_gap = 60)
  r3 <- clean_incomplete_trips(distance_df = distance_df,
                               max_time_gap = 60,
                               return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t3),
    expected = data.table::as.data.table(distance_df)
  )
  expect_equal(
    dim(r3)[1],
    expected = 0
  )

  t4 <- clean_incomplete_trips(distance_df = distance_df,
                               max_time_gap = 59)
  r4 <- clean_incomplete_trips(distance_df = distance_df,
                               max_time_gap = 59,
                               return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t4),
    expected = data.table::as.data.table(distance_df %>% dplyr::filter(trip_id_performed == "b"))
  )
  expect_equal(
    dim(r4)[1],
    expected = 1
  )
  expect_equal(
    r4$trip_id_performed,
    expected = "a"
  )
})
test_that("clean_incomplete_trips: time & dist filters", {

  distance_df <- data.frame(
    trip_id_performed = c("a", "a", "b", "b"),
    distance = c(0, 100, 0, 50),
    event_timestamp = as.POSIXct(c(0, 60, 0, 30))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # min dist & duration
  t <- clean_incomplete_trips(distance_df = distance_df,
                              min_trip_distance = 51,
                              min_trip_duration = 31)
  r <- clean_incomplete_trips(distance_df = distance_df,
                              min_trip_distance = 51,
                              min_trip_duration = 31,
                              return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t),
    expected = data.table::as.data.table(distance_df %>% dplyr::filter(trip_id_performed == "a"))
  )
  expect_equal(
    dim(r)[1],
    expected = 1
  )
  expect_equal(
    r$trip_id_performed,
    expected = "b"
  )

  # dist & time gaps
  t2 <- clean_incomplete_trips(distance_df = distance_df,
                               max_distance_gap = 99,
                               max_time_gap = 29)
  r2 <- clean_incomplete_trips(distance_df = distance_df,
                               max_distance_gap = 99,
                               max_time_gap = 29,
                               return_removals = TRUE)
  expect_equal(
    data.table::as.data.table(t2),
    expected = data.table::as.data.table(distance_df %>% dplyr::filter(!(trip_id_performed %in% c("a", "b"))))
  )
  expect_equal(
    dim(r2)[1],
    expected = 2
  )
  expect_equal(
    r2$trip_id_performed,
    expected = c("a", "b")
  )
  expect_equal(
    r2$dist_gap_ok,
    expected = c(FALSE, TRUE)
  )
  expect_equal(
    r2$t_gap_ok,
    expected = c(FALSE, FALSE)
  )

})

# --- trim_trips() ---
test_that("trim_trips: type validation", {

  distance_df <- data.frame(
    trip_id_performed = rep("a", 7),
    distance = c(0, 0, 0, 0, 1, 2, 3),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 7))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  # error
  expect_error(
    trim_trips(distance_df = distance_df,
               trim_type = "abc"),
    class = "error_trimtrips_type"
  )

  # ok inputs
  expect_no_error(
    trim_trips(distance_df = distance_df,
               trim_type = "beginning")
  )
  expect_no_error(
    trim_trips(distance_df = distance_df,
               trim_type = "end")
  )
  expect_no_error(
    trim_trips(distance_df = distance_df,
               trim_type = "both")
  )

})
test_that("trim_trips: direction warning", {

  distance_df <- data.frame(
    trip_id_performed = rep("a", 6),
    distance = c(5, 1, 0, 1, 2, 3),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  expect_warning(
    trim_trips(distance_df = distance_df),
    class = "warn_trimtrips_dir"
  )
})
test_that("trim_trips: trim beginning", {

  # trim
  distance_df1 <- data.frame(
    trip_id_performed = rep("a", 6),
    distance = c(2, 1, 0, 1, 2, 3),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  t1 <- trim_trips(distance_df = distance_df1,
                   trim_type = "beginning")
  r1 <- trim_trips(distance_df = distance_df1,
                   trim_type = "beginning",
                   return_removals = TRUE)

  expect_equal(
    data.table::as.data.table(t1),
    expected = data.table::as.data.table(distance_df1[3:6,])
  )
  expect_equal(
    dim(r1)[1],
    expected = 2
  )
  expect_equal(
    r1$location_ping_id,
    expected = distance_df1$location_ping_id[1:2]
  )

  # no trim
  distance_df2 <- data.frame(
    trip_id_performed = rep("a", 6),
    distance = c(0, 0, 0, 1, 2, 3),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  t2 <- trim_trips(distance_df = distance_df2,
                   trim_type = "beginning")
  r2 <- trim_trips(distance_df = distance_df2,
                   trim_type = "beginning",
                   return_removals = TRUE)

  expect_equal(
    data.table::as.data.table(t2),
    expected = data.table::as.data.table(distance_df2)
  )
  expect_equal(
    dim(r2)[1],
    expected = 0
  )
})
test_that("trim_trips: trim end", {

  # trim
  distance_df1 <- data.frame(
    trip_id_performed = rep("a", 6),
    distance = c(0, 1, 3, 4, 3, 2),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  t1 <- trim_trips(distance_df = distance_df1,
                   trim_type = "end")
  r1 <- trim_trips(distance_df = distance_df1,
                   trim_type = "end",
                   return_removals = TRUE)

  expect_equal(
    data.table::as.data.table(t1),
    expected = data.table::as.data.table(distance_df1[1:4,])
  )
  expect_equal(
    dim(r1)[1],
    expected = 2
  )
  expect_equal(
    r1$location_ping_id,
    expected = distance_df1$location_ping_id[5:6]
  )

  # no trim
  distance_df2 <- data.frame(
    trip_id_performed = rep("a", 6),
    distance = c(1, 2, 3, 4, 5, 6),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  t2 <- trim_trips(distance_df = distance_df2,
                   trim_type = "end")
  r2 <- trim_trips(distance_df = distance_df2,
                   trim_type = "end",
                   return_removals = TRUE)

  expect_equal(
    data.table::as.data.table(t2),
    expected = data.table::as.data.table(distance_df2)
  )
  expect_equal(
    dim(r2)[1],
    expected = 0
  )
})
test_that("trim_trips: trim both", {

  # trim
  distance_df1 <- data.frame(
    trip_id_performed = rep("a", 6),
    distance = c(1, 0, 1, 2, 3, 1),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  t1 <- trim_trips(distance_df = distance_df1,
                   trim_type = "both")
  r1 <- trim_trips(distance_df = distance_df1,
                   trim_type = "both",
                   return_removals = TRUE)

  expect_equal(
    data.table::as.data.table(t1),
    expected = data.table::as.data.table(distance_df1[2:5,])
  )
  expect_equal(
    dim(r1)[1],
    expected = 2
  )
  expect_equal(
    r1$location_ping_id,
    expected = distance_df1$location_ping_id[c(1, 6)]
  )

  # no trim
  distance_df2 <- data.frame(
    trip_id_performed = rep("a", 6),
    distance = c(1, 2, 3, 4, 5, 6),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  t2 <- trim_trips(distance_df = distance_df2,
                   trim_type = "both")
  r2 <- trim_trips(distance_df = distance_df2,
                   trim_type = "both",
                   return_removals = TRUE)

  expect_equal(
    data.table::as.data.table(t2),
    expected = data.table::as.data.table(distance_df2)
  )
  expect_equal(
    dim(r2)[1],
    expected = 0
  )
})

# --- make_monotonic() ---
test_that("make_monotonic: distance error validation", {

  distance_df <- data.frame(
    trip_id_performed = rep("a", 6),
    distance = c(0, 1, 2, 3, 4, 5),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()))

  expect_error(
    make_monotonic(distance_df = distance_df,
                   add_distance_error = "a"),
    class = "error_mono_dist_error"
  )
})
test_that("make_monotonic: weak", {

  # change
  distance_df1 <- data.frame(
    trip_id_performed = rep("a", 6),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()),
                  distance = c(0, 1, 2, 3, 2, 2))

  t1 <- make_monotonic(distance_df = distance_df1,
                      correct_speed = FALSE,
                      add_distance_error = 0)
  r1 <- make_monotonic(distance_df = distance_df1,
                      correct_speed = FALSE,
                      add_distance_error = 0,
                      return_changes = TRUE)

  expect_equal(
    t1$distance,
    expected = c(0, 1, 2, 3, 3, 3)
  )
  expect_equal(
    dim(r1)[1],
    expected = 2
  )
  expect_equal(
    r1$location_ping_id,
    expected = distance_df1$location_ping_id[5:6]
  )

  # no change
  distance_df2 <- data.frame(
    trip_id_performed = rep("a", 6),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()),
                  distance = c(0, 1, 2, 3, 3, 3))

  t2 <- make_monotonic(distance_df = distance_df2,
                       correct_speed = FALSE,
                       add_distance_error = 0)
  r2 <- make_monotonic(distance_df = distance_df2,
                       correct_speed = FALSE,
                       add_distance_error = 0,
                       return_changes = TRUE)

  expect_equal(
    data.table::as.data.table(t2),
    expected = data.table::as.data.table(distance_df2)
  )
  expect_equal(
    dim(r2)[1],
    expected = 0
  )

})
test_that("make_monotonic: strict", {

  # change
  distance_df1 <- data.frame(
    trip_id_performed = rep("a", 6),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()),
                  distance = c(0, 1, 2, 3, 2, 2))

  t1 <- make_monotonic(distance_df = distance_df1,
                       correct_speed = FALSE,
                       add_distance_error = 0.1)
  r1 <- make_monotonic(distance_df = distance_df1,
                       correct_speed = FALSE,
                       add_distance_error = 0.1,
                       return_changes = TRUE)

  expect_equal(
    t1$distance,
    expected = c(0, 1, 2, 3, 3.1, 3.2)
  )
  expect_equal(
    dim(r1)[1],
    expected = 2
  )
  expect_equal(
    r1$location_ping_id,
    expected = distance_df1$location_ping_id[5:6]
  )

  # no change
  distance_df2 <- data.frame(
    trip_id_performed = rep("a", 6),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()),
                  distance = c(0, 1, 2, 3, 4, 5))

  t2 <- make_monotonic(distance_df = distance_df2,
                       correct_speed = FALSE,
                       add_distance_error = 0)
  r2 <- make_monotonic(distance_df = distance_df2,
                       correct_speed = FALSE,
                       add_distance_error = 0,
                       return_changes = TRUE)

  expect_equal(
    data.table::as.data.table(t2),
    expected = data.table::as.data.table(distance_df2)
  )
  expect_equal(
    dim(r2)[1],
    expected = 0
  )

})
test_that("make_monotonic: prevent overshoot", {

  distance_df1 <- data.frame(
    trip_id_performed = rep("a", 6),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()),
                  distance = c(0, 1, 2, 2, 2, 3))

  t1 <- make_monotonic(distance_df = distance_df1,
                       correct_speed = FALSE,
                       add_distance_error = 0.5)

  expect_equal(
    t1$distance,
    expected = c(0, 1, 2, 2 + (1 / 4.5), 2 + (2 / 4.5), 3)
  )

})
test_that("make_monotonic: speeds", {

  # change
  distance_df1 <- data.frame(
    trip_id_performed = rep("a", 6),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()),
                  distance = c(0, 1, 2, 2, 2, 3),
                  speed = c(0.2, 0.2, 0, 100, 0, 0.2))

  r1 <- make_monotonic(distance_df = distance_df1,
                       correct_speed = TRUE,
                       add_distance_error = 0.01,
                       return_changes = TRUE)

  expect_equal(
    dim(r1)[1],
    expected = 3
  )
  expect_equal(
    r1$location_ping_id,
    expected = distance_df1$location_ping_id[3:5]
  )

  # no change
  distance_df2 <- data.frame(
    trip_id_performed = rep("a", 6),
    event_timestamp = as.POSIXct(seq(from = 0, by = 5, length.out = 6))
  ) %>%
    dplyr::mutate(location_ping_id = as.character(dplyr::row_number()),
                  distance = c(0, 1, 2.01, 2.02, 2.03, 3),
                  speed = c(0.2, 0.2, 1e-9, 1e-9, 1e-9, 0.2))

  r2 <- make_monotonic(distance_df = distance_df2,
                       correct_speed = TRUE,
                       add_distance_error = 0.01,
                       return_changes = TRUE)

  expect_equal(
    dim(r2)[1],
    expected = 0
  )

})
