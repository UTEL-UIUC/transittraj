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

















#
