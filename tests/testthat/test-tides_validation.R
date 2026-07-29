# --- validate_tides() ---
test_that("validate_tides: output on sample data", {

  base_val <- validate_tides(lacmta_avl)
  edit_val <- validate_tides(lacmta_avl %>%
                               dplyr::mutate(trip_id_performed = as.numeric(trip_id_performed)))

  # fields
  expect_all_true(
    base_val$field_present[c(1:4,6:7,9)]
  )
  expect_all_false(
    base_val$field_present[c(5,8)]
  )

  # data types
  expect_all_true(
    base_val$field_type_ok[c(1:4,6:7,9)]
  )
  expect_all_true(
    is.na(base_val$field_type_ok[c(5,8)])
  )
  expect_all_false(
    edit_val$field_type_ok[2]
  )
})

# --- validate_input_to_tides() ---
test_that("validate_input_to_tides: inputs", {

  # fields present
  expect_no_error(
    validate_input_to_tides(
      needed_fields = c("event_timestamp"),
      avl_df = lacmta_avl
    )
  )
  expect_no_error(
    validate_input_to_tides(
      needed_fields = c("event_timestamp", "trip_id_performed"),
      avl_df = lacmta_avl
    )
  )
  expect_error(
    validate_input_to_tides(
      needed_fields = c("event_timestamp"),
      avl_df = lacmta_avl %>% dplyr::select(-event_timestamp)
    ),
    class = "error_tidesval_missing_fields"
  )

  # field types
  expect_error(
    validate_input_to_tides(
      needed_fields = c("event_timestamp"),
      avl_df = lacmta_avl %>%
        dplyr::mutate(event_timestamp = as.numeric(event_timestamp))
    ),
    class = "error_tidesval_field_datatype"
  )
})

# --- validate_monotonicity() ---
test_that("validate_monotonicity: output on sample data", {

  # get data
  non_mono <- new_transittraj_data(func_output = "trim_trips")
  weak_mono_non_speed <- make_monotonic(distance_df = non_mono,
                                   correct_speed = FALSE)
  strict_mono_non_speed <- make_monotonic(distance_df = non_mono,
                                   correct_speed = FALSE,
                                   add_distance_error = 0.01)
  strict_mono_speed <- make_monotonic(distance_df = non_mono,
                               correct_speed = TRUE,
                               add_distance_error = 0.01)
  full_check_df <- validate_monotonicity(strict_mono_speed,
                                         check_speed = TRUE,
                                         return_full = TRUE)

  # non-mono
  expect_all_false(
    validate_monotonicity(non_mono, check_speed = TRUE)
  )

  # weak mono
  expect_equal(
    validate_monotonicity(weak_mono_non_speed, check_speed = TRUE),
    expected = c("weak" = TRUE,
                 "strict" = FALSE,
                 "speed" = FALSE)
  )

  # strict mono
  expect_equal(
    validate_monotonicity(strict_mono_non_speed, check_speed = TRUE),
    expected = c("weak" = TRUE,
                 "strict" = TRUE,
                 "speed" = FALSE)
  )

  # mono speed
  expect_equal(
    validate_monotonicity(strict_mono_speed, check_speed = TRUE),
    expected = c("weak" = TRUE,
                 "strict" = TRUE,
                 "speed" = TRUE)
  )

  # return full
  expect_s3_class(
    full_check_df,
    "data.frame"
  )
  expect_all_true(
    full_check_df$all_ok
  )
  expect_equal(
    dim(full_check_df)[1],
    expected = dim(non_mono)[1]
  )
})
