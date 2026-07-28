# --- validate_tides() ---
test_that("validate_tides: test output on sample data", {

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

# --- validate_monotonicity() ---
test_that("validate_monotonicity")
