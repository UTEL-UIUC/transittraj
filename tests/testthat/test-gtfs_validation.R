# --- validate_gtfs_input() ---
test_that("validate_gtfs_input: test tables", {

  # table present
  expect_no_error(
    validate_gtfs_input(gtfs = lacmta_gtfs,
                        table = "routes",
                        needed_fields = "route_id")
  )

  # table not present: bad table
  expect_error(
    validate_gtfs_input(gtfs = lacmta_gtfs,
                        table = "mystery",
                        needed_fields = "route_id"),
    class = "error_gtfsval_missing_table"
  )

  # table not present: good table
  expect_error(
    validate_gtfs_input(gtfs = lacmta_gtfs,
                        table = "fare_attributes",
                        needed_fields = "route_id"),
    class = "error_gtfsval_missing_table"
  )

})
test_that("validate_gtfs_input: test fields", {

  # field present
  expect_no_error(
    validate_gtfs_input(gtfs = lacmta_gtfs,
                        table = "shapes",
                        needed_fields = c("shape_id",
                                          "shape_pt_sequence",
                                          "shape_pt_lat"))
  )

  # field not present: bad field
  expect_error(
    validate_gtfs_input(gtfs = lacmta_gtfs,
                        table = "shapes",
                        needed_fields = "elevation"),
    class = "error_gtfsval_missing_fields"
  )

  # field not present: good field
  expect_error(
    validate_gtfs_input(gtfs = lacmta_gtfs,
                        table = "shapes",
                        needed_fields = "shape_dist_traveled"),
    class = "error_gtfsval_missing_fields"
  )
})
