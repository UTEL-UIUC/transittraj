# --- validate_gtfs_input() ---
test_that("validate_gtfs_input: is gtfs", {

  # not gtfs
  expect_error(
    validate_gtfs_input(gtfs = "test",
                        table = "routes",
                        needed_fields = "route_id"),
    class = "error_gtfsval_not_tidygtfs"
  )

  # gtfs-like object
  expect_error(
    validate_gtfs_input(gtfs = unclass(lacmta_gtfs),
                        table = "routes",
                        needed_fields = "route_id"),
    class = "error_gtfsval_not_tidygtfs"
  )

})
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

# --- validate_shape_geometry() ---
test_that("validate_shape_geometry: data type", {

  shapes <- get_shape_geometry(gtfs = lacmta_gtfs)

  # no error on standard
  expect_no_error(
    validate_shape_geometry(shapes)
  )

  # error on SFC
  expect_error(
    validate_shape_geometry(shapes %>% sf::st_geometry()),
    class = "error_geomval_datatype"
  )

  # error on points
  expect_error(
    suppressWarnings(validate_shape_geometry(shapes %>%
                                             sf::st_cast("LINESTRING") %>%
                                             sf::st_cast("POINT"))),
    class = "error_geomval_geomtype"
  )

  # error on non-spatial
  expect_error(
    validate_shape_geometry("a string"),
    class = "error_geomval_datatype"
  )
})
test_that("validate_shape_geometry: shape IDs", {

  shapes <- get_shape_geometry(gtfs = lacmta_gtfs)[, 2]

  expect_error(
    validate_shape_geometry(shapes,
                            require_shape_id = TRUE),
    class = "error_geomval_id"
  )

  expect_no_error(
    validate_shape_geometry(shapes,
                            require_shape_id = FALSE)
  )
})
test_that("validate_shape_geometry: length", {

  shapes <- get_shape_geometry(gtfs = lacmta_gtfs)

  expect_error(
    validate_shape_geometry(shapes,
                            max_length = 1),
    class = "error_geomval_length"
  )

  expect_no_error(
    validate_shape_geometry(shapes,
                            max_length = 10)
  )
})
test_that("validate_shape_geometry: crs", {

  shapes <- get_shape_geometry(gtfs = lacmta_gtfs)

  expect_error(
    validate_shape_geometry(shapes,
                            match_crs = 32611),
    class = "error_geomval_crs"
  )
})
