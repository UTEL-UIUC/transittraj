test_that("get_gtfs_service_dates: date input range validation", {
  d1 <- as.Date("2026-05-27")
  d2 <- as.Date("2026-05-28")
  d3 <- as.Date("1980-01-01")
  d4 <- as.Date("1980-02-01")
  d5 <- as.Date("2030-01-01")
  d6 <- as.Date("2030-02-01")

  # incorrect date data types
  expect_error(
    get_gtfs_service_dates(gtfs = lacmta_gtfs,
                           date_min = "2026-01-01",
                           date_max = "2026-02-01"),
    class = "error_gtfsdate_inputdata"
  )
  expect_error(
    get_gtfs_service_dates(gtfs = lacmta_gtfs,
                           date_min = d1,
                           date_max = "2026-02-01"),
    class = "error_gtfsdate_inputdata"
  )
  expect_error(
    get_gtfs_service_dates(gtfs = lacmta_gtfs,
                           date_min = "2026-01-01",
                           date_max = d2),
    class = "error_gtfsdate_inputdata"
  )
  expect_error(
    get_gtfs_service_dates(gtfs = lacmta_gtfs,
                           date_min = "2026-01-01"),
    class = "error_gtfsdate_inputdata"
  )
  expect_error(
    get_gtfs_service_dates(gtfs = lacmta_gtfs,
                           date_max = "2026-02-01"),
    class = "error_gtfsdate_inputdata"
  )

  # date_min > date_max
  expect_error(
    get_gtfs_service_dates(gtfs = lacmta_gtfs,
                           date_min = d2,
                           date_max = d1),
    class = "error_gtfsdate_inputdata"
  )

  # range too late
  expect_error(
    get_gtfs_service_dates(gtfs = lacmta_gtfs,
                           date_min = d5),
    class = "error_gtfsdate_inputdata"
  )
  expect_error(
    get_gtfs_service_dates(gtfs = lacmta_gtfs,
                           date_max = d6),
    class = "error_gtfsdate_inputdata"
  )
  expect_error(
    get_gtfs_service_dates(gtfs = lacmta_gtfs,
                           date_min = d5,
                           date_max = d6),
    class = "error_gtfsdate_inputdata"
  )
  expect_error(
    get_gtfs_service_dates(gtfs = lacmta_gtfs,
                           date_min = d1,
                           date_max = d6),
    class = "error_gtfsdate_inputdata"
  )

  # range too early
  expect_error(
    get_gtfs_service_dates(gtfs = lacmta_gtfs,
                           date_min = d3),
    class = "error_gtfsdate_inputdata"
  )
  expect_error(
    get_gtfs_service_dates(gtfs = lacmta_gtfs,
                           date_max = d4),
    class = "error_gtfsdate_inputdata"
  )
  expect_error(
    get_gtfs_service_dates(gtfs = lacmta_gtfs,
                           date_min = d3,
                           date_max = d4),
    class = "error_gtfsdate_inputdata"
  )
  expect_error(
    get_gtfs_service_dates(gtfs = lacmta_gtfs,
                           date_min = d3,
                           date_max = d2),
    class = "error_gtfsdate_inputdata"
  )
})

# --- filter_by_route() ---
test_that("filter_by_route: route validation", {

  # One route
  expect_error(
    filter_by_route(gtfs = lacmta_gtfs,
                    route_ids = "does not exist"),
    class = "error_gtfsfilt_none"
  )

  # Multiple routes, neither exist
  expect_error(
    filter_by_route(gtfs = lacmta_gtfs,
                    route_ids = c("does not exist", "also does not exist")),
    class = "error_gtfsfilt_none"
  )

  # Multiple routes, one doesn't exist
  expect_equal(
    unique(filter_by_route(gtfs = lacmta_gtfs,
                           route_ids = c("does not exist", "804"))$routes$route_id),
    expected = "804"
  )
})
test_that("filter_by_route: direction validation", {

  # One dir
  expect_error(
    filter_by_route(gtfs = lacmta_gtfs,
                    route_ids = "804",
                    dir_id = 10),
    class = "error_gtfsfilt_none"
  )

  # Multiple dirs
  expect_error(
    filter_by_route(gtfs = lacmta_gtfs,
                    route_ids = "804",
                    dir_id = c(10, 11)),
    class = "error_gtfsfilt_none"
  )

  # Multiple dirs, one doesn't exist
  expect_equal(
    unique(filter_by_route(gtfs = lacmta_gtfs,
                           route_ids = "804",
                           c(0, 10))$trips$direction_id),
    expected = 0
  )
})
test_that("filter_by_route: route expectations", {

  # Filter to one route
  gtfs_filt_1 <- filter_by_route(gtfs = lacmta_gtfs,
                                 route_ids = "804")
  expect_equal(unique(gtfs_filt_1$routes$route_id),
               expected = "804")
  expect_setequal(unique(gtfs_filt_1$trips$direction_id),
               expected = c(0, 1))
  expect_s3_class(gtfs_filt_1,
                  class = "tidygtfs")

  # Filter to multiple routes
  gtfs_filt_2 <- filter_by_route(gtfs = lacmta_gtfs,
                                 route_ids = c("801", "804"))
  expect_setequal(unique(gtfs_filt_2$routes$route_id),
               expected = c("801", "804"))
  expect_setequal(unique(gtfs_filt_2$trips$direction_id),
               expected = c(0, 1))
  expect_s3_class(gtfs_filt_2,
                  class = "tidygtfs")
})
test_that("filter_by_route: direction expectations", {

  # Filter to one route, one dir
  gtfs_filt_2 <- filter_by_route(gtfs = lacmta_gtfs,
                                 route_ids = "804",
                                 dir_id = 0)
  expect_equal(unique(gtfs_filt_2$routes$route_id),
                   expected = "804")
  expect_equal(unique(gtfs_filt_2$trips$direction_id),
                   expected = c(0))
  expect_s3_class(gtfs_filt_2,
                  class = "tidygtfs")

  # Filter to one route, multiple dir
  gtfs_filt_3 <- filter_by_route(gtfs = lacmta_gtfs,
                                 route_ids = "804",
                                 dir_id = c(0, 1))
  expect_equal(unique(gtfs_filt_3$routes$route_id),
                   expected = "804")
  expect_setequal(unique(gtfs_filt_3$trips$direction_id),
                   expected = c(0, 1))
  expect_s3_class(gtfs_filt_3,
                  class = "tidygtfs")
})

# --- get_shape_geometry() ---
test_that("get_shape_geometry: shape validation", {

  expect_error(
    get_shape_geometry(lacmta_gtfs,
                       shape = "does not exist"),
    class = "error_gtfsshape_none"
  )
})
test_that("get_shape_geometry: shape filter", {

  all_shapes <- c("804EB_RC_221121",
                  "804WB_RC_221121",
                  "801NB_P2B_250722",
                  "801SB_P2B_250722")

  # No shape filter
  shapes_1 <- get_shape_geometry(lacmta_gtfs)
  expect_setequal(
    shapes_1$shape_id,
    expected = all_shapes
  )
  expect_s3_class(
    shapes_1,
    class = "sf"
  )

  # Shape filter
  shapes_2 <- get_shape_geometry(lacmta_gtfs,
                                 shape = all_shapes[1])
  expect_equal(
    shapes_2$shape_id,
    expected = all_shapes[1]
  )
  expect_s3_class(
    shapes_2,
    class = "sf"
  )
})
test_that("get_shape_geometry: spatial projection", {

  # None
  shapes_1 <- get_shape_geometry(lacmta_gtfs)
  crs_1 <- sf::st_crs(shapes_1)
  expect_equal(
    crs_1$input,
    expected = "EPSG:4326"
  )

  # Filter
  shapes_2 <- get_shape_geometry(lacmta_gtfs,
                                 project_crs = 32616)
  crs_2 <- sf::st_crs(shapes_2)
  expect_equal(
    crs_2$input,
    expected = "EPSG:32616"
  )
})

# --- project_onto_route() ---
test_that("project_onto_route: points validation", {

  test_shape <- get_shape_geometry(gtfs = lacmta_gtfs,
                                   shape = "804EB_RC_221121")

  # Test field requirement: neither
  points_1 <- data.frame(lat = c(33.8),
                         lon = c(-118.1))
  expect_error(
    project_onto_route(shape_geometry = test_shape,
                       points = points_1),
    class = "error_pointsval_fields"
  )

  # Test field requirement: one
  points_2 <- data.frame(latitude = c(33.8),
                         lon = c(-118.1))
  expect_error(
    project_onto_route(shape_geometry = test_shape,
                       points = points_2),
    class = "error_pointsval_fields"
  )

  # non-point SF
  expect_error(
    project_onto_route(shape_geometry = test_shape,
                       points = test_shape),
    class = "error_pointsval_geomtype"
  )

  # other data type
  points_3 <- c(33.8, -118.1)
  expect_error(
    project_onto_route(shape_geometry = test_shape,
                       points = points_3),
    class = "error_pointsval_datatype"
  )
})
test_that("project_onto_route: points output testing", {

  # expected results, depending on coord sys
  exp_WGS <- 35405.46
  exp_UTM <- 35443.62

  test_shape <- get_shape_geometry(gtfs = lacmta_gtfs,
                                   shape = "804EB_RC_221121")

  # Test points: df
  points_1 <- data.frame(latitude = c(33.8),
                         longitude = c(-118.1))
  proj_1 <- project_onto_route(shape_geometry = test_shape,
                               points = points_1)
  expect_setequal(
    names(proj_1),
    expected = c("latitude", "longitude", "distance")
  )
  expect_equal(
    proj_1$distance,
    expected = exp_WGS,
    tolerance = 0.01
  )

  # Test points: sf
  points_2 <- data.frame(latitude = c(33.8),
                         longitude = c(-118.1)) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"),
                 crs = 4326)
  proj_2 <- project_onto_route(shape_geometry = test_shape,
                               points = points_2)
  expect_setequal(
    names(proj_2),
    expected = c("distance")
  )
  expect_equal(
    proj_2$distance,
    expected = exp_WGS,
    tolerance = 0.01
  )

  # Test points: sfc
  points_3 <- data.frame(latitude = c(33.8),
                         longitude = c(-118.1)) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"),
                 crs = 4326) %>%
    sf::st_geometry()
  proj_3 <- project_onto_route(shape_geometry = test_shape,
                               points = points_3)
  expect_class(
    proj_3,
    class = "numeric"
  )
  expect_equal(
    length(proj_3),
    expected = 1
  )
  expect_equal(
    proj_3,
    expected = exp_WGS,
    tolerance = 0.01
  )
})
test_that("project_onto_route: projection testing", {

  # expected results, depending on coord sys
  exp_WGS <- 35405.46
  exp_UTM <- 35443.62

  # projection CRS
  test_shape_1 <- get_shape_geometry(gtfs = lacmta_gtfs,
                                     shape = "804EB_RC_221121",
                                     project_crs = 32611)
  points_1 <- data.frame(latitude = c(33.8),
                         longitude = c(-118.1))
  proj_1 <- project_onto_route(shape_geometry = test_shape_1,
                               points = points_1,
                               project_crs = 32611)
  expect_equal(
    proj_1$distance,
    expected = exp_UTM,
    tolerance = 0.01
  )

  # original and proj CRS
  points_2 <- data.frame(latitude = c(398177.5),
                         longitude = c(3740525))
  proj_2 <- project_onto_route(shape_geometry = test_shape_1,
                               points = points_2,
                               project_crs = 32611,
                               original_crs = 32611)
  expect_equal(
    proj_2$distance,
    expected = exp_UTM,
    tolerance = 0.01
  )

  # original CRS only
  test_shape_3 <- get_shape_geometry(gtfs = lacmta_gtfs,
                                     shape = "804EB_RC_221121",
                                     project_crs = 4326)
  points_3 <- data.frame(latitude = c(398177.5),
                         longitude = c(3740525))
  proj_3 <- project_onto_route(shape_geometry = test_shape_3,
                               points = points_3,
                               original_crs = 32611)
  expect_equal(
    proj_2$distance,
    expected = exp_WGS,
    tolerance = 0.01
  )
})





















a
