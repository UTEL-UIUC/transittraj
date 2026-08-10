# --- filter_by_route() ---
test_that("filter_by_route: validation", {

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
test_that("get_shape_geometry: validation", {

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
  proj_1 <- suppressMessages(project_onto_route(shape_geometry = test_shape,
                                                points = points_1))
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
  proj_2 <- suppressMessages(project_onto_route(shape_geometry = test_shape,
                               points = points_2))
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
  proj_3 <- suppressMessages(project_onto_route(shape_geometry = test_shape,
                               points = points_3))
  expect_type(
    proj_3,
    type = "double"
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
  proj_1 <- suppressMessages(project_onto_route(shape_geometry = test_shape_1,
                               points = points_1,
                               project_crs = 32611))
  expect_equal(
    proj_1$distance,
    expected = exp_UTM,
    tolerance = 0.01
  )

  # original and proj CRS
  points_2 <- data.frame(latitude = c(398177.5),
                         longitude = c(3740525))
  proj_2 <- suppressMessages(project_onto_route(shape_geometry = test_shape_1,
                               points = points_2,
                               project_crs = 32611,
                               original_crs = 32611))
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
  proj_3 <- suppressMessages(project_onto_route(shape_geometry = test_shape_3,
                               points = points_3,
                               original_crs = 32611))
  expect_equal(
    proj_2$distance,
    expected = exp_WGS,
    tolerance = 0.01
  )
})

# --- get_stop_distances() ---
test_that("get_stop_distances: stops-shapes validation", {

  # create test GTFS with A Line only stop
  test_gtfs <- lacmta_gtfs
  test_gtfs$stops <- test_gtfs$stops %>%
    dplyr::filter(stop_id == 80101)
  shape_lineE <- get_shape_geometry(gtfs = lacmta_gtfs,
                                    shape = "804EB_RC_221121")

  expect_error(
    get_stop_distances(gtfs = test_gtfs,
                       shape_geometry = shape_lineE),
    class = "error_stopdists_inputshapes"
  )
})
test_that("get_stop_distances: stop dists", {

  # expected values
  stops_EBlineE <- 29
  stops_all <- 151

  EBlineE_shape <- "804EB_RC_221121"
  all_shapes <- c("804EB_RC_221121",
                  "804WB_RC_221121",
                  "801NB_P2B_250722",
                  "801SB_P2B_250722")

  # one shape
  shape_lineE <- get_shape_geometry(gtfs = lacmta_gtfs,
                                    shape = EBlineE_shape)
  stop_dists_1 <- suppressMessages(get_stop_distances(gtfs = lacmta_gtfs,
                                     shape_geometry = shape_lineE))
  expect_s3_class(
    stop_dists_1,
    "data.frame"
  )
  expect_true(
    "distance" %in% names(stop_dists_1)
  )
  expect_type(
    stop_dists_1$distance,
    "double"
  )
  expect_equal(
    dim(stop_dists_1)[1],
    expected = stops_EBlineE
  )
  expect_equal(
    unique(stop_dists_1$shape_id),
    EBlineE_shape
  )

  # all shapes
  stop_dists_2 <- suppressMessages(get_stop_distances(gtfs = lacmta_gtfs))
  expect_s3_class(
    stop_dists_2,
    "data.frame"
  )
  expect_true(
    "distance" %in% names(stop_dists_2)
  )
  expect_type(
    stop_dists_2$distance,
    "double"
  )
  expect_equal(
    dim(stop_dists_2)[1],
    expected = stops_all
  )
  expect_setequal(
    unique(stop_dists_2$shape_id),
    all_shapes
  )
})

# --- get_gtfs_service_dates() ---
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
test_that("get_gtfs_service_dates: calendar type validation", {

  expect_error(
    get_gtfs_service_dates(gtfs = lacmta_gtfs,
                           use_calendar_table = "oops"),
    class = "error_gtfsdate_inputdata"
  )
})
test_that("get_gtfs_service_dates: calendar", {

  exp_date_seq <- seq(from = as.Date("2026-05-27"),
                      to = as.Date("2026-06-05"),
                      by = 1)
  lineE_service_ids <- c("RDEC25-804-1_Weekday-90",
                         "RDEC25-804-1_Weekday-90",
                         "RDEC25-804-1_Weekday-90",
                         NA, NA, # weekends, not included in sample GTFS
                         "RDEC25-804-1_Weekday-90",
                         "RDEC25-804-1_Weekday-90",
                         "RDEC25-804-1_Weekday-90",
                         "RDEC25-804-1_Weekday-90",
                         "RDEC25-804-1_Weekday-90")
  gtfs_ELine <- filter_by_route(gtfs = lacmta_gtfs,
                                route_ids = "804")

  # no filt
  dates_1 <- get_gtfs_service_dates(gtfs = gtfs_ELine,
                                    use_calendar_table = "calendar")
  expect_s3_class(
    dates_1,
    "data.frame"
  )
  expect_setequal(
    names(dates_1),
    expected = c("date", "service_id")
  )
  expect_setequal(
    dates_1$date,
    expected = exp_date_seq
  )
  expect_equal(
    dates_1$service_id,
    lineE_service_ids
  )

  # valid date range
  dates_2 <- get_gtfs_service_dates(gtfs = gtfs_ELine,
                                    use_calendar_table = "calendar",
                                    date_min = exp_date_seq[3],
                                    date_max = exp_date_seq[4])
  expect_s3_class(
    dates_2,
    "data.frame"
  )
  expect_setequal(
    names(dates_2),
    expected = c("date", "service_id")
  )
  expect_setequal(
    dates_2$date,
    expected = exp_date_seq[3:4]
  )
  expect_equal(
    dates_2$service_id,
    lineE_service_ids[3:4]
  )
})
test_that("get_gtfs_service_dates: calendar_dates", {

  # build fake gtfs
  # will throw many warnings due to missing tables
  calendar_dates <- data.frame(
    date = seq(from = as.Date("2026-01-01"),
               to = as.Date("2026-01-31"),
               by = 1)
  ) %>%
    dplyr::mutate(service_id = dplyr::if_else(condition = (weekdays(x = date) %in% c("Saturday", "Sunday")),
                                              true = "0",
                                              false = "1"))
  gtfs <- suppressWarnings(tidytransit::as_tidygtfs(
    list("calendar_dates" = calendar_dates)
  ))

  # no date filter
  dates_1 <- suppressWarnings(
    get_gtfs_service_dates(gtfs = gtfs,
                           use_calendar_table = "calendar_dates")
  )
  expect_equal(
    names(dates_1),
    expected = c("service_id", "date")
  )
  expect_s3_class(
    dates_1$date,
    class = "Date"
  )
  expect_equal(
    data.frame(dates_1),
    data.frame(service_id = gtfs$calendar_dates$service_id,
               date = gtfs$calendar_dates$date)
  )

  # date filter
  dates_2 <- suppressWarnings(
    get_gtfs_service_dates(gtfs = gtfs,
                           use_calendar_table = "calendar_dates",
                           date_min = as.Date("2026-01-15"),
                           date_max = as.Date("2026-01-16"))
  )
  expect_equal(
    names(dates_2),
    expected = c("service_id", "date")
  )
  expect_s3_class(
    dates_2$date,
    class = "Date"
  )
  expect_equal(
    min(dates_2$date),
    expected = as.Date("2026-01-15")
  )
  expect_equal(
    max(dates_2$date),
    expected = as.Date("2026-01-16")
  )
})

# --- plot_interactive_gtfs() ---
test_that("plot_interactive_gtfs: viewer", {

  # expected
  exp_def_colors <- c("#1B9E77", "#7570B3")
  exp_gtfs_colors <- c("#0072BC", "#FDB913")
  exp_routes <- c("801", "804")
  exp_shapes <- 2 * 2 # 2 routes, 2 directions each
  exp_stops <- 72

  # default color palette
  lf <- plot_interactive_gtfs(gtfs = lacmta_gtfs)
  expect_s3_class(
    lf,
    class = "leaflet"
  )
  num_shapes <- length(lf$x$calls[[1]]$args[[1]])
  expect_equal(
    num_shapes,
    expected = exp_shapes
  )
  num_stops <- length(lf$x$calls[[3]]$args[[9]])
  expect_equal(
    num_stops,
    expected = exp_stops
  )
  routes_present <- lf$x$calls[[2]]$args[[1]]$labels
  expect_setequal(
    routes_present,
    expected = exp_routes
  )
  line_colors <- unique(lf$x$calls[[1]]$args[[4]]$color)
  expect_setequal(
    line_colors,
    expected = exp_def_colors
  )

  # GTFS colors
  lf_2 <- plot_interactive_gtfs(gtfs = lacmta_gtfs,
                                color_palette = "gtfs")
  expect_s3_class(
    lf_2,
    class = "leaflet"
  )
  line_colors_2 <- unique(lf_2$x$calls[[1]]$args[[4]]$color)
  expect_setequal(
    line_colors_2,
    expected = exp_gtfs_colors
  )

  # GTFS case-insensitive
  lf_3 <- plot_interactive_gtfs(gtfs = lacmta_gtfs,
                                color_palette = "gTfS")
  expect_s3_class(
    lf_3,
    class = "leaflet"
  )
  line_colors_3 <- unique(lf_3$x$calls[[1]]$args[[4]]$color)
  expect_setequal(
    line_colors_3,
    expected = exp_gtfs_colors
  )

})













###
