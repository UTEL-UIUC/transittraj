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
  expect_equal(unique(gtfs_filt_1$trips$direction_id),
               expected = c(0, 1))
  expect_s3_class(gtfs_filt_1,
                  class = "tidygtfs")

  # Filter to multiple routes
  gtfs_filt_2 <- filter_by_route(gtfs = lacmta_gtfs,
                                 route_ids = c("801", "804"))
  expect_equal(unique(gtfs_filt_2$routes$route_id),
               expected = c("801", "804"))
  expect_equal(unique(gtfs_filt_2$trips$direction_id),
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
  expect_equal(unique(gtfs_filt_3$trips$direction_id),
                   expected = c(0, 1))
  expect_s3_class(gtfs_filt_3,
                  class = "tidygtfs")
})





















a
