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
