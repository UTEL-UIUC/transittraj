#' LA Metro AVL Data
#'
#' @description
#' This dataset contains TIDES-formatted automatic vehicle location (AVL)
#' data from the Los Angeles
#' County Metropolitan Transportation Authority (LACMTA), or Metro. Pings
#' for all Lines A and E trips starting between 6:00 and 8:00 AM on May 27,
#' 2026 are included. The data was accessed via Caltrans's [open-source
#' bucket](https://tides.dds.dot.ca.gov/) of TIDES `vehicle_locations`
#' tables. This dataset is inteded to be used alongside the static GTFS
#' feed provided in `lacmta_gtfs`.
#'
#' @details
#' The dataset contains two light rail routes, with two directions for each:
#'
#' - Line A: Pomona North to Downtown Long Beach
#' - Line E: Downtown Santa Monica to Atlantic
#'
#' @format ## `lacmta_avl`
#' A dataframe with 14,179 rows and 11 columns.
#' \describe{
#'    \item{location_ping_id}{A unique ID for each row.}
#'    \item{service_date}{The date of the trip's beginning.}
#'    \item{trip_id_performed}{Trip IDs, matching those in GTFS.}
#'    \item{latitude, longitude}{The GPS ping longitude and latitude.}
#'    \item{speed}{The recorded speed, in meters per second.}
#'    \item{vehicle_id}{An ID corresponding to each vehicle.}
#'    \item{event_timestamp}{POSIXct time objects, including the day, time,
#'    and local timezone.}
#'    \item{direction_id}{Direction IDs, matching those in GTFS. For Line A, `0` is northbound and `1` is soutbound; for Line E, `0` is eastbound and `1` is westbound.}
#'    \item{shape_id}{Shape IDs, matching those in GTFS. Each route and direction has one shape ID.}
#'    \item{route_id}{Route IDs, matching those in GTFS. `"801"` is Line A,
#'    and `"804"` is Line E.}
#' }
#' @source <https://tides.dds.dot.ca.gov/>
#' @examples
#' # Print the header
#' head(lacmta_avl)
#'
#' # Filter the data
#' lineE_avl <- lacmta_avl %>%
#'     dplyr::filter((route_id == "804") & (direction_id == 0))
#' print(unique(lineE_avl$shape_id))
#'
#' # Use in the AVL cleaning workflow
#' lineE_shape <- get_shape_geometry(gtfs = lacmta_gtfs,
#'                                   shape = "804EB_RC_221121",
#'                                   project_crs = 32611)
#' lineE_dists <- get_linear_distances(avl_df = lineE_avl,
#'                                     shape_geometry = lineE_shape,
#'                                     clip_buffer = 50,
#'                                     project_crs = 32611)
#' head(lineE_dists)
"lacmta_avl"

#' LA Metro GTFS
#'
#' @description
#' This dataset is a portion of the rail General Transit Feed
#' Specification (GTFS) from the Los Angeles County Metropolitan
#' Transportation Authority (LACMTA), or Metro. This feed version was first
#' published on May 27, 2026, and was valid through May 28, 2026. This
#' dataset is intended to be used alongside the TIDES AVL data provided
#' in `lacmta_avl`.
#'
#' @details
#' The GTFS feed has been filtered to two light rail routes, with two
#' directions for each, on one service date (May 27, 2026):
#'
#' - Line A: Pomona North to Downtown Long Beach
#' - Line E: Downtown Santa Monica to Atlantic
#'
#' @format ## `lacmta_gtfs`
#' A `tidytransit` `tidygtfs` object (list) with 8 files.
#' \describe{
#'    \item{agency}{The GTFS `agency.txt` file.}
#'    \item{routes}{The GTFS `routes.txt` file.}
#'    \item{trips}{The GTFS `trips.txt` file.}
#'    \item{stop_times}{The GTFS `stop_times.txt` file.}
#'    \item{stops}{The GTFS `stops.txt` file.}
#'    \item{shapes}{The GTFS `shapes.txt` file.}
#'    \item{calendar}{The GTFS `calendar.txt` file.}
#'    \item{calendar_dates}{The GTFS `calendar_dates.txt` file.}
#'    \item{fare_rules}{The GTFS `fare_rules.txt` file.}
#' }
#' @source <https://www.transit.land/feeds/f-9q5-metro~losangeles~rail>
#' @examples
#' # Print the tidytransit summary
#' summary(lacmta_gtfs)
#'
#' # Filter by route & direction
#' my_route <- "804"
#' my_dir <- 0
#' lineE_gtfs <- filter_by_route(gtfs = lacmta_gtfs,
#'                               route_ids = my_route,
#'                               dir_id = my_dir)
#' summary(lineE_gtfs)
#'
#' # Extract route alignments
#' lineE_shapes <- get_shape_geometry(gtfs = lineE_gtfs)
#' print(lineE_shapes)
"lacmta_gtfs"
