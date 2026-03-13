#' WMATA Bus Automatic Vehicle Location Data
#'
#' @description
#' This dataset is an archive of WMATA's public GTFS-realtime feed, accessed
#' from their developer API. It has been reformatted from GTFS-rt to match
#' the TIDES standard for both fields and datatypes. This dataset is intended
#' to be used alongside the static GTFS feed provided in `wmata_gtfs`.
#'
#' @details
#' The dataset contains three bus routes, with two directions for each:
#'
#' - D40: Georgia Ave
#' - C53: U St/Congress Heights
#' - D96: Mass Ave to Bethesda
#'
#' @format ## `wmata_avl`
#' A dataframe with 20,777 rows and 12 columns.
#' \describe{
#'    \item{location_ping_id}{A unique ID for each row}
#'    \item{vehicle_id}{An ID corresponding to each vehicle}
#'    \item{trip_id_performed}{Trip IDs, matching those in GTFS}
#'    \item{service_date}{The data of the trip's beginning}
#'    \item{route_id}{Route IDs, matching those in GTFS}
#'    \item{direction_id}{Direction IDs, matching those in GTFS}
#'    \item{latitude, longitude}{The GPS ping longitude and latitude}
#'    \item{speed}{The recorded speed, in meters per second}
#'    \item{trip_stop_sequence}{The stop number the vehicle is approaching}
#'    \item{event_timestamp}{POSIXct time objects}
#'    \item{stop_id}{Stop IDs the vehicles are approaching, matching those in GTFS}
#' }
#' @source <https://developer.wmata.com/>
#' @examples
#' # Print the header
#' head(wmata_avl)
#'
#' # Filter the data
#' c53_avl <- wmata_avl %>%
#'     dplyr::filter((route_id == "C53") & (direction_id == 0))
#' c53_shape <- get_shape_geometry(gtfs = wmata_gtfs,
#'                                 shape = "C53:04",
#'                                 project_crs = 32618)
#'
#' # Use in the AVL cleaning workflow
#' c53_dists <- get_linear_distances(avl_df = c53_avl,
#'                                   shape_geometry = c53_shape,
#'                                   clip_buffer = 50,
#'                                   project_crs = 32618)
#' head(c53_dists)
"wmata_avl"

#' WMATA Bus GTFS
#'
#' @description
#' This dataset is a portion of WMATA's bus GTFS, first published on Dec 14,
#' 2025 and valid through June 13, 2026, accessed through TransitLand. The
#' dataset is intended to be used alongside the archived GTFS-realtime
#' feed provided in `wmata_avl`.
#'
#' @details
#' This dataset has been filtered to three routes, with two directions for each:
#'
#' - D40: Georgia Ave
#' - C53: U St/Congress Heights
#' - D96: Mass Ave toBethesda
#'
#' @format ## `wmata_gtfs`
#' A tidytransit object (list) with 8 files.
#' \describe{
#'    \item{agency}{The GTFS `agency.txt` file}
#'    \item{routes}{The GTFS `routes.txt` file}
#'    \item{trips}{The GTFS `trips.txt` file}
#'    \item{stop_times}{The GTFS `stop_times.txt` file}
#'    \item{stops}{The GTFS `stops.txt` file}
#'    \item{shapes}{The GTFS `shapes.txt` file}
#'    \item{calendar}{The GTFS `calendar.txt` file}
#'    \item{calendar_dates}{The GTFS `calendar_dates.txt` file}}
#' @source <https://www.transit.land/feeds/f-dqc-wmata~bus>
#' @examples
#' # Print the tidytransit summary
#' summary(wmata_gtfs)
#'
#' # Filter by route & direction
#' my_route <- "D96"
#' my_dir <- 0
#' d96_gtfs <- filter_by_route(gtfs = wmata_gtfs,
#'                             route_ids = my_route,
#'                             dir_id = my_dir)
#'
#' # Extract route alignments
#' d96_shapes <- get_shape_geometry(gtfs = d96_gtfs)
#' print(d96_shapes)
#'
#' # Plot interactive Leaflet view
#' plot_interactive_gtfs(gtfs = wmata_gtfs,
#'                       color = "gtfs")
"wmata_gtfs"
