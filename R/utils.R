#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Set global variables to use throughout, silencing notes during check.
#'
#' These variables are generally those expected in the standard data formats
#' used, such as GTFS and TIDES. Whenever one of these variable names must be
#' used, checks are performed to ensure they are present in the input data.
#'
#' @name set_globals
utils::globalVariables(c(
  # GTFS
  "agency_id", "service_id", "roud_id", "stop_id", "stop_lat", "stop_lon",
  "stop_sequence", "trip_id", "shape_pt_lat", "shape_pt_lon",
  # TIDES
  "trip_id_performed", "event_timestamp", "vehicle_id", "location_ping_id",
  "operator_id", "speed", "latitude", "longitude"
  ))

#' Calculates numerical inverse of a trajectory function
#'
#' Not intended for external use
#'
#' @param f Direct traj function
#' @param lower lower distance range
#' @param upper upper distance range
#' @param inv_tol tolerance for numeric inverse
#' @return function for inverse trajectory
get_inverse_traj <- function(f, lower, upper, inv_tol) {
  Vectorize(function(distance) {
    stats::uniroot(f = function(x) {f(x) - distance},
            lower = lower, upper = upper, tol = inv_tol)$root
  })
}
