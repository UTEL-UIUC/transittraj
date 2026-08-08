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
#' @keywords internal
utils::globalVariables(c(
  # GTFS
  "agency_id", "service_id", "route_id", "stop_id", "stop_lat", "stop_lon",
  "stop_sequence", "trip_id", "shape_pt_lat", "shape_pt_lon",
  "departure_time", "arrival_time", "direction_id", "shape_id", "stop_name",
  "shape_pt_sequence", "exception_type",
  # TIDES
  "trip_id_performed", "event_timestamp", "vehicle_id", "location_ping_id",
  "operator_id", "speed", "latitude", "longitude", "distance",
  # Internal to transittraj functions
  "delta_dist", "delta_time", "max_dist", "min_dist", "max_time", "min_time",
  "trip_distance", "trip_distance", "duration", "max_dist_gap", "max_t_gap",
  "dist_ok", "dist_gap_ok", "t_gap_ok", "dur_ok", "all_ok", "window_med",
  "window_mad", "is_implosion", "is_tail", "med_dist", "mad_ok", "dev_ok",
  "ignore_observation", "all_ok", "n_veh", "n_oper", "remove_trip", "n_obs",
  "t_start", "t_end", "t_interval", "time_range", "n_subtrips_in_range",
  "subtrip", "file_provided_status", "dwell_time", "hour_num", "monotonic_dist",
  "constant_id", "row_index", "initial_adjustment", "run_length", "target_dist",
  "target_max", "correction_applied", "final_distance", "time_sec",
  "replace_na", "corrected_implied_speed", "fc_delta", "initial_distance",
  "final_speed", "initial_speed", "interp", "x", "route_color", "gtfs_stops",
  "min_dist_index", "max_dist_index", "before_min", "after_max", "remove_trip",
  "obs_ok", "field", "field_provided_status", "is_weak", "is_strict",
  "required_field", "fc_alpha", "fc_beta", "sum_sq", "is_fc_speed",
  "field_type_ok", "field_present", "x_spatial", "y_spatial", "stp_time",
  "point_geom", "distance_lims", "excep_id", "sched_id", "wkday",
  "user_min_dist", "user_max_dist", "deriv", "distance_change", "speed_change",
  "is_traj", "traj_type", "max_deriv", "used_speeds", "is_inv", "inv_tol",
  # Exported datasets
  "lacmta_avl", "lacmta_gtfs"
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
#' @keywords internal
get_inverse_traj <- function(f, lower, upper, inv_tol) {
  Vectorize(function(distance) {
    stats::uniroot(f = function(x) {f(x) - distance},
            lower = lower, upper = upper, tol = inv_tol)$root
  })
}

#' Corrects speeds to Fristch-Carlson constraints, recursively.
#'
#' Internal function. Not intended for external use.
#'
#' @param m_0 A numeric vector of initial slopes (observed velocities)
#' @param deltas A numeric vector of initial FC delta values
#' @return A numeric vector of m_0 adjusted to FC constraints
#' @keywords internal
correct_speeds_fun <- function(m_0, deltas) {

  # validate
  if (length(m_0) < 2) {
    rlang::abort(message = "Must have at least two observations to correct speeds.",
                 class = "error_avlclean_fc")
  }

  # Algorithm is recursive -- loop through each of m_0
  for (iter in 1:(length(m_0) - 1)) {
    # Get initial values
    m_i = m_0[iter]
    m_i1 = m_0[iter + 1]
    delta_i = deltas[iter]

    # Calculate FC params
    alpha_i = m_i / delta_i
    beta_i = m_i1 / delta_i
    ab_sq = (alpha_i^2) + (beta_i^2)

    if (ab_sq > 9) {
      # If FC constraint not satisfied
      tau_i = 3 / sqrt(ab_sq)

      new_m_i = tau_i * alpha_i * delta_i
      new_m_i1 = tau_i * beta_i * delta_i

      # Replace slope
      m_0[iter] <- new_m_i
      m_0[iter + 1] <- new_m_i1
    }
    # Otherwise, can leave slope as is
  }
  return(m_0)
}

#' Retrieve an object from a particular step of `transittraj`'s workflow
#'
#' This function runs `transittraj`'s AVL cleaning and trajectory reconstruction
#' workflow up until a certain point (as defined by `func_output`), then returns
#' the object at that point. A subset of the `lacmta_avl` dataset is used.
#' This is primarily intended for use in testing and examples. The workflow
#' applied here is the same as what is in
#' `vignette("articles/data-workflow-la")`.
#'
#' @param func_output The `transittraj` function to return an output for. Should
#' be a string corresponding to the function name. Default is `NULL`, where a
#' vector of allowed inputs will be returned.
#' @return The object returned by the specified function.
#' @export
#' @examples
#' # Get AVL data after projection onto route
#' lineE_dists <- new_transittraj_data("get_linear_distances")
#' head(lineE_dists)
#'
#' # Get a full, fit trajectory
#' lineE_traj <- new_transittraj_data("get_trajectory_fun")
#' summary(lineE_traj)
new_transittraj_data <- function(func_output = NULL) {

  # Define allowed steps
  allowed_steps <- c("filter_by_route",
                     "lineE_avl",
                     "get_shape_geometry",
                     "get_linear_distances",
                     "clean_overlapping_subtrips",
                     "clean_jumps",
                     "clean_incomplete_trips",
                     "trim_trips",
                     "make_monotonic",
                     "get_trajectory_fun",
                     "get_trajectory_fun_single")
  if (is.null(func_output)) {
    return(allowed_steps)
  } else if (!(func_output %in% allowed_steps)) {
    rlang::abort(message = "Unknown step. Run new_transittraj_data() for list of allowed inputs.",
                 class = "error_datahelper_input")
  }

  # - filter_by_route -
  filt_dir <- 0 # 0 is EB, 1 is WB
  lineE_id <- "804" # the internal route ID for Line E
  lineE_avl <- lacmta_avl %>%
    dplyr::filter((route_id == lineE_id) & (direction_id == filt_dir))
  lineE_gtfs <- filter_by_route(gtfs = lacmta_gtfs,
                                route_ids = lineE_id,
                                dir_id = filt_dir)
  if (func_output == "filter_by_route") {
    return(lineE_gtfs)
  }
  if (func_output == "lineE_avl") {
    return(lineE_avl)
  }

  # - get_shape_geometry -
  lineE_EB_shape_id <- "804EB_RC_221121"
  la_CRS <- 32611
  lineE_shape <- get_shape_geometry(gtfs = lineE_gtfs,
                                    shape = lineE_EB_shape_id,
                                    project_crs = la_CRS)
  if (func_output == "get_shape_geometry") {
    return(lineE_shape)
  }

  # - get_linear_distances -
  buffer = 50 # meters
  lineE_distances <- get_linear_distances(avl_df = lineE_avl,
                                          shape_geometry = lineE_shape,
                                          project_crs = la_CRS,
                                          clip_buffer = buffer)
  if (func_output == "get_linear_distances") {
    return(lineE_distances)
  }

  # - clean_overlapping_subtrips -
  lineE_check_op <- FALSE
  lineE_remove_singles <- TRUE
  lineE_remove_non_overlap <- FALSE
  lineE_cleaned_subtrips <- clean_overlapping_subtrips(
    distance_df = lineE_distances,
    check_operator = lineE_check_op,
    remove_single_observations = lineE_remove_singles,
    remove_non_overlapping = lineE_remove_non_overlap
  )

  if (func_output == "clean_overlapping_subtrips") {
    return(lineE_cleaned_subtrips)
  }

  # - clean_jumps -
  lineE_max_jump <- 80 # meters
  lineE_min_jump <- -1 * lineE_max_jump # meters
  lineE_no_jumps <- clean_jumps(distance_df = lineE_cleaned_subtrips,
                                max_median_deviation = lineE_max_jump,
                                min_median_deviation = lineE_min_jump,
                                t_cutoff = Inf)

  if (func_output == "clean_jumps") {
    return(lineE_no_jumps)
  }

  # - clean_incomplete_trips -
  lineE_min_dist <- 1000 # meters
  lineE_min_time <- 120 # seconds
  lineE_max_gap <- 1000 # meters
  lineE_cleaned_incompletes <- clean_incomplete_trips(
    distance_df = lineE_no_jumps,
    min_trip_distance = lineE_min_dist,
    min_trip_duration = lineE_min_time,
    max_distance_gap = lineE_max_gap
  )
  if (func_output == "clean_incomplete_trips") {
    return(lineE_cleaned_incompletes)
  }

  # - trim_trips -
  lineE_trim_type <- "both"
  lineE_trimmed <- trim_trips(distance_df = lineE_cleaned_incompletes,
                              trim_type = lineE_trim_type)
  if (func_output == "trim_trips") {
    return(lineE_trimmed)
  }

  # - make_monotonic -
  lineE_dist_error <- 0.001
  lineE_correct_speeds <- TRUE
  lineE_mono <- make_monotonic(distance_df = lineE_trimmed,
                               correct_speed = lineE_correct_speeds,
                               add_distance_error = lineE_dist_error)
  if (func_output == "make_monotonic") {
    return(lineE_mono)
  }

  # - get_trajectory_fun - grouped
  lineE_traj <- get_trajectory_fun(distance_df = lineE_mono,
                                   interp_method = "monoH.FC",
                                   use_speeds = TRUE,
                                   find_inverse_function = TRUE)
  if (func_output == "get_trajectory_fun") {
    return(lineE_traj)
  }

  # - get_trajectory_fun - single
  lineE_traj_s <- get_trajectory_fun(distance_df = lineE_mono,
                                   interp_method = "monoH.FC",
                                   use_speeds = TRUE,
                                   find_inverse_function = TRUE,
                                   return_group_function = FALSE)
  if (func_output == "get_trajectory_fun_single") {
    return(lineE_traj_s)
  }
}
