#' Internal function to validate inputs to trajectory prediction methods.
#'
#' Checks that the proper combination of inputs is provided. Should be one
#' of: new_times; new_distances; distance_lims AND timestep. If latter or
#' new_distances, trajectory must also have inverse function. Derivative
#' is also checked against maximum allowed.
#'
#' @param new_times A DF or vector of new time values, or `NULL`
#' @param new_distances A DF or vector of new distance values, or `NULL`
#' @param distance_lims A vector of min, max distance, or `NULL`
#' @param timestep An integer for interpolation timestep, or `NULL`
#' @param has_inv Boolean, does traj have inv fun?
#' @param deriv vector of numeric derivs to interpolate at
#' @param max_deriv Maximum derivative supported by traj fun
#' @return Throws error only if not all OK
#' @keywords internal
predict_traj_input_validation <- function(new_times, new_distances,
                                          distance_lims, timestep,
                                          has_inv, deriv, max_deriv) {

  # --- Check Input Combination ---
  # Create list of allowed input combos
  inputs <- list(new_times, new_distances, distance_lims, timestep)
  valid_inputs <- list(c(TRUE, FALSE, FALSE, FALSE),
                       c(FALSE, TRUE, FALSE, FALSE),
                       c(FALSE, FALSE, TRUE, TRUE))
  # Check if provided combo is in list of alloweds
  inputs_check <- sapply(X = inputs, FUN = function(x) !is.null(x))
  inputs_ok <- any(sapply(X = valid_inputs, FUN = identical, inputs_check))
  # If not, throw error
  if (!inputs_ok) {
    rlang::abort(message = "Invalid inputs provided. Please provide one of: new_times; or new_distances; or distance_lims AND timestep.",
                 class = "error_trajpredict_input")
  }

  # --- Check Inverse ---
  if (!has_inv) {
    # Inv required if distance_lims in use
    if (!is.null(distance_lims)) {
      rlang::abort(message = "distance_lims and timestep provided, but trajectory has no inverse function. Inverse required for these inputs.",
                   class = "error_trajpredict_input")
    }

    # Inv required if using new_distances
    if (!is.null(new_distances)) {
      rlang::abort(message = "new_distances provided, but trajectory has no inverse function. Inverse function required for this input.",
                   class = "error_trajpredict_input")
    }
  }

  # --- Check Derivative ---
  # Check that derivative is not provided with inv fun
  if ((max(deriv) > 0) & !is.null(new_distances)) {
    rlang::abort(message = "Derivative not allowed for inverse function. Considering finding timepoints first, then derivatives at timepoints.",
                 class = "error_trajpredict_input")
  }
  # If user-requested derivative is larger than function's maximum
  if (max(deriv) > max_deriv) {
    rlang::abort(message = paste("Input deriv is larger than trajectory function's maximum (",
                                 max_deriv, ").",
                                 sep = ""),
                 class = "error_trajpredict_input")
  }
  # If user-requested derivative is less than 0
  if (min(deriv) < 0) {
    rlang::abort(message = paste("Negative deriv not allowed. Please enter value between 0 and ",
                                 max_deriv, ".",
                                 sep = ""),
                 class = "error_trajpredict_input")
  }
}

#' Internal function to set up dataframe for interpolating timesteps
#' between distance limits.
#'
#' @param trajectory trajectory object
#' @param trip_extremes DF of trip time & distance extremes
#' @param distance_lims a vector of (min, max) distance
#' @param timestep time interval for interpolation
#' @param deriv vector of numeric derivs to interpolate at
#' @return DF of trip IDs & times to interpolate at
#' @keywords internal
predict_traj_setup_dist_lims <- function(trajectory, trip_extremes,
                                         distance_lims, timestep,
                                         deriv) {

  # Get observed trip limits & user-defined limits
  trip_extremes_filt <- trip_extremes %>%
    dplyr::select(-c(min_time, max_time)) %>%
    dplyr::mutate(user_min_dist = distance_lims[1],
                  user_max_dist = distance_lims[2]) %>%
    # Filter to trips whose observed ranges overlap with user-defined
    dplyr::filter((min_dist <= user_max_dist) &
                    (max_dist >= user_min_dist))

  if (dim(trip_extremes_filt)[1] == 0) {
    rlang::abort(message = "Trajectory distance range does not overlap with input distance_lims.",
                 class = "error_trajpredict_lims")
  }

  # Get min & max of user-defined and observed distance limits
  trip_absolute_extremes <- trip_extremes_filt %>%
    # Get max/min of user-defined range and observed range
    dplyr::mutate(min_time = pmax(min_dist, user_min_dist),
                  max_time = pmin(max_dist, user_max_dist)) %>%
    dplyr::select(-c(min_dist, max_dist,
                     user_min_dist, user_max_dist)) %>%
    # Pivot & add distance column
    tidyr::pivot_longer(cols = c("min_time", "max_time"),
                        names_to = "trip_end",
                        values_to = "distance")

  # Get times at distance extremes
  trip_time_extremes <- interpolate_times(trajectory = trajectory,
                                          new_dist_trips = trip_absolute_extremes) %>%
    dplyr::rename(time_extreme = interp) %>%
    dplyr::select(-distance) %>%
    tidyr::pivot_wider(values_from = "time_extreme", names_from = "trip_end")

  # For each trip, get all timesteps between the entry/exit times
  interp_times <- trip_time_extremes %>%
    # Filter out trips that do not cross one of the boundaries
    dplyr::filter(!is.na(min_time) & !is.na(max_time)) %>%
    # Group by trip
    dplyr::group_by(trip_id_performed) %>%
    # Duplicate trip row for every interpolate timepoint necessary
    tidyr::uncount(weights = floor((max_time - min_time) / timestep + 1)) %>%
    # Create interp timepoint sequence
    dplyr::mutate(event_timestamp = seq(from = min_time[1],
                                        to = max_time[1],
                                        by = timestep)) %>%
    dplyr::select(-c(max_time, min_time)) %>%
    dplyr::ungroup()

  # Uncount for derivatives
  num_derivs <- length(deriv)
  num_new_points <- dim(interp_times)[1]
  interp_deriv_times <- interp_times %>%
    tidyr::uncount(weight = num_derivs) %>%
    dplyr::mutate(deriv = rep(deriv, num_new_points))

  return(interp_deriv_times)
}

#' Internal function to set up dataframe for interpolating distances
#' from times
#'
#' @param new_times new event_timestamps to interpolate at
#' @param trip_extremes DF of trip time & distance extremes
#' @param deriv vector of numeric derivs to interpolate at
#' @return DF of trip IDs & times to interpolate at
#' @keywords internal
predict_traj_setup_new_times <- function(new_times, trip_extremes,
                                         deriv) {

  # --- Validate Input ---
  if (is.data.frame(new_times)) {
    # If DF provided
    # Check if has needed columns
    if (!("event_timestamp" %in% names(new_times))) {
      rlang::abort(message = "Column event_timestamp missing from new_times.",
                   class = "error_trajpredict_input")
    }
    # If OK...
    new_times_df <- new_times
  } else if (is.vector(new_times)) {
    new_times_df <- data.frame(event_timestamp = new_times)
  } else {
    # If not DF or vector
    rlang::abort(message = "Unrecognized new_times type. Please input either dataframe or vector.",
                 class = "error_trajpredict_input")
  }

  # --- Setup ---
  # Create DF of trip & time pairs
  trips <- trip_extremes$trip_id_performed
  if ("trip_id_performed" %in% names(new_times_df)) {
    # If DF has trip IDs, use those; filter to desired trips & appropriate ranges
    new_times_trips <- new_times_df %>%
      # Filter to input plot_trips (via trip extremes)
      dplyr::filter(trip_id_performed %in% trips) %>%
      dplyr::mutate(event_timestamp = as.numeric(event_timestamp)) %>%
      # Join extremes & filter to non-extrapolated times
      dplyr::left_join(y = trip_extremes, by = "trip_id_performed") %>%
      dplyr::filter(((event_timestamp >= min_time) & (event_timestamp <= max_time))) %>% # Remove extrapolated points
      dplyr::select(-c(min_time, max_time, min_dist, max_dist))
  } else {
    # If DF doesn't have trip IDs, duplicate times for all trips & filter to appropraite ranges
    num_times <- dim(new_times_df)[1]
    num_trips <- dim(trip_extremes)[1]
    new_times_trips <- new_times_df %>%
      dplyr::mutate(event_timestamp = as.numeric(event_timestamp)) %>%
      # Duplicate for all trip IDs
      tidyr::uncount(weights = num_trips) %>%
      dplyr::mutate(trip_id_performed = rep(trips, num_times)) %>%
      # Join trip extremes & filter to non-extrapolated times
      dplyr::left_join(y = trip_extremes, by = "trip_id_performed") %>%
      dplyr::filter(((event_timestamp >= min_time) & (event_timestamp <= max_time))) %>% # Remove extrapolated points
      dplyr::select(-c(min_time, max_time, min_dist, max_dist))
  }

  # Check that observations remain
  if (dim(new_times_trips)[1] == 0) {
    rlang::abort(message = "No trips within range of new_times.",
                 class = "error_trajpredict_range")
  }

  # Add deriv column -- uncount for each deriv requested
  num_derivs <- length(deriv)
  num_new_points <- dim(new_times_trips)[1]
  new_times_trips_derivs <- new_times_trips %>%
    tidyr::uncount(weight = num_derivs) %>%
    dplyr::mutate(deriv = rep(deriv, num_new_points))

  return(new_times_trips_derivs)
}

#' Internal function to set up dataframe for interpolating times
#' from distances
#'
#' @param new_distances new distances to interpolate at
#' @param trip_extremes DF of trip time & distance extremes
#' @return DF of trip IDs & distances to interpolate at
#' @keywords internal
predict_traj_setup_new_dists <- function(new_distances, trip_extremes) {

  # --- Validate Input ---
  if (is.data.frame(new_distances)) {
    # If DF provided
    # Check if has needed columns
    if (!("distance" %in% names(new_distances))) {
      rlang::abort(message = "Column distance missing from new_distances.",
                   class = "error_trajpredict_input")
    }
    # If OK...
    new_distances_df <- new_distances
  } else if (is.vector(new_distances) & is.numeric(new_distances)) {
    new_distances_df <- data.frame(distance = new_distances)
  } else {
    # If not DF or vector
    rlang::abort(message = "Unrecognized new_distances type. Please input either dataframe or numeric vector.",
                 class = "error_trajpredict_input")
  }

  # --- Setup ---
  # Create DF of trip & dist pairs
  trips <- trip_extremes$trip_id_performed

  if ("trip_id_performed" %in% names(new_distances_df)) {
    # If DF contains trip IDs, use those; filter to desired trips & ranges
    new_distances_trips <- new_distances_df %>%
      dplyr::filter(trip_id_performed %in% trips) %>%
      # Join extremes & filter to non-extrapolated times
      dplyr::left_join(y = trip_extremes, by = "trip_id_performed") %>%
      dplyr::filter(((distance >= min_dist) & (distance <= max_dist))) %>% # Remove extrapolated points
      dplyr::select(-c(min_time, max_time, min_dist, max_dist))
  } else {
    # If DF does not contain trip IDs, duplicate distances for all trips & filter
    num_dists <- dim(new_distances_df)[1]
    num_trips <- dim(trip_extremes)[1]
    new_distances_trips <- new_distances_df %>%
      tidyr::uncount(weights = num_trips) %>%
      dplyr::mutate(trip_id_performed = rep(trips, num_dists)) %>%
      dplyr::left_join(y = trip_extremes, by = "trip_id_performed") %>%
      dplyr::filter(((distance >= min_dist) & (distance <= max_dist))) %>% # Remove extrapolated points
      dplyr::select(-c(min_time, max_time, min_dist, max_dist))
  }

  # Check that observations remain
  if (dim(new_distances_trips)[1] == 0) {
    rlang::abort(message = "No trips within range of new_distances.",
                 class = "error_trajpredict_range")
  } else {
    return(new_distances_trips)
  }
}
