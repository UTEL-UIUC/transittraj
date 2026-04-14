#' Validate inputs to predict trajectory methods & set up dataframes.
#'
#' Checks that the input `new_times` and `new_distances` to
#' `predict.avltrajectory_group()` and `predict.avltrajectory_single()`
#' meet requirements of these functions, and sets up dataframes for
#' interpolation. Factoring intended to reduce code duplication.
#'
#' @param new_times DF or vector of new times
#' @param new_distances DF or vector of new distances
#' @param distance_lims A vector of distance limits
#' @param timestep An integer for the time interval between each step
#' @return List of `new_times_df` and `new_distances_df`
#' @keywords internal
predict_traj_input_setup <- function(new_times, new_distances,
                                     distance_lims, timestep) {

  # --- Check Inputs ---
  # Create list of allowed input combos
  inputs <- list(new_times, new_distances, distance_lims, timestep)
  valid_inputs <- list(c(TRUE, FALSE, FALSE, FALSE),
                       c(FALSE, TRUE, FALSE, FALSE),
                       c(FALSE, FALSE, TRUE, TRUE))
  # Check if provided combo is in list of alloweds
  inputs_check <- sapply(X = valid_inputs, FUN = is.null)
  inputs_ok <- any(sapply(X = valid_inputs, FUN = identical, inputs_check))
  # If not, throw error
  if (!inputs_ok) {
    rlang::abort(message = "Invalid inputs provided. Please provide one of: new_times; or new_distances; or distance_lims AND timestep.",
                 class = "error_trajpredict_input")
  }

  # Initialize final DFs as NULLs; they will be overridden if requirements
  # are met
  new_times_df <- NULL
  new_distances_df <- NULL

  # Check DFs provided
  if ((!is.null(new_times)) & (!is.null(new_distances))) {
    # Only allow one of time or distance
    rlang::abort(message = "Please provide only one of new_times or new_distances.",
                 class = "error_trajpredict_input")
  } else if (!is.null(distance_lims) & is.null()) {

  } else if (all(is.null(new_times), is.null(new_distances),
                 is.null(distance_lims), is.null(timestep))) {
    # Require one of new_times or new_distances
    rlang::abort(message = "Please provide one of: new_times; or new_distances; or distance_lims AND timestep.",
                 class = "error_trajpredict_input")
  }

  # Build DFs for interpolation
  if (!is.null(new_times)) { # If interpolating for distance from new times

    # Convert data types
    if (is.vector(new_times)) {
      # If a vector is provided, convert it to dataframe
      new_times_df <- data.frame(event_timestamp = new_times)
    } else if (is.data.frame(new_times)) {
      # If a DF is provided
      if (!("event_timestamp" %in% names(new_times))) {
        # Check if new_times has needed column
        rlang::abort(message = "Please provide event_timestamp column in new_times",
                     class = "error_trajpredict_input")
      } else {
        new_times_df <- new_times
      }
    } else {
      rlang::abort(message = "Please provide either a vector or dataframe of new_times.",
                   class = "error_trajpredict_input")
    }
  } else { # If interpolating for times from new distances

    # Convert data types
    if (is.vector(new_distances)) {
      # If a vector is provided, convert it to dataframe
      new_distances_df <- data.frame(distance = new_distances)
    } else if (is.data.frame(new_distances)) {
      # If a DF is provided
      if (!("distance" %in% names(new_distances))) {
        # Check if new_distances has needed column
        rlang::abort(message = "Please provide distance column in new_distances",
                     class = "error_trajpredict_input")
      } else {
        new_distances_df <- new_distances
      }
    } else {
      rlang::abort(message = "Please provide either a vector or dataframe of new_distances.",
                   class = "error_trajpredict_input")
    }
  }

  return(list(new_times_df, new_distances_df))
}

#' Get the distance and time range of each trip in a trajectory object.
#'
#' This function extracts the time and distance ranges stored in a trajectory
#' object and formats them into a dataframe for each use. The dataframe can
#' be filtered to a desired set of `trip_id_performed`s.
#'
#' @param trajectory A trajectory object.
#' @param filter_trips Optional. A vector of `trip_id_performed`s to filter the
#' dataframe to. At least one must of `filter_trips` must be present in
#' `trajectory`. Default is `NULL`, where all `trip_id_performed`s in
#' `trajectory` are returned.
#' @return A dataframe with the columns `trip_id_performed`, `min_time`,
#' `max_time`, `min_dist`, and `max_dist`.
#' @export
#' @examples
#' # Get input data
#' c53_traj <- new_transittraj_data("get_trajectory_fun")
#'
#' # Run function
#' c53_extremes <- get_trip_extremes(c53_traj)
#' print(c53_extremes)
get_trip_extremes <- function(trajectory, filter_trips = NULL) {

  if (!("avltrajectory_group" %in% class(trajectory))) {
    rlang::abort(messge = "Unrecognized trajectory object. Please input a trajectory object from `get_trajectory_fun()`.",
                 class = "error_trajextremes_input")
  }

  trip_extremes <- data.frame(trip_id_performed = unclass(trajectory),
                              min_dist = attr(trajectory, "min_dist"),
                              max_dist = attr(trajectory, "max_dist"),
                              min_time = attr(trajectory, "min_time"),
                              max_time = attr(trajectory, "max_time"))

  if (!is.null(filter_trips)) {
    trip_extremes_filt <- trip_extremes %>%
      dplyr::filter(trip_id_performed %in% filter_trips)

    if (dim(trip_extremes_filt)[1] == 0) {
      rlang::abort(message = "Desired trips not found in trajectory object.",
                   class = "error_trajextremes_input")
    }
    return(trip_extremes_filt)
  } else {
    return(trip_extremes)
  }
}

#' Distance interpolation for group trajectories
#'
#' Not intended for external use.
#'
#' @param trip_extremes DF of max and min distance values
#' @param new_times DF of new time points, not paired with trips
#' @param new_times_trips DF of already-paired trip IDs & timepoints
#' @param trajectory_function trajectory function list
#' @param deriv derivative to use
#' @return DF of interpolated values
#' @keywords internal
interpolate_distances_group <- function(trip_extremes = NULL, new_times = NULL,
                                        new_times_trips = NULL,
                                        trajectory_function, deriv) {

  # If new_times_trips not provided, create it -- only not true for plot_trips_df_setup
  if (is.null(new_times_trips)) {
    trips <- trip_extremes$trip_id_performed
    num_times <- dim(new_times)[1]
    num_trips <- dim(trip_extremes)[1]
    new_times_trips <- new_times %>%
      dplyr::mutate(event_timestamp = as.numeric(event_timestamp)) %>%
      tidyr::uncount(weights = num_trips) %>%
      dplyr::mutate(trip_id_performed = rep(trips, num_times)) %>%
      dplyr::left_join(y = trip_extremes, by = "trip_id_performed") %>%
      dplyr::filter(((event_timestamp >= min_time) & (event_timestamp <= max_time))) %>% # Remove extrapolated points
      dplyr::select(-c(min_time, max_time, min_dist, max_dist))
  }

  # Check that there are trips
  if (dim(new_times_trips)[1] == 0) {
    rlang::abort("No trips provided within the range of new_times.",
                 class = "error_trajpredict_trips")
  }

  if (deriv == 0) {
    # If deriv is 0, do not pass it
    # Deriv should always default to 0. If function does not take in deriv at all, we would get an error if trying to pass it
    int_df <- new_times_trips %>%
      dplyr::mutate(interp = purrr::map2_dbl(trip_id_performed, event_timestamp,
                                             function(trip_id_performed, event_timestamp) {
                                               trajectory_function[[trip_id_performed]](event_timestamp) }))
  } else {
    int_df <- new_times_trips %>%
      dplyr::mutate(interp = purrr::map2_dbl(trip_id_performed, event_timestamp,
                                             function(trip_id_performed, event_timestamp) {
                                               trajectory_function[[trip_id_performed]](event_timestamp,
                                                                                        deriv = deriv) }))
  }

  return(int_df)
}

#' Distance interpolation for single trajectories
#'
#' Not intended for external use.
#'
#' @param trip_extremes DF of max and min distance values
#' @param new_times DF of new time points
#' @param trajectory_function trajectory function
#' @param deriv derivative to use
#' @return DF of interpolated values
#' @keywords internal
interpolate_distances_single <- function(trip_extremes, new_times, trajectory_function, deriv) {
  # Filter to allowed range
  filt_df <- new_times %>%
    dplyr::mutate(event_timestamp = as.numeric(event_timestamp)) %>%
    dplyr::filter((event_timestamp >= trip_extremes["min_time"]) &
                    (event_timestamp <= trip_extremes["max_time"]))

  if (dim(filt_df)[1] == 0) {
    # Check that points remain
    rlang::abort("Trip not within the range of new_times.",
                 class = "error_trajpredict_trips")
  }

  if (deriv == 0) {
    # Interpolate
    int_df <- filt_df %>%
      dplyr::mutate(interp = trajectory_function(event_timestamp))
  } else {
    # Interpolate
    int_df <- filt_df %>%
      dplyr::mutate(interp = trajectory_function(event_timestamp,
                                                 deriv = deriv))
  }

  return(int_df)
}

#' Time interpolation for grouped trajectories
#'
#' Not intended for external use.
#'
#' @param trip_extremes DF of max and min time values
#' @param new_distances DF of new distance points
#' @param new_dist_trips DF of new trip & distance pairs
#' @param inv_trajectory_function Inverse trajectory function list
#' @return DF of interpolated values
#' @keywords internal
interpolate_times_group <- function(trip_extremes = NULL, new_distances = NULL,
                                    new_dist_trips = NULL, inv_trajectory_function) {

  # If new_dist_trips not provided, create it -- only not true for plot_trips_df_setup
  if (is.null(new_dist_trips)) {
    trips <- trip_extremes$trip_id_performed
    num_dist <- dim(new_distances)[1]
    num_trips <- dim(trip_extremes)[1]
    new_dist_trips <- new_distances %>%
      tidyr::uncount(weights = num_trips) %>%
      dplyr::mutate(trip_id_performed = rep(trips, num_dist)) %>%
      dplyr::left_join(y = trip_extremes, by = "trip_id_performed") %>%
      dplyr::filter(((distance >= min_dist) & (distance <= max_dist))) %>% # Remove extrapolated points
      dplyr::select(-c(min_time, max_time, min_dist, max_dist))
  }

  if (dim(new_dist_trips)[1] == 0) {
    rlang::abort("No trips provided within the range of new_distances.",
                 class = "error_trajpredict_trips")
  }

  int_df <- new_dist_trips %>%
    dplyr::mutate(interp = purrr::map2_dbl(trip_id_performed, distance,
                                           function(trip_id_performed, distance) {
                                             inv_trajectory_function[[trip_id_performed]](distance)}))

  return(int_df)
}

#' Time interpolation for single trajectories
#'
#' Not intended for external use.
#'
#' @param trip_extremes DF of max and min time values
#' @param new_distances DF of new distance points
#' @param inv_trajectory_function Inverse trajectory function
#' @return DF of interpolated values
#' @keywords internal
interpolate_times_single <- function(trip_extremes, new_distances, inv_trajectory_function) {
  # Filter to allowed range
  filt_df <- new_distances %>%
    dplyr::filter((distance >= trip_extremes["min_dist"]) &
                    (distance <= trip_extremes["max_dist"]))

  if (dim(filt_df)[1] == 0) {
    # Check that points remain
    rlang::abort("Trip not within the range of new_distances",
                 class = "error_trajpredict_trips")
  }

  # Interpolate
  int_df <- filt_df %>%
    dplyr::mutate(interp = inv_trajectory_function(distance))
  return(int_df)
}

#' Interpolate time or distance points using AVL trajectories.
#'
#' @description
#' Using a function stored in a grouped or single trajectory object, new points
#' will be interpolated along a trajectory. Depending on whether new_times or
#' new_distances is provided, the function will utilize the direct or inverse
#' trajectory function.
#'
#' @details
#' This function is the recommended way to use a fit trajectory function. It has
#' a few key features:
#'
#' ## Interpolate for Distance or Time
#'
#' If `new_times` is provided, the function will find the `distance` of each
#' trip at each new time using the direct trajectory function. Conversely, if
#' `new_distances` is provided, the function will find the `event_timestamp` at
#' which each trip crossed that distance using the inverse trajectory function.
#' If an input trajectory object does not contain an inverse function, an error
#' will be thrown.
#'
#' These new points can be either vectors or dataframes:
#'
#' - If the input is a vector, the returned value will be a dataframe
#' associating each trip with each new point and a column `interp` indicating
#' the interpolated value.
#'
#' - If the input is a dataframe, it must contain a column names
#' `event_timestamp` or `distance`, depending on whether it is put into
#' `new_times` or `new_distances`. All other columns will be preserved in the
#' output. Each row will be duplicated for each trip, and a column `interp` will
#' be added indicating the interpolated value.
#'
#' The latter option is particularly useful for finding crossing times of
#' particular features, or the positions at notable points in time. For
#' instance, `new_distances` may be a dataframe of corridors, with a column
#' `name` for the corridor name and `inout` for whether each row is the
#' entrance or exit to the corridor. The returned value will append the column
#' `trip_id_performed` for each trip that crosses those distances, and `interp`
#' for the time the trip passes the entrance or exit.
#'
#' ## Finding Derivatives
#'
#' Depending on the `interp_method` used when fitting the trajectory object, a
#' its derivative may be able to be found:
#'
#' - `interp_method = "linear"`. This will not allow derivatives. This is
#' because, at each observation, the piecewise linear function is not
#' differentiable.
#'
#' - `interp_method` is a spline from `stats::splinefun()`. This will typically
#' be differentiable up to the third degree.
#'
#' The derivative returned (as column `interp`) is the derivative of distance
#' with respect to time. This means the first derivative is velocity, second is
#' acceleration, and third is jerk. The derivative is taken from the direct
#' trajectory, not the inverse, and the inverse trajectory cannot be used to
#' find derivatives. This means that if `new_distances` is provided, `deriv `
#' must equal 0. If starting from distance values, but derivatives are desired,
#' consider interpolating for timepoints first, then using these as `new_times`
#' to find the derivative.
#'
#' ## Prevents Extrapolation
#'
#' By default, many fit interpolating curves will allow extrapolation (i.e.,
#' the input of an `event_timestamp` beyond the original time domain of the
#' trip). This is especially true for splines fit using `stats::splinefun()`.
#' In general, this will not be reasonable for transit vehicles: time points
#' should be constrained by the time that a trip has actually been observed,
#' and distances should be constrained to the part of a route a trip actually
#' ran.
#'
#' This function uses the maximum and minimum time and distance values stored
#' in the trajectory object to identify if an input `new_times` or
#' `new_distances` is beyond the domain/range of each trip individually. The
#' returned output will only include `interp` values for trips within the
#' domain/range of the input.
#'
#' ## Accessing the Raw Trajectory Function
#'
#' Because of the above features and protections, it is recommend that these
#' `predict()` functions are used to access the fit trajectory and inverse
#' trajectory functions. However, if the raw function itself is desired,
#' it can be accessed using `attr(trajectory, "traj_fun")` or
#' `attr(trajectory, "inv_traj_fun")`. For a group trajectory object, these
#' will return lists of individual trip functions indexed by
#' `trip_id_performed`; for single trajectory objects, these will return the
#' single function for that trip.
#'
#' @param object The single or grouped trajectory object.
#' @param new_times Optional. A vector of numeric timepoints, or a dataframe
#' with at least the column `"event_timestamp"` of new timepoints to interpolate
#' at. Default is `NULL`.
#' @param new_distances Optional. A vector of numeric distances, or a dataframe
#' with at least the column `"distance"` of new distances to interpolate at.
#' Default is `NULL`.
#' @param deriv Optional. The derivative with which to calculate at. Default is
#' 0.
#' @param trips Optional. A vector of `trip_id_performed`s to interpolate for.
#' Default is `NULL`, which will use all trips found in the trajectory object.
#' @param ... Other parameters (not used).
#' @return The input dataframe, with an additional column `"interp"` of the
#' interpolated values requested, and an additional `"trip_id_performed"`
#' column will all trips for which that point is within range.
#' @export
#' @examples
#' # Set my parameters
#' my_times = seq(from = 1771260000,
#'                to = 1771264000,
#'                by = 180)
#' my_distances = seq(from = 0,
#'                    to = 15000,
#'                    by = 1000)
#'
#' # Get input data
#' c53_traj <- new_transittraj_data("get_trajectory_fun")
#'
#' # Run function: get distances from times
#' interp_dists <- predict(object = c53_traj,
#'                         new_times = my_times)
#' dim(interp_dists)
#' head(interp_dists)
#'
#' # Run function: get speeds from times
#' interp_speeds <- predict(object = c53_traj,
#'                          new_times = my_times,
#'                          deriv = 1)
#' dim(interp_speeds)
#' head(interp_speeds)
#'
#' # Run function: get times from distances
#' interp_times <- predict(object = c53_traj,
#'                         new_distances = my_distances)
#' dim(interp_times)
#' head(interp_times)
predict.avltrajectory_group <- function(object, new_times = NULL, new_distances = NULL,
                                        distance_lims = NULL, timestep = NULL,
                                        deriv = 0, trips = NULL, ...) {

  # --- Validation ---
  # Validate & format input DFs
  df_list <- predict_traj_input_setup(new_times = new_times,
                                      new_distances = new_distances,
                                      distance_lims = distance_lims,
                                      timestep = timestep)
  new_times_df <- df_list[[1]]
  new_distances_df <- df_list[[2]]

  # Validate trips input
  all_trips <- unclass(object)
  if (is.null(trips)) {
    # If not provided, use all
    trips <- all_trips
  } else {
    # If trips are provided, check that they are in traj functions
    trips_check <- trips %in% all_trips
    if (!all(trips_check)) {
      # If at least one trip is not supported by the function
      rlang::abort(message = paste(c("The following requested trips are not in this trajectory function:\n",
                                     trips[!trips_check]), collapse = " "),
                   class = "error_trajpredict_trips")
    }
  }

  # --- Interpolation ---
  # Get required data for all methods
  trip_extremes <- get_trip_extremes(trajectory = object,
                                     filter_trips = trips)

  if (!is.null(new_times)) {
    # If interpolating for distances from new times

    # Check deriv
    if (deriv > attr(object, "max_deriv")) {
      # Check if derivative is above function's allowed limit
      rlang::abort(message = paste("Derivative too high. Maximum for this function is ",
                                   attr(object, "max_deriv"), sep = ""),
                   class = "error_trajpredict_input")
    }

    # Function
    trajectory_function <- attr(object, "traj_fun")

    # Call internal function to do interpolation
    interpolate_distances_group(trip_extremes = trip_extremes,
                                new_times = new_times_df,
                                trajectory_function = trajectory_function,
                                deriv = deriv)
  } else {
    # If interpolating for times from new distances
    if (deriv > 0) {
      # Check if derivative provided
      rlang::abort(message = "Derivative not allowed for inverse function. Please find timepoints first, then derivatives at timepoints.",
                   class = "error_trajpredict_input")
    }

    # Get inverse trajectory function
    inv_trajectory_function <- attr(object, "inv_traj_fun")
    if (is.null(inv_trajectory_function)) {
      # Check that inverse function actually exists
      rlang::abort(message = "Trajectory object does not contain inverse function. Please create one using get_trajectory_function().",
                   class = "error_trajpredict_input")
    }

    # Call internal function to do interpolation
    interpolate_times_group(trip_extremes = trip_extremes,
                            new_distances = new_distances_df,
                            inv_trajectory_function = inv_trajectory_function)
  }
}

#' @rdname predict.avltrajectory_group
#' @export
predict.avltrajectory_single <- function(object, new_times = NULL, new_distances = NULL,
                                         deriv = 0, ...) {

  # --- Validation ---
  df_list <- predict_traj_input_setup(new_times = new_times,
                                      new_distances = new_distances)
  new_times_df <- df_list[[1]]
  new_distances_df <- df_list[[2]]

  # --- Interpolation ---
  # Dist & time extremes
  trip_extremes <- c("min_dist" = attr(object, "min_dist"),
                     "max_dist" = attr(object, "max_dist"),
                     "min_time" = attr(object, "min_time"),
                     "max_time" = attr(object, "max_time"))

  if (!is.null(new_times)) {
    # If interpolating for distances from new times

    # Check derivative value
    if (deriv > attr(object, "max_deriv")) {
      # Check if derivative is above function's allowed limit
      rlang::abort(message = paste("Derivative too high. Maximum for this function is ",
                                   attr(object, "max_deriv"), sep = ""),
                   class = "error_trajpredict_input")
    }

    # Function
    trajectory_function <- attr(object, "traj_fun")

    # Call internal function to do interpolation
    interpolate_distances_single(trip_extremes = trip_extremes,
                                 new_times = new_times_df,
                                 trajectory_function = trajectory_function,
                                 deriv = deriv)
  } else {
    # If interpolating for times from new distance values

    # Check if derivative provided
    if (deriv > 0) {
      rlang::abort(message = "Derivative not allowed for inverse function. Please find timepoints first, then derivatives at timepoints.",
                   class = "error_trajpredict_input")
    }

    # Get inverse trajectory function
    inv_trajectory_function <- attr(object, "inv_traj_fun")
    if (is.null(inv_trajectory_function)) {
      # Check that inverse function actually exists
      rlang::abort(message = "Trajectory object does not contain inverse function. Please create one using get_trajectory_function().",
                   class = "error_trajpredict_input")
    }

    # Call internal function to do interpolation
    interpolate_times_single(trip_extremes = trip_extremes,
                             new_distances = new_distances_df,
                             inv_trajectory_function = inv_trajectory_function)
  }
}
