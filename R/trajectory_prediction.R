#' Get the distance and time range of each trip in a trajectory object
#'
#' This function extracts the time and distance ranges stored in a trajectory
#' object and formats them into a dataframe. The dataframe can
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
#' lineE_traj <- new_transittraj_data("get_trajectory_fun")
#'
#' # Run function
#' lineE_extremes <- get_trip_extremes(lineE_traj)
#' print(lineE_extremes)
get_trip_extremes <- function(trajectory, filter_trips = NULL) {

  # --- Validation ---
  # Is traj
  if (!("avltrajectory_group" %in% class(trajectory))) {
    rlang::abort(message = "Unrecognized trajectory object. Please input a trajectory object from `get_trajectory_fun()`.",
                 class = "error_trajextremes_input")
  }

  # Validate trips input: If filter_trips are provided, check that they are in traj functions
  if (!is.null(filter_trips)) {
    all_trips <- as.vector(trajectory)
    trips_check <- filter_trips %in% all_trips

    if (!all(trips_check)) {
      # If at least one trip is not supported by the function
      rlang::abort(message = paste(c("The following requested trips are not in this trajectory function:\n",
                                     filter_trips[!trips_check]), collapse = " "),
                   class = "error_trajextremes_input")
    }
  }

  # --- Get extremes ---
  trip_extremes <- data.frame(trip_id_performed = as.vector(trajectory),
                              min_dist = attr(trajectory, "min_dist"),
                              max_dist = attr(trajectory, "max_dist"),
                              min_time = attr(trajectory, "min_time"),
                              max_time = attr(trajectory, "max_time"))

  if (!is.null(filter_trips)) {
    trip_extremes_filt <- trip_extremes %>%
      dplyr::filter(trip_id_performed %in% filter_trips)
    return(trip_extremes_filt)
  } else {
    return(trip_extremes)
  }
}

#' Interpolate time or distance points using AVL trajectories
#'
#' @description
#' This function uses a fit interpolating curve stored in a grouped or single
#' trajectory object to find new points along each trip's trajectory.
#' Depending on whether `new_times` or
#' `new_distances` is provided, the function will utilize the direct or inverse
#' trajectory function.
#'
#' @details
#' This function is the recommended way to use a fit trajectory function. It has
#' a few key features:
#'
#' ## Interpolation
#'
#' There are three ways to interpolate: finding distance from times (direct
#' trajectory function), times from distance (inverse trajectory function),
#' or timesteps over a distance range (both inverse and direct trajectory
#' function). For the former two, either a vector or dataframe of
#' `new_times` or `new_distances` may be provided. If a dataframe is
#' provided, it must contain the column `event_timestamp` or `distance`,
#' and all additional columns will be preserved through the interpolation.
#'
#' ### Distances from Times
#'
#' If `new_times` is provided, the function will find the `distance` of each
#' trip at each point in time. If a dataframe is provided, it must contain
#' the column `event_timestamp`. This will use the trajectory's direct function.
#' When using `new_times`, a `deriv` value can also be set greater than 0.
#' See below for a more detailed discussion.
#'
#' ### Times from Distances
#'
#' If `new_distances` is provided, the function will find the `event_timestamp`
#' of each trip at each point in space. If a dataframe is provided, it must
#' contain the column `distance`. This will use the trajectory's
#' inverse function. When using `new_distances`, a `deriv` value cannot
#' be set greater than 0. See below for a more detailed discussion.
#'
#' ### Time & Distance Pairs from Distance Bounds
#'
#' Oftentimes, you may want to interpolate by small timesteps over a defined
#' region of space. This can be done by setting `distance_lims` and
#' `timestep`. The function will use the trajectory's inverse function to find
#' each trip's entrance and exit time through `distance_lims`, then create
#' a sequence between these entrance and exit times with a step of `timestep`.
#' Finally, the trajectory's direct function is used to find the distance
#' at each of these timepoints. A `deriv` value can also be set greater
#' than 0 for the final direct interpolation.
#'
#' If you have a well-defined region of space, this approach allows you to
#' interpolate vehicle positions at a very tight timescale over a large
#' number of trips efficiently. You could alternatively use `new_times` to
#' interpolate over the entire time range of all trips (which wouldn't
#' require an inverse function), though this may require orders of magnitude
#' more points and would be substantially less efficient.
#'
#' ## Finding Derivatives
#'
#' Depending on the `interp_method` used when fitting the trajectory object,
#' a derivative may be able to be found:
#'
#' - `interp_method = "linear"`: This will not allow derivatives. This is
#' because, at each observation, the piecewise linear function is not
#' differentiable.
#'
#' - `interp_method` is a spline from `stats::splinefun()`: This will typically
#' be differentiable up to the third degree (i.e., `deriv = 0` is position,
#' `deriv = 1` is speed, etc.).
#'
#' The derivative returned (as column `interp`) is the derivative of distance
#' with respect to time. This means the first derivative is velocity, second is
#' acceleration, and third is jerk. The derivative is taken from the direct
#' trajectory, not the inverse, and the inverse trajectory cannot be used to
#' find derivatives. This means that if `new_distances` is provided, `deriv`
#' must equal 0. If starting from distance values, but derivatives are desired,
#' consider interpolating for timepoints first, then using these as `new_times`
#' to find the derivative.
#'
#' ## Prevents Extrapolation
#'
#' By default, many interpolating curves provided by R and `stats` will allow
#' extrapolation (i.e., the input of an `event_timestamp` or `distance` beyond
#' the original time or space domain of the
#' trip). In general, this will not be reasonable for transit vehicles:
#' time points should be constrained by the time that a trip has actually
#' been observed, and distances should be constrained to the part of a route
#' a trip actually ran.
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
#' with at least the column `event_timestamp` of new timepoints to interpolate
#' at. May also contain the column `trip_id_performed`, which will
#' interpolate distances at each trip and time row pair. Default is `NULL`.
#' @param new_distances Optional. A vector of numeric distances, or a dataframe
#' with at least the column `distance` of new distances to interpolate at.
#' May also contain the column `trip_id_performed`, which will
#' interpolate times at each trip and distance row pair. Default is `NULL`.
#' @param distance_lims Optional. A vector of `(minimum, maximum)` distance
#' bounds over which to interpolate at a given timestep. If provided,
#' `timestep` must also be provided. Default is `NULL`.
#' @param timestep Optional. A single numeric indicating the time interval
#' between successive interpolating steps when defining `distance_lims`. If
#' provided, `distance_lims` must also be provided. Default is `NULL`.
#' @param deriv Optional. The vector of numeric derivative degrees to
#' calculate at. May only be set if `new_times` or `distance_lims`/`timestep`
#' is provided, and not if `new_distances` is provided. Default is `0`
#' (i.e., position).
#' @param trips Optional. A vector of `trip_id_performed`s to interpolate for.
#' Default is `NULL`, which will use all trips found in the trajectory object
#' (or, if include, in the `trip_id_performed` column of `new_times` or
#' `new_distances`).
#' @param ... Other parameters (not used).
#' @return The input dataframe, with an additional column `interp` of the
#' interpolated values requested, and an additional `trip_id_performed`
#' column will all trips for which that point is within range. If `new_times`
#' or `distance_lims`/`timestep` are used, a column `deriv` will also be
#' included, indicating which derivative degree each interpolated row
#' corresponds to.
#' @export
#' @examples
#' # Set my parameters
#' my_times = seq(from = 1779890000,
#'                to = 1779893600,
#'                by = 180)
#' my_distances = seq(from = 100,
#'                    to = 35000,
#'                    by = 5000)
#' my_distance_lims = c(500, 600)
#' my_timestep = 10
#'
#' # Get input data
#' lineE_traj <- new_transittraj_data("get_trajectory_fun")
#'
#' # Run function: get distances from times
#' interp_dists <- predict(object = lineE_traj,
#'                         new_times = my_times)
#' dim(interp_dists)
#' head(interp_dists)
#'
#' # Run function: get speeds from times
#' interp_speeds <- predict(object = lineE_traj,
#'                          new_times = my_times,
#'                          deriv = 1)
#' dim(interp_speeds)
#' head(interp_speeds)
#'
#' # Run function: get times from distances
#' interp_times <- predict(object = lineE_traj,
#'                         new_distances = my_distances)
#' dim(interp_times)
#' head(interp_times)
#'
#' # Run function: get time & distance pairs given distance bounds
#' interp_time_dist_pairs <- predict(object = lineE_traj,
#'                                   distance_lims = my_distance_lims,
#'                                   timestep = my_timestep)
#' dim(interp_time_dist_pairs)
#' head(interp_time_dist_pairs)
#'
#' # Run function: vectorized derivatives
#' interp_vec <- predict(object = lineE_traj,
#'                       new_times = my_times,
#'                       deriv = c(0, 1, 2))
#' dim(interp_vec)
#' head(interp_vec)
predict.avltrajectory_group <- function(object, new_times = NULL, new_distances = NULL,
                                        distance_lims = NULL, timestep = NULL,
                                        deriv = 0, trips = NULL, ...) {

  # --- Validation ---
  if ("avltrajectory_single" %in% class(object)) {
    has_inv <- is.function(attr(object, "inv_traj_fun"))
  } else {
    has_inv <- is.function(attr(object, "inv_traj_fun")[[1]])
  }
  max_deriv <- attr(object, "max_deriv")
  # Validate & format input DFs
  predict_traj_input_validation(new_times = new_times,
                                new_distances = new_distances,
                                distance_lims = distance_lims,
                                timestep = timestep,
                                has_inv = has_inv,
                                deriv = deriv,
                                max_deriv = max_deriv)

  # --- DF Setup & Interpolation ---
  trip_extremes <- get_trip_extremes(trajectory = object,
                                     filter_trips = trips)
  # Find correct function to use
  if (!is.null(new_times)) {
    new_times_trips <- predict_traj_setup_new_times(trip_extremes = trip_extremes,
                                                    new_times = new_times,
                                                    deriv = deriv)
    interp <- interpolate_distances(trajectory = object,
                                    new_times_trips = new_times_trips)
  }
  if (!is.null(new_distances)) {
    new_dist_trips <- predict_traj_setup_new_dists(trip_extremes = trip_extremes,
                                                   new_distances = new_distances)
    interp <- interpolate_times(trajectory = object,
                                new_dist_trips = new_dist_trips)
  }
  if (!is.null(distance_lims)) {
    new_times_trips <- predict_traj_setup_dist_lims(trajectory = object,
                                                    trip_extremes = trip_extremes,
                                                    distance_lims = distance_lims,
                                                    timestep = timestep,
                                                    deriv = deriv)
    interp <- interpolate_distances(trajectory = object,
                                    new_times_trips = new_times_trips)
  }

  return(interp)
}
