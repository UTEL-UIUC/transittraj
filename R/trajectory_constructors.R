#' Constructor for grouped trajectory class
#'
#' This superclass holds a single, combined trajectory function. Plus vectors of
#' all trip IDs described by that function, and vectors of the time & distance
#' ranges of those trips. Not intended for external use.
#'
#' @param trip_id_performed Character if trip ids.
#' @param traj_fun List or single trajectory functions.
#' @param inv_traj_fun List or single inverse trajectory functions.
#' @param min_dist Vector if minimum distance values.
#' @param max_dist Vector of maximum distance values.
#' @param min_time Vector of minimum time values.
#' @param max_time Vector of maximum time values.
#' @param traj_type Interp method character string
#' @param inv_tol Tolerance used in numeric inverse
#' @param max_deriv Max derivative allowed
#' @param used_speeds Whether speeds were used
#' @param agency_tz Timezone of agency
#' @param ... Other inputs
#' @param class Object class
#' @return Grouped trajectory object
#' @keywords internal
new_avltrajectory_group <- function(trip_id_performed = character(),
                                    traj_fun, inv_traj_fun = NULL,
                                    min_dist, max_dist, min_time, max_time,
                                    traj_type, inv_tol = NULL, max_deriv = 0,
                                    used_speeds = FALSE, agency_tz,
                                    ..., class = character()) {

  structure(trip_id_performed, class = c(class, "avltrajectory_group"),
            traj_fun = traj_fun,
            inv_traj_fun = inv_traj_fun,
            min_dist = min_dist,
            max_dist = max_dist,
            min_time = min_time,
            max_time = max_time,
            traj_type = traj_type,
            inv_tol = inv_tol,
            max_deriv = max_deriv,
            used_speeds = used_speeds,
            agency_tz = agency_tz)
}

#' Constructor for single trajectory class
#'
#' This is a subclass (special case) of a grouped trajectory, in which there is
#' only one trip. Trajectory function is inteded to describe only one trip.
#' Associated trip properties (ID & ranges) should describe only that trip. Not
#' intended for external use.
#'
#' @inheritParams new_avltrajectory_group
#' @return Single trajectory object
#' @keywords internal
new_avltrajectory_single <- function(trip_id_performed = character(),
                                     traj_fun, inv_traj_fun = NULL,
                                     min_dist, max_dist, min_time, max_time,
                                     traj_type, inv_tol = NULL, max_deriv = 0,
                                     used_speeds = FALSE, agency_tz) {

  # Should take in only a single trip ID
  stopifnot(length(trip_id_performed) == 1)

  # Call parent class constructor
  new_avltrajectory_group(trip_id_performed,
                          traj_fun = traj_fun,
                          inv_traj_fun = inv_traj_fun,
                          min_dist = min_dist,
                          max_dist = max_dist,
                          min_time = min_time,
                          max_time = max_time,
                          traj_type = traj_type,
                          inv_tol = inv_tol,
                          max_deriv = max_deriv,
                          used_speeds = used_speeds,
                          agency_tz = agency_tz,
                          class = "avltrajectory_single")
}

#' Fit continuous trajectory interpolating curves from AVL data
#'
#' @description
#' This function fits a continuous vehicle trajectory to cleaned AVL
#' points (see `vignette("articles/data-workflow-la")`). This function
#' operates as a "function factory", returning a function (closure) which
#' takes a timestamp and returns each trip's position. A separate curve is
#' fit for each trip, and stored in a special trajectory object class.
#' The default interpolating method is a velocity-informed piecewise
#' cubic interpolating polynomial, but linear interpolation and other
#' spline-based techniques are also supported. See `Details` for a discussion.
#'
#' @details
#'
#' ## Interpolating Methods
#'
#' The goal of this function is to fit a continuous function representing a
#' vehicle's distance traveled as a function of time, for each trip. This
#' function supports to types of interpolating curves:
#'
#' - Linear interpolation, for `interp_method = "linear"`. This will fit a
#' simple linear function, ignorant to recorded `speed` values.
#'
#' - Spline interpolation, for `interp_method` set to any method supported by
#' `stats::splinefun()` (i.e., `"fmm"`, `"natural"`, `"periodic"`, `"monoH.FC"`,
#' or `"hyman"`.) Only `interp_method = "monoH.FC"` supports use of recorded
#' `speed` values.
#'
#' By default, `interp_method = "monoH.FC"` and `use_speeds = TRUE`. This will
#' yield a continuous, differentiable, and invertible trajectory.
#' If the input `distance` and `speed` values satisfy Fritsch-Carlson,
#' the interpolating function is guaranteed to be montonic.
#' See `make_monotonic()`. This is equivalent to the velocity-informed
#' monotonic peicewise cubic Hermite interpolating polynomials with
#' monotonic enforcement (VCHIP-ME) technique proposed by
#' Robbennolt et al. (2026), and is our recommended approach.
#'
#' Note that `use_speeds = TRUE` requires `interp_method = "monoH.FC"`, but
#' `interp_method = "monoH.FC"` does not require `use_speeds = TRUE`. In the
#' latter scenario, a "velocity-ignorant' Fritsch-Carlson interpolating function
#' can be created (Huang et al., 2023). If input `distance` values are
#' monotonic, this curve is guaranteed to be monotonic.
#'
#' ## Inverse Functions
#'
#' Often times, we are concerned not with the position of a vehicle at a
#' particular time, but the time at which a vehicle crosses a specific point
#' in space. This can be accomplished by computing an inverse trajectory
#' function. If `find_inverse_function = TRUE` (the default), a numeric
#' inverse to the fit trajectory function will be found, with a tolerance
#' controlled by `inv_tol`.
#'
#' Because the inverse function is numerical, it can be found for any type of
#' interpolating curve (linear or spline). However, the input data must be
#' strictly monotonic for the trajectory curve to be invertible. If
#' `find_inverse_function = TRUE`, this will be verified before proceeding (see
#' `validate_monotonicity()`).
#'
#' ## The Trajectory Object
#'
#' A trajectory function does not exist by itself; rather, it requires the
#' context about the trip it describes, as well as its inverse function. As
#' such, `get_trajectory_fun()` returns an AVL trajectory object. If
#' `return_group_function = TRUE` (the default), the function will return a
#' single object containing:
#'
#' - A vector of `trip_id_performed`s present in `distance_df`.
#'
#' - A list of fit trajectory functions (closures), one per trip, indexed by
#' their `trip_id_performed`.
#'
#' - A list of fit inverse trajectory functions (closures), one per trip,
#' indexed by their `trip_id_performed`.
#'
#' - Information about how the trajectory and inverse trajectory functions were
#' fit, including `interp_method`, `use_speeds`, and `inv_tol`.
#'
#' - A vector each for the minimum distances, maximum distances, minimum times,
#' and maximum times of each trip. These inform the domain and range of the
#' trajectory function and its inverse, preventing extrapolation beyond the
#' time or distance range actually served by a trip.
#'
#' - The agency's timezone (see `OlsonNames()`), as extracted from
#' the `event_timestamp` column.
#'
#' Alternatively, if `return_group_function = FALSE`, a separate trajectory
#' object will be returned for each trip, as a list of objects indexed by
#' their `trip_id_performed`.
#'
#' More information about the trajectory object classes and how to use them is
#' available at `vignette("articles/intro-trajectories-la")`.
#'
#' @references
#' Robbennolt, Jake, Sirajum Munira, and Stephen D. Boyles. 2026.
#' “A Comparative Study of Spline-Based Trajectory Reconstruction Methods
#' Across Varying Automatic Vehicle Location Data Densities.” Paper
#' presented at 2026 Transportation Research Board Annual Meeting, January 11.
#' http://arxiv.org/abs/2509.00119.
#'
#' Huang, Yuzhu, Awad Abdelhalim, Anson Stewart, Jinhua Zhao, and Haris
#' Koutsopoulos. 2023. “Reconstructing Transit Vehicle Trajectory Using
#' High-Resolution GPS Data.” 2023 IEEE 26th International Conference on
#' Intelligent Transportation Systems (ITSC), September 24, 5247–53.
#' https://doi.org/10.1109/ITSC57777.2023.10422524.
#'
#' @param distance_df A dataframe of linearized AVL data. Must include
#' `trip_id_performed`, `event_timestamp`, and `distance`. If
#' `use_speed = TRUE`, must also include `speed`.
#' @param interp_method Optional. The type of interpolation function to be fit.
#' Either `"linear"`, or a spline method from `stats::splinefun()`. Default is
#' `"monoH.FC"`.
#' @param use_speeds Optional. A boolean, should curves be constrained by
#' observed AVL speeds? Should only be used with `interp_method = "monoH.FC"`,
#' but `monoH.FC` does not require speeds. Default is `TRUE`.
#' @param return_group_function Optional. A boolean, should the returned
#' trajectory object be grouped into a single function? If FALSE, will return a
#' list (indexed by `trip_id_performed`) of single trajectory objects. Default
#' is `TRUE`.
#' @param find_inverse_function Optional. A boolean, should the numeric inverse
#' function (time ~ distance) be calculated? Default is `TRUE`.
#' @param inv_tol Optional. A numeric in the units of input `distance`, the
#' tolerance used when calculating the numeric inverse function. Default is
#' `0.01`.
#' @return If `return_group_function = TRUE`, a grouped trajectory object. If
#' `FALSE`, a list of single trajectory objects, indexed by their
#' `trip_id_performed`.
#' @export
#' @examples
#' # Get input data
#' lineE_mono <- new_transittraj_data("make_monotonic")
#'
#' # Run function: grouped trajectory object
#' lineE_traj_grouped <- get_trajectory_fun(distance_df = lineE_mono)
#' summary(lineE_traj_grouped)
#'
#' # Run function: list of single trajectory objects
#' lineE_traj_singles <- get_trajectory_fun(distance_df = lineE_mono,
#'                                          return_group_function = FALSE)
#' length(lineE_traj_singles)
#' summary(lineE_traj_singles[[2]])
get_trajectory_fun <- function(distance_df,
                               interp_method = "monoH.FC", use_speeds = TRUE,
                               find_inverse_function = TRUE, inv_tol = 0.01,
                               return_group_function = TRUE) {

  # --- Validation of all inputs ---
  # Fields
  if (use_speeds) {
    needed_fields <- c("trip_id_performed", "event_timestamp", "distance",
                       "speed")
  } else {
    needed_fields <- c("trip_id_performed", "event_timestamp", "distance")
  }
  validate_input_to_tides(needed_fields, distance_df)
  # Monotonicty -- must be strict if finding inverse
  mono_check <- validate_monotonicity(distance_df,
                                      check_speed = use_speeds)
  if (use_speeds) { # If using speeds, check all three bools; otherwise, only need first two
    max_mono_check <- 3
  } else {
    max_mono_check <- 2
  }
  checked_conditions <- mono_check[1:max_mono_check]
  if (find_inverse_function) {
    # If finding an inverse function, must error if montonicity not met
    if (!all(checked_conditions)) {
      rlang::abort(message = paste(c("The following monotonicity conditions are not satisfied:",
                                     names(checked_conditions)[!checked_conditions],
                                     "\nMonotonicity required inverse function."),
                                   collapse = " "),
                   class = "error_tidesval_mono")
    }
  } else {
    # If not finding inverse function, can proveed but will give warning
    if (!all(checked_conditions)) {
      rlang::warn(message = paste(c("The following monotonicity conditions are not satisfied:",
                                    names(checked_conditions)[!checked_conditions],
                                    "\nMonotonicity not required for non-inverse function. Proceeding with direct function fitting."),
                                  collapse = " "),
                  class = "warn_tidesval_mono")
    }
  }
  # Methods
  if (use_speeds) { # If using speeds
    if (interp_method != "monoH.FC") {
      # If method is monoH.FC
      if (interp_method == "linear") {
        rlang::warn(message = "Speeds cannot be used for linear interpolation. Ignoring speeds and performing linear interpolation.",
                    class = "warn_traj_type")
      } else {
        rlang::warn(message = "Using speeds for spline interpolation requires method monoH.FC. monoH.FC will be used unless use_speeds set to FALSE.",
                    class = "warn_traj_type")
      }
    }
  }

  # Set derivative
  if (interp_method == "linear") {
    # If linear
    max_deriv = 0
  } else {
    # If spline
    max_deriv = 3
  }

  # Get timezone
  current_tz <- attr(distance_df$event_timestamp, "tzone")

  # Perform calculations for trip bounds
  trip_bounds <- distance_df %>%
    dplyr::group_by(trip_id_performed) %>%
    dplyr::summarise(min_dist = min(distance),
                     max_dist = max(distance),
                     min_time = min(as.numeric(event_timestamp)),
                     max_time = max(as.numeric(event_timestamp)))

  # Set up loop
  trips <- trip_bounds$trip_id_performed
  num_trips <- length(trips)
  traj_functions <- vector("list")
  traj_inv_functions <- vector("list")

  # Loop through each trip
  for (index in 1:num_trips) {
    # Filter to current trip
    current_trip <- trips[index]
    trip_df <- distance_df %>%
      dplyr::filter(trip_id_performed == current_trip) %>%
      dplyr::mutate(event_timestamp = as.numeric(event_timestamp)) %>%
      dplyr::arrange(event_timestamp)

    # Trajectory fitting
    if (interp_method == "linear") {
      # If fit a linear function
      current_fun <- stats::approxfun(x = trip_df$event_timestamp,
                                      y = trip_df$distance,
                                      method = "linear", rule = 1)
    } else {
      # Otherwise, use spline
      if (use_speeds) {
        # If using speeds, fit PCHIP
        current_fun <- stats::splinefunH(x = trip_df$event_timestamp,
                                         y = trip_df$distance,
                                         m = trip_df$speed)
      } else {
        # If not using speeds
        current_fun <- stats::splinefun(x = trip_df$event_timestamp,
                                        y = trip_df$distance,
                                        method = interp_method)
      }
    }

    # Saving fit trajectory
    traj_functions[[current_trip]] <- current_fun
  }

  # Calculate inverse functions
  # Must occur outside for loop
  if (find_inverse_function) {
    # If calculating inverse function
    traj_inv_functions <- lapply(names(traj_functions), function(trip_id_performed) {
      # Set bounds
      lwr <- trip_bounds$min_time[which(trip_bounds$trip_id_performed == trip_id_performed)]
      upr <- trip_bounds$max_time[which(trip_bounds$trip_id_performed == trip_id_performed)]
      # Find inverse
      get_inverse_traj(f = traj_functions[[trip_id_performed]],
                       lower = lwr, upper = upr, inv_tol = inv_tol)
    })
    names(traj_inv_functions) <- names(traj_functions)
  } else {
    # Otherwise, set to NULL
    traj_inv_functions <- NULL
    inv_tol <- NULL
  }

  # If grouping into a single function
  if (return_group_function) {
    # Create object
    grouped_traj <- new_avltrajectory_group(trip_id_performed = trips,
                                            traj_fun = traj_functions,
                                            inv_traj_fun = traj_inv_functions,
                                            min_dist = trip_bounds$min_dist,
                                            max_dist = trip_bounds$max_dist,
                                            min_time = trip_bounds$min_time,
                                            max_time = trip_bounds$max_time,
                                            traj_type = interp_method,
                                            inv_tol = inv_tol,
                                            max_deriv = max_deriv,
                                            used_speeds = use_speeds,
                                            agency_tz = current_tz)

    return(grouped_traj)
  } else {
    # If not grouping, create list of individual
    # Must occur outside for loop bc inverse calculation must occur outside for loop
    single_traj_list <- lapply(names(traj_functions), function(trip_id_performed) {
      # Get index
      current_index <- which(trip_bounds$trip_id_performed == trip_id_performed)
      # Create object
      current_obj <- new_avltrajectory_single(trip_id_performed = trip_id_performed,
                                              traj_fun = traj_functions[[trip_id_performed]],
                                              inv_traj_fun = traj_inv_functions[[trip_id_performed]],
                                              min_dist = trip_bounds$min_dist[current_index],
                                              max_dist = trip_bounds$max_dist[current_index],
                                              min_time = trip_bounds$min_time[current_index],
                                              max_time = trip_bounds$max_time[current_index],
                                              traj_type = interp_method,
                                              max_deriv = max_deriv,
                                              inv_tol = inv_tol,
                                              used_speeds = use_speeds,
                                              agency_tz = current_tz)
      return(current_obj)
    })
    names(single_traj_list) <- names(traj_functions)
    return(single_traj_list)
  }
}

#' Fit continuous trajectory interpolating curves from GTFS schedule data
#'
#' @description
#' This function fits a continuous vehicle trajectory function to scheduled GTFS
#' `stop_times`. This function
#' operates as a "function factory", returning a function (closure) which
#' takes a timestamp and returns each trip's position. A separate curve is
#' fit for each trip, and stored in a special trajectory object class.
#' The default interpolating method is linear, but
#' spline-based techniques are also supported. See `Details` for a discussion.
#'
#' @details
#'
#' ## Stops, Dwells, and Monotonicity
#'
#' To fit an interpolating trajectory function, each observation must include
#' distance and timestamp pairs throughout each trip. While `stop_times` does
#' include a `shape_dist_traveled` field, this is optional and often left
#' empty by agencies. Additionally, small distortions in spatial projections
#' mean that projected GPS points may not align perfectly with the agency's
#' calculated `shape_dist_traveled`. As such, this function uses
#' `get_stop_distances()` to get the distance of each stop along each shape for
#' each trip. Alternatively, all stops and trips can be referenced to the
#' same spatial feature using `shape_geometry`. Consider setting `project_crs`
#' to the same spatial projection used to linearize AVL GPS points.
#'
#' The trajectory functions are fit using the times a trip is scheduled to
#' serve each stop. There is some ambiguity here: should a stop's timestamp
#' be when the vehicle arrives, or departs? This can be controlled using
#' `use_stop_time`, set to `"departure"` for `departure_time`, `"arrival"` for
#' `arrival_time`, or `"both"` to include both `departure_time` and
#' `arrival_time` as distinct observations (i.e., distance & timestamp pairs).
#'
#' Often times, however, a GTFS schedule will not have different `arrival_time`
#' and `departure_time` values, especially if the timetable was not developed
#' considering stop-level dwell times. In this scenario, it may be best to use
#' only one of `departure_time` or `arrival_time`. If a dwell is desired, use
#' `add_stop_dwell` to simulate a dwell time at each stop. This will increase
#' the `departure_time` by the number of seconds specified.
#'
#' Adding dwells opens a new consideration, however: the trajectory will no
#' longer be strictly monotonic, as the vehicle will hold at a constant distance
#' for some period of time. This is only a concern if
#' `find_inverse_function = TRUE`, which requires strictly monotonic input data.
#' If both dwell times and an inverse function are desired, consider setting
#' `add_distance_error > 0` to restore strict monotonicity. See
#' `make_monotonic()` for more details.
#'
#' ## Interpolating Methods
#'
#' The goal of this function is to fit a continuous function representing a
#' GTFS trip's scheduled distance traveled as a function of time. This
#' function supports two types of interpolating curves:
#'
#' - Linear interpolation, for `interp_method = "linear"`. This will fit a
#' simple linear function between stops.
#'
#' - Spline interpolation, for `interp_method` set to any method supported by
#' `stats::splinefun()` (i.e., `"fmm"`, `"natural"`, `"periodic"`, `"monoH.FC"`,
#' or `"hyman"`.)
#'
#' By default, `interp_method = linear`, and linear interpolation is the
#' recommended method for schedule trajectories. This is because timetable
#' development typically assumes a constant running speed over a corridor, so
#' linearly connecting stop times will best reflect a trip's scheduled
#' trajectory.
#'
#' ## Inverse Functions
#'
#' Often times, we are concerned not with the position of a vehicle at a
#' particular time, but the time at which a vehicle crosses a specific point
#' in space. This can be accomplished by computing an inverse trajectory
#' function. If `find_inverse_function = TRUE` (the default), a numeric
#' inverse to the fit trajectory function will be found, with a tolerance
#' controlled by `inv_tol`.
#'
#' Because the inverse function is numerical, it can be found for any type of
#' interpolating curve (linear or spline). However, the input data must be
#' strictly monotonic for the trajectory curve to be invertible. If
#' `find_inverse_function = TRUE`, this will be verified before proceeding (see
#' `validate_monotonicity()`).
#'
#' ## The Trajectory Object
#'
#' A trajectory function does not exist by itself; rather, it requires the
#' context about the trip it describes, as well as its inverse function. As
#' such, `get_trajectory_fun()` returns an AVL trajectory object. If
#' `return_group_function = TRUE` (the default), the function will return a
#' single object containing:
#'
#' - A vector of `trip_id_performed`s present in `distance_df`.
#'
#' - A list of fit trajectory functions (closures), one per trip, indexed by
#' their `trip_id_performed`.
#'
#' - A list of fit inverse trajectory functions (closures), one per trip,
#' indexed by their `trip_id_performed`.
#'
#' - Information about how the trajectory and inverse trajectory functions were
#' fit, including `interp_method`, `use_speeds`, and `inv_tol`.
#'
#' - A vector each for the minimum distances, maximum distances, minimum times,
#' and maximum times of each trip. These inform the domain and range of the
#' trajectory function and its inverse, preventing extrapolation beyond the
#' time or distance range actually served by a trip.
#'
#' - The agency's timezone (see `OlsonNames()`), as extracted from
#' the `event_timestamp` column.
#'
#' Alternatively, if `return_group_function = FALSE`, a separate trajectory
#' object will be returned for each trip, as a list of objects indexed by
#' their `trip_id_performed`.
#'
#' More information about the trajectory object classes and how to use them is
#' available at `vignette("articles/intro-trajectories-la")`.
#'
#' @inheritParams get_stop_distances
#' @inheritParams get_trajectory_fun
#' @inheritParams make_monotonic
#' @inheritParams get_gtfs_service_dates
#' @param agency_timezone Optional. A timezone string (see `OlsonNames()`)
#' indicating he appropriate timezone for the stop times. Default is `NULL`,
#' where the timezone in `agency.txt` will be used.
#' @param use_stop_time Optional. A string, which stop time column should be
#' used for the timepoint? Must be one of `"arrival"` (use `arrival_time`),
#' `"departure"` (use `departure_time`), or `"both"`,
#' (timepoints will be created at both the stop arrival and departure). Default
#' is `"departure"`.
#' @param add_stop_dwell Optional. A numeric. If `use_stop_time = "both"`,
#' but scheduled arrival and departure times are equal (i.e., no dwell), how
#' many seconds of dwell should be added? This will adjust forward the
#' `departure_time`. Default is 0.
#' @param interp_method Optional. The type of interpolation function to be fit.
#' Either `"linear"`, or a spline method from `stats::splinefun()`. Default is
#' `"linear"`.
#' @return If `return_group_function = TRUE`, a grouped trajectory object. If
#' `FALSE`, a list of single trajectory objects, indexed by their
#' `trip_id_performed`.
#' @export
#' @examples
#' # Set my parameters
#' my_crs <- 32611
#' my_start_date <- as.Date("2026-05-27")
#' my_end_date <- as.Date("2026-05-27")
#'
#' # Get input data
#' lineE_gtfs <- new_transittraj_data("filter_by_route")
#' lineE_shape <- new_transittraj_data("get_shape_geometry")
#'
#' # Run function: build trajectory
#' lineE_scheduled_traj <- get_gtfs_trajectory_fun(gtfs = lineE_gtfs,
#'                                                 project_crs = my_crs,
#'                                                 date_min = my_start_date,
#'                                                 date_max = my_end_date)
#'
#' # Show trajectory: summary
#' summary(lineE_scheduled_traj)
#'
#' # Show trajectory: plot (just a handful of trips)
#' ordered_trips <- get_trip_extremes(lineE_scheduled_traj) %>%
#'    dplyr::arrange(min_time) %>%
#'    dplyr::pull(trip_id_performed)
#' plot_trajectory(trajectory = lineE_scheduled_traj,
#'                 plot_trips = ordered_trips[20:25],
#'                 traj_color = "indianred3")
get_gtfs_trajectory_fun <- function(gtfs,
                                    shape_geometry = NULL, project_crs = 4326,
                                    date_min = NULL, date_max = NULL,
                                    use_calendar_table = "calendar",
                                    agency_timezone = NULL,
                                    use_stop_time = "departure",
                                    add_stop_dwell = 0, add_distance_error = 0,
                                    interp_method = "linear",
                                    find_inverse_function = TRUE,
                                    return_group_function = TRUE,
                                    inv_tol = 0.01) {

  # --- Validate GTFS ---
  # Only need to validate the files & fields used by this function uniquely
  # Others will be validated in get_stop_distances()
  # stop_times: trip_id, stop_id, stop_sequence; others depend on timepoint used
  if (use_stop_time == "departure") {
    stop_times_fields <- c("trip_id", "stop_id", "stop_sequence",
                           "departure_time")
  } else if (use_stop_time == "arrival") {
    stop_times_fields <- c("trip_id", "stop_id", "stop_sequence",
                           "arrival_time")
  } else if (use_stop_time == "both") {
    stop_times_fields <- c("trip_id", "stop_id", "stop_sequence",
                           "departure_time", "arrival_time")
  } else {
    rlang::abort(message = "Input use_stop_time not recognized. Please input \"departure\", \"arrival\", or \"both\".",
                 class = "error_gtfstraj_stoptime")
  }
  validate_gtfs_input(gtfs,
                      table = "stop_times",
                      needed_fields = stop_times_fields)

  # trips: service_id (shape_id, trip_id will be validated by get_stop_distances())
  validate_gtfs_input(gtfs,
                      table = "trips",
                      needed_fields = c("service_id", "trip_id"))
  # If TZ not povided, pull from input GTFS
  if(is.null(agency_timezone)) {
    agency_timezone <- gtfs$agency$agency_timezone[1]
  }

  # --- Build trip timepoints ---
  # Get stop distances
  stop_dist_df <- get_stop_distances(gtfs = gtfs,
                                     shape_geometry = shape_geometry,
                                     project_crs = project_crs) %>%
    dplyr::select(stop_id, shape_id, distance)

  # Get time by desired schedule time
  if (use_stop_time == "departure") {
    # If using departure times, pull that
    trip_timepoints <- gtfs$stop_times %>%
      dplyr::arrange(trip_id, stop_sequence) %>%
      dplyr::select(trip_id, stop_id, departure_time) %>%
      tidyr::pivot_longer(cols = c("departure_time"),
                          names_to = "in_out",
                          values_to = "stp_time")
  } else if (use_stop_time == "arrival") {
    # If using arrival times, pull that
    trip_timepoints <- gtfs$stop_times %>%
      dplyr::arrange(trip_id, stop_sequence) %>%
      dplyr::select(trip_id, stop_id, arrival_time) %>%
      tidyr::pivot_longer(cols = c("arrival_time"),
                          names_to = "in_out",
                          values_to = "stp_time")
  } else if (use_stop_time == "both") {
    # If using both, start by pulling dwell times
    # must make sure unique (time, distance) points -- if there are zero-second dwells, this won't be true
    trip_dwells <- gtfs$stop_times %>%
      dplyr::select(trip_id, stop_id, arrival_time, departure_time) %>%
      dplyr::mutate(dwell_time = as.numeric(difftime(departure_time, arrival_time, units = "secs")))

    # Calculate number of zero-second dwell times
    num_zero_dwells <- sum(trip_dwells$dwell_time == 0)
    if(num_zero_dwells > 0) {
      # If there are zero-second dwell times
      if(add_stop_dwell == 0) {
        # Stop dwell not provided
        rlang::abort(message = "Zero-second stop dwells detected, but no stop dwell addition provided. Please either: change stop time method, or provide stop dwell time to add.",
                     class = "error_gtfstraj_inputdata")
      } else {
        # Use provided dwell time to adjust forward departure times
        trip_dwells_adj <- trip_dwells %>%
          dplyr::mutate(dwell_time = dplyr::if_else(condition = (dwell_time == 0),
                                                    true = add_stop_dwell,
                                                    false = dwell_time),
                        departure_time = hms::as_hms(as.numeric(arrival_time) + dwell_time))

        # Replace GTFS departure times with adjusted. Arrivals stay the same.
        # Correct in the provided GTFS object
        gtfs$stop_times$departure_time <- trip_dwells_adj$departure_time
      }
    }

    # Get corrected times (or uncorrected if it was not necessary)
    trip_timepoints <- gtfs$stop_times %>%
      dplyr::arrange(trip_id, stop_sequence) %>%
      dplyr::select(trip_id, stop_id, arrival_time, departure_time) %>%
      tidyr::pivot_longer(cols = c("arrival_time", "departure_time"),
                          names_to = "in_out",
                          values_to = "stp_time")
  }

  # Join previous info
  if (!is.null(shape_geometry)) {
    # If shape geometry provided, do not need to join by shape_id
    trip_dists <- trip_timepoints %>%
      dplyr::left_join(y = (gtfs$trips %>% dplyr::select(trip_id, service_id)),
                       by = "trip_id", relationship = "many-to-many") %>%
      dplyr::left_join(y = stop_dist_df, by = "stop_id")
  } else {
    # If shape geometry not provided, need to join by shape_id
    trip_dists <- trip_timepoints %>%
      dplyr::left_join(y = (gtfs$trips %>% dplyr::select(trip_id, shape_id, service_id)),
                       by = "trip_id", relationship = "many-to-many") %>%
      dplyr::left_join(y = stop_dist_df, by = c("stop_id", "shape_id"))
  }

  service_dates <- get_gtfs_service_dates(gtfs = gtfs,
                                          date_min = date_min,
                                          date_max = date_max,
                                          use_calendar_table = use_calendar_table)

  # Enumerate trips over all service dates
  trip_dates <- trip_dists %>%
    dplyr::filter(service_id %in% service_dates$service_id) %>%
    dplyr::left_join(y = service_dates,
                     by = "service_id", relationship = "many-to-many") %>%
    dplyr::mutate(trip_id = paste(date, trip_id, sep = "-"))


  # Get timetable, time-distance pairs
  trip_TIDES <- trip_dates %>%
    dplyr::mutate(hour_num = as.numeric(substr(stp_time, start = 1, stop = 2)),
                  # If past midnight, increment date
                  date = dplyr::if_else(condition = (hour_num >= 24),
                                        true = (date + 1),
                                        false = date),
                  # If past midnight, adjust hour back down
                  hour_num = dplyr::if_else(condition = (hour_num >= 24),
                                            true = (hour_num - 24),
                                            false = hour_num),
                  # Format stp_time string
                  stp_time = paste(sprintf("%02d", hour_num), substr(stp_time, start = 3, stop = 8),
                                   sep = ""),
                  # Convert time string to date type
                  event_timestamp = as.POSIXct(paste(date, stp_time, sep = " "),
                                               format = "%Y-%m-%d %H:%M:%S",
                                               tz = agency_timezone),
                  location_ping_id = as.character(dplyr::row_number())) %>%
    dplyr::select(-c(date, hour_num, stp_time)) %>%
    dplyr::rename(trip_id_performed = trip_id)

  # Correct for monotonicity
  if (add_distance_error > 0) {
    trip_TIDES <- make_monotonic(distance_df = trip_TIDES,
                                     correct_speed = FALSE,
                                     add_distance_error = add_distance_error)
  }

  # --- Get trajectory function ---
  traj_funs <- get_trajectory_fun(distance_df = trip_TIDES,
                                  interp_method = interp_method,
                                  find_inverse_function = find_inverse_function,
                                  return_group_function = return_group_function,
                                  inv_tol = inv_tol,
                                  use_speeds = FALSE)
}

#' Group existing trajectory objects or split them apart
#'
#' Trajectory objects hold the trajectory functions, and related information,
#' from one or more trip IDs. This function groups the fit trajectories from
#' multiple trips into one object, or splits a grouped object into many single
#' trajectory objects, one for each trip. See `help(get_trajectory_fun)` for
#' more information.
#'
#' @param trajectories A trajectory object to operate on. Can be a list of
#' single trajectories, a list of grouped trajectories, or one grouped
#' trajectory.
#' @param grouping A character string, either `"group"` to group all
#' trajectories in `trajectories`, or `"split"` to split `trajectories` into
#' a list of single trajectories.
#' @return If `grouping = "group"`, a group trajectory object; if
#' `grouping = "split"`, a list of single trajectory objects.
#' @export
#' @examples
#' # Get input data
#' lineE_mono <- new_transittraj_data("make_monotonic")
#'
#' # Fit a list of single trajectory functions
#' lineE_traj_singles <- get_trajectory_fun(distance_df = lineE_mono,
#'                                          return_group_function = FALSE)
#'
#' # Show sample singles
#' print(length(lineE_traj_singles))
#' print(lineE_traj_singles[[2]])
#'
#' # Run function: group singles
#' lineE_traj_grouped <- group_trajectories(trajectories = lineE_traj_singles,
#'                                          grouping = "group")
#' summary(lineE_traj_grouped)
#'
#' # Run function: split apart again
#' lineE_traj_singles_2 <- group_trajectories(trajectories = lineE_traj_grouped,
#'                                            grouping = "split")
#' print(length(lineE_traj_singles_2))
#' print(lineE_traj_singles_2[[2]])
group_trajectories <- function(trajectories,
                               grouping) {

  # --- Input Validation ---
  # grouping
  if (length(grouping) > 1) {
    rlang::abort(message = "Please provide only one option for grouping.",
                 class = "error_trajgrouping_input")
  }
  if (!(grouping %in% c("group", "split"))) {
    rlang::abort(message = "Unrecognized grouping action. Please input either \"group\" or \"split\".",
                 class = "error_trajgrouping_input")
  }
  # trajectories
  if (class(trajectories)[1] == "list") {
    # If list and every object is not a traj, throw error
    input_classes <- sapply(trajectories, function(x) class(x)[1])
    if (!all(input_classes %in% c("avltrajectory_single", "avltrajectory_group"))) {
      rlang::abort(message = "Unrecognized object in trajectories. Please provide one grouped trajectory, list of grouped trajectories, or list of single trajectories.",
                   class = "error_trajgrouping_input")
    }
  } else if ("avltrajectory_single" %in% class(trajectories)) {
    # If not list, but only one single, throw error
    rlang::abort(message = "Only one single trajectory provided as trajectories. Please provide one grouped trajectory, list of grouped trajectories, or list of single trajectories.",
                 class = "error_trajgrouping_input")
  } else if (!("avltrajectory_group" %in% class(trajectories))) {
    # If not some other group traj, throw error
    rlang::abort(message = "Unrecognized trajectories. Please provide one grouped trajectory, list of grouped trajectories, or list of single trajectories.",
                 class = "error_trajgrouping_input")
  }

  # --- Grouping ---
  if (grouping == "group") { # If grouping together
    if (class(trajectories)[1] != "list") {
      rlang::abort(message = "Unrecognized trajectories. Please provide list for grouping.",
                   class = "error_trajgrouping_input")
    }

    # Get single dataframe of all trip extremes
    all_extremes <- lapply(X = trajectories,
                           FUN = get_trip_extremes)
    # Remove distinct?? Won't align with functions
    all_extremes_df <- dplyr::bind_rows(all_extremes)

    if (length(unique(all_extremes_df$trip_id_performed)) <
        length(all_extremes_df$trip_id_performed)) {
      rlang::abort(message = "Duplicate trip IDs found in trajectories. Please input trajectories with unique trip_id_performeds.",
                   class = "error_trajgrouping_dup")
    }

    # Get single list of all traj functions
    fun_list <- lapply(X = trajectories,
                       FUN = function(x) attr(x, "traj_fun"))
    if (class(fun_list[[1]])[1] == "list") {
      fun_list <- unlist(fun_list)
    }
    inv_fun_list <- lapply(X = trajectories,
                           FUN = function(x) attr(x, "inv_traj_fun"))
    if (class(inv_fun_list[[1]])[1] == "list") {
      inv_fun_list <- unlist(inv_fun_list)
    }

    # Get single constant parameters
    new_traj_type <- unique(sapply(X = trajectories,
                                   FUN = function(x) attr(x, "traj_type")))
    if (length(new_traj_type) > 1) {
      rlang::abort(message = "All input trajectories must share traj_type.",
                   class = "error_trajgrouping_constants")
    }
    new_inv_tol <- unique(sapply(X = trajectories,
                                 FUN = function(x) attr(x, "inv_tol")))
    if (length(new_inv_tol) > 1) {
      rlang::abort(message = "All input trajectories must share inv_tol.",
                   class = "error_trajgrouping_constants")
    }
    new_max_deriv <- unique(sapply(X = trajectories,
                                 FUN = function(x) attr(x, "max_deriv")))
    if (length(new_max_deriv) > 1) {
      rlang::abort(message = "All input trajectories must share max_deriv.",
                   class = "error_trajgrouping_constants")
    }
    new_use_speeds <- unique(sapply(X = trajectories,
                                   FUN = function(x) attr(x, "used_speeds")))
    if (length(new_use_speeds) > 1) {
      rlang::abort(message = "All input trajectories must share use_speeds.",
                   class = "error_trajgrouping_constants")
    }
    new_agency_tz <- unique(sapply(X = trajectories,
                                   FUN = function(x) attr(x, "agency_tz")))
    if (length(new_agency_tz) > 1) {
      rlang::abort(message = "All input trajectories must share agency_tz.",
                   class = "error_trajgrouping_constants")
    }

    new_grouped_traj <- new_avltrajectory_group(trip_id_performed = all_extremes_df$trip_id_performed,
                                                traj_fun = fun_list,
                                                inv_traj_fun = inv_fun_list,
                                                min_dist = all_extremes_df$min_dist,
                                                max_dist = all_extremes_df$max_dist,
                                                min_time = all_extremes_df$min_time,
                                                max_time = all_extremes_df$max_time,
                                                traj_type = new_traj_type,
                                                inv_tol = new_inv_tol,
                                                max_deriv = new_max_deriv,
                                                used_speeds = new_use_speeds,
                                                agency_tz = new_agency_tz)
    return(new_grouped_traj)

  } else { # Otherwise, splitting apart
    # Check that input is one grouped traj
    if (!("avltrajectory_group" %in% class(trajectories))) {
      rlang::abort(message = "Unrecognized trajectories. Please provide one grouped trajectory for splitting.",
                   class = "error_trajgrouping_input")
    }

    # Get shared info
    new_traj_type <- attr(trajectories, "traj_type")
    new_inv_tol <- attr(trajectories, "inv_tol")
    new_max_deriv <- attr(trajectories, "max_deriv")
    new_used_speeds <- attr(trajectories, "used_speeds")
    new_agency_tz <- attr(trajectories, "agency_tz")

    # Create list of singles
    num_trips <- length(trajectories)
    single_traj_list <- vector(mode = "list")
    for (current_index in 1:num_trips) {
      current_trip_id <- as.vector(trajectories)[current_index]
      current_single_traj <- get_traj_index(group_traj = trajectories,
                                            index_num = current_index,
                                            new_traj_type = new_traj_type,
                                            new_inv_tol = new_inv_tol,
                                            new_max_deriv = new_max_deriv,
                                            new_used_speeds = new_used_speeds,
                                            new_agency_tz = new_agency_tz)
      single_traj_list[[current_trip_id]] <- current_single_traj
    }

    return(single_traj_list)
  }
}

#' Get a single trajectory object based on an index.
#'
#' From a grouped trajectory object and given index number, will return the
#' single trajectory object at that index.
#' Internal function. Not intended for external use.
#'
#' @param group_traj A transittraj avltrajectory_group object.
#' @param index_num Number indicating index to pull trajectory from
#' @param new_traj_type Interp method character string
#' @param new_inv_tol Tolerance used in numeric inverse
#' @param new_max_deriv Max derivative allowed
#' @param new_used_speeds Whether speeds were used
#' @param new_agency_tz Agency's timezone as Olson name
#' @return Single trajectory object
#' @keywords internal
get_traj_index <- function(group_traj, index_num,
                           new_traj_type, new_inv_tol, new_max_deriv,
                           new_used_speeds, new_agency_tz) {

  # Trip ID
  new_trip_id <- as.vector(group_traj)[index_num]

  # Time & distance ranges
  new_min_dist <- attr(group_traj, "min_dist")[index_num]
  new_max_dist <- attr(group_traj, "max_dist")[index_num]
  new_min_time <- attr(group_traj, "min_time")[index_num]
  new_max_time <- attr(group_traj, "max_time")[index_num]

  # Functions
  new_traj_fun <- attr(group_traj, "traj_fun")[[index_num]]
  new_inv_traj_fun <- attr(group_traj, "inv_traj_fun")[[index_num]]

  # Create single traj
  new_single_traj <- new_avltrajectory_single(trip_id_performed = new_trip_id,
                                              traj_fun = new_traj_fun,
                                              inv_traj_fun = new_inv_traj_fun,
                                              min_dist = new_min_dist,
                                              max_dist = new_max_dist,
                                              min_time = new_min_time,
                                              max_time = new_max_time,
                                              traj_type = new_traj_type,
                                              inv_tol = new_inv_tol,
                                              max_deriv = new_max_deriv,
                                              used_speeds = new_used_speeds,
                                              agency_tz = new_agency_tz)
  return(new_single_traj)
}
