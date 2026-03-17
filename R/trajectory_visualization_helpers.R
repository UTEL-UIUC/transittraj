#' Internal function for `plot_df_setup()` to interpolate.
#'
#' Uses a trajectory object to interpolate over of the time range by-trip
#' provided through `trip_time_extremes`.
#' For internal use only.
#'
#' @param trajectory A trajectory object.
#' @param trip_time_extremes For grouped, a DF with trip_id_performed, min_time,
#' and max_time; for single, a vector with min, max time
#' @param timestep Numeric of timestep for interpolation
#' @return A dataframe of trips_df for plotting functions
#' @keywords internal
interp_df_setup <- function(trajectory, trip_time_extremes, timestep) {

  if ("avltrajectory_single" %in% class(trajectory)) {

    # Get timepoint sequence to interp over, from extremes
    interp_times <- seq(from = trip_time_extremes$min_time[1],
                        to = trip_time_extremes$max_time[1],
                        by = timestep)

    # Interpolate
    trips_df <- predict.avltrajectory_single(object = trajectory,
                                             new_times = interp_times) %>%
      dplyr::rename(distance = interp) %>%
      dplyr::mutate(trip_id_performed = unclass(trajectory))

  } else if ("avltrajectory_group" %in% class(trajectory)) {

    # For each trip, get all timesteps between the entry/exit times
    interp_times <- trip_time_extremes %>%
      dplyr::group_by(trip_id_performed) %>%
      # Duplicate trip row for every interpolate timepoint necessary
      tidyr::uncount(weights = floor((max_time - min_time) / timestep + 1)) %>%
      # Create interp timepoint sequence
      dplyr::mutate(event_timestamp = seq(from = min_time[1],
                                          to = max_time[1],
                                          by = timestep)) %>%
      dplyr::select(-c(max_time, min_time)) %>%
      dplyr::ungroup()

    # Interpolate using internal function
    trips_df <- interpolate_distances_group(new_times_trips = interp_times,
                                            trajectory_function = attr(trajectory, "traj_fun"),
                                            deriv = 0) %>%
      dplyr::rename(distance = interp)
  }

  return(trips_df)
}

#' Set up dataframe & validate of point objects for vehicle animations
#'
#' Intended for internal use only.
#'
#' @param trajectory Single or grouped trajectory object.
#' @param distance_df AVL distance DF.
#' @param plot_trips Vector of trip_id_performed to plot.
#' @param timestep Time in seconds for interpolation.
#' @param distance_lim Vector of (minimum, maximum) distance to plot.
#' @param center_vehicles Should vehicles be centered
#' @param convert_to_timezone Should times be converted to timezones
#' @return plotting dataframe (trips_df)
#' @keywords internal
plot_trips_df_setup <- function(trajectory, distance_df,
                          plot_trips,
                          timestep,
                          distance_lim,
                          center_vehicles,
                          convert_to_timezone) {

  # Check provided trajectories & distance DF, and filter as needed
  if (!is.null(trajectory) & !is.null(distance_df)) {
    rlang::abort(message = "Please provide only one of trajectory and distance_df.",
                 class = "error_plottraj_inputdata")
  } else if (!is.null(trajectory)) {
    # If trajectory is provided, generate the DF by predicting from functions

    # First, verify traj
    if (!("avltrajectory_group" %in% class(trajectory))) {
      rlang::abort(message = "Unrecognized trajectory object. Please use get_trajectory_function() to generate a trajectory object.",
                   class = "error_plottraj_inputdata")
    }
    if ("avltrajectory_single" %in% class(trajectory)) {
      has_inv <- is.function(attr(trajectory, "inv_traj_fun"))
    } else if ("avltrajectory_group" %in% class(trajectory)) {
      has_inv <- is.function(attr(trajectory, "inv_traj_fun")[[1]])
    }

    # Set up trip time extremes, the timepoints at which to interpolate
    # for each trip. Will depend on trajectory type (single or group), wheter
    # a distance limit is provided, and wheter an inverse function is present.
    if (!is.null(distance_lim)) {
      # If a distance limit is present

      # If the traj has an inverse function, use it to plot only the
      # timepoints within that distance range for each trip
      if (has_inv) {

        if ("avltrajectory_single" %in% class(trajectory)) {
          # If single traj, don't need to worry about trips

          extremes_df <- get_trip_extremes(trajectory = trajectory,
                                           filter_trips = plot_trips)
          trip_min_dist <- extremes_df$min_dist[1]
          trip_max_dist <- extremes_df$max_dist[1]
          user_min_dist <- distance_lim[1]
          user_max_dist <- distance_lim[2]

          # Check that there is overlap between the two ranges
          if ((trip_min_dist <= user_max_dist) & (trip_max_dist >= user_min_dist)) {
            absolute_dist_lims <- c(max(user_min_dist, trip_min_dist),
                                    min(user_max_dist, trip_max_dist))
          } else {
            rlang::abort(message = "Trajectory distance range does not overlap with input distance limits.",
                         class = "error_plottraj_inputdata")
          }

          dist_lims_df <- data.frame(trip_end = c("min_time", "max_time"),
                                     distance = absolute_dist_lims)
          # Get trip's enter & exit time for distance_lim
          trip_time_extremes <- predict.avltrajectory_single(object = trajectory,
                                                             new_distances = dist_lims_df) %>%
            dplyr::rename(time_extreme = interp) %>%
            dplyr::select(-distance) %>%
            tidyr::pivot_wider(values_from = "time_extreme", names_from = "trip_end") %>%
            dplyr::mutate(trip_id_performed = unclass(trajectory))

        } else if ("avltrajectory_group" %in% class(trajectory)) {
          # If grouped traj, must worry about trips

          # Get min & max of observed distances & user-defined plotting limits
          trip_extremes_filt <- get_trip_extremes(trajectory = trajectory,
                                             filter_trips = plot_trips) %>%
            dplyr::select(-c(min_time, max_time)) %>%
            dplyr::mutate(user_min_dist = distance_lim[1],
                          user_max_dist = distance_lim[2]) %>%
            # Filter to trips whose observed ranges overlap with user-defined
            dplyr::filter((min_dist <= user_max_dist) &
                            (max_dist >= user_min_dist))

          if (dim(trip_extremes_filt)[1] == 0) {
            rlang::abort(message = "Trajectory distance range does not overlap with input distance limits.",
                         class = "error_plottraj_inputdata")
          }

          trip_extremes <- trip_extremes_filt %>%
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
          trip_time_extremes <- interpolate_times_group(new_dist_trips = trip_extremes,
                                                        inv_trajectory_function = attr(trajectory, "inv_traj_fun")) %>%
            dplyr::rename(time_extreme = interp) %>%
            dplyr::select(-distance) %>%
            tidyr::pivot_wider(values_from = "time_extreme", names_from = "trip_end")
        }
      } else {
        # Distance lim, but no inv fun
        rlang::inform(message = "Distance limit requested, but trajectory does not have inverse. Interpolating over entire route, then filtering; this may be time consuming.",
                      class = "message_plottraj_inputdata")

        trip_time_extremes <- get_trip_extremes(trajectory = trajectory,
                                                filter_trips = plot_trips) %>%
          dplyr::select(-c(min_dist, max_dist))
      }
    } else {
      # If no distance limit, plot entirety of all trips

      # Get time limit of each trip
      trip_time_extremes <- get_trip_extremes(trajectory = trajectory,
                                         filter_trips = plot_trips) %>%
        dplyr::select(-c(min_dist, max_dist))
    }

    # Interpolate via internal function
    trips_df <- interp_df_setup(trajectory = trajectory,
                                trip_time_extremes = trip_time_extremes,
                                timestep = timestep)

    # Need to filter to dist lims in one scenario -- distance_lim but no inverse
    if (!is.null(distance_lim) & !has_inv) {
      trips_df <- trips_df %>%
        dplyr::filter((distance >= distance_lim[1]) &
                        (distance <= distance_lim[2]))

      # Check that observations remain after filtering.
      if (dim(trips_df)[1] == 0) {
        rlang::abort(message = "No trip observations within trip or distance limit.",
                     class = "error_plottraj_inputdata")
      }
    }

    # For all trajectory DFs: adjust to timezone
    if (convert_to_timezone) {
      agency_tz <- attr(trajectory, "agency_tz")
      trips_df <- trips_df %>%
        dplyr::mutate(event_timestamp = as.POSIXct(event_timestamp,
                                                   tz = agency_tz))
    }
  } else if (!is.null(distance_df)) {
    # If distance_df provided, validate it
    needed_fields <- c("trip_id_performed", "event_timestamp", "distance")
    validate_input_to_tides(needed_fields = needed_fields,
                            avl_df = distance_df)

    # Filter to desired trips
    if (is.null(plot_trips)) {
      plot_trips <- unique(distance_df$trip_id_performed)
    }
    trips_df <- distance_df %>%
      dplyr::filter(trip_id_performed %in% plot_trips)

    # Filter to distance limits
    if (!is.null(distance_lims)) {
      trips_df <- trips_df %>%
        dplyr::filter((distance >= distance_lim[1]) &
                        (distance <= distance_lim[2]))
    }

    # Check that observations remain after filtering.
    if (dim(trips_df)[1] == 0) {
      rlang::abort(message = "No trip observations within trip or distance limit.",
                   class = "error_plottraj_inputdata")
    }
  } else {
    # If both trajectory & distance_df are null
    rlang::abort(message = "Please provide one of trajectory or distance_df.",
                 class = "error_plottraj_inputdata")
  }

  # Center trajectories to all begin at same point
  if (center_vehicles) {
    trips_df <- trips_df %>%
      dplyr::mutate(event_timestamp = as.numeric(event_timestamp)) %>%
      dplyr::group_by(trip_id_performed) %>%
      dplyr::mutate(event_timestamp = event_timestamp - min(event_timestamp)) %>%
      dplyr::ungroup()
  }

  return(trips_df)
}

#' Set up feature distances DF
#'
#' Filters features DF down to desired limit, and checks that it meets necessary
#' conditions.
#'
#' @param feature_distances DF of features & their distances
#' @param distance_lim Vector of min & max distances
#' @return A DF of filtered & validated feature distances
#' @keywords internal
plot_feature_df_setup <- function(feature_distances,
                                  distance_lim) {

  # --- Filtering ---
  # Filter observations to distance limits
  if (!is.null(distance_lim)) {
    feature_distances <- feature_distances %>%
      dplyr::filter((distance >= distance_lim[1]) &
                      (distance <= distance_lim[2]))
  }

  # Check that feature values remain after filtering.
  if (dim(feature_distances)[1] == 0) {
    rlang::abort(message = "No features within distance limit.",
                 class = "error_plottraj_inputdata")
  }

  # --- Validation ---
  # Must be dataframe
  if (!is.data.frame(feature_distances)) {
    rlang::abort(message = "Input feature_distances must be a dataframe.",
                 class = "error_plottraj_features")
  }
  # Must contain distance column
  if (!("distance" %in% names(feature_distances))) {
    rlang::abort(message = "feature_distances must include distance column.",
                 class = "error_plottraj_features")
  }
  # distance must be numeric
  if (!is.numeric(feature_distances$distance)) {
    rlang::abort(message = "feature_distances distance column must be numeric.",
                 class = "error_plottraj_features")
  }

  return(feature_distances)
}

#' Function to set up plot formats.
#'
#' Intended for internal use only.
#'
#' @importFrom rlang :=
#' @param plotting_df DF for plotting, either trips or features
#' @param attribute_input The user input value for the attribute (e.g.,
#' outline_input = veh_outline)
#' @param attribute_type The type of attribute being constructed (e.g.,
#' "outline")
#' @param attribute_name The name of the attribute (e.g., "veh_outline")
#' @param user_show_legend Boolean, user input for if legend should be
#' shown.
#' @return List with: 1) new plotting_df, 2) show_legend, 3) attribute_by,
#' and 4) attribute_vals
#' @keywords internal
plot_format_setup <- function(plotting_df,
                              attribute_input,
                              attribute_type,
                              attribute_name,
                              user_show_legend) {

  if (!is.data.frame(attribute_input)) {
    temp_attr_name <- paste("temp_", attribute_name, sep = "")
    show_legend <- "none"
    plotting_df <- plotting_df %>%
      dplyr::mutate(!!rlang::sym(temp_attr_name) := "1")
    attribute_by <- temp_attr_name
    attribute_vals <- c(attribute_input)
    names(attribute_vals) <- "1" # Temp = 1 is a dummy grouping factor to code all plotting_df the same color
  } else if (attribute_type %in% names(attribute_input)) {
    show_legend <- "legend"
    attr_df_names <- names(attribute_input)
    plotting_names <- names(plotting_df)

    # Match outline to a vehicle location data type
    attribute_by <- plotting_names[!is.na(match(plotting_names,
                                                attr_df_names))]
    # Check attribute_by -- should be exaclty one matching column
    if (length(attribute_by) > 1) {
      rlang::abort(message = paste(attribute_name, ": multiple columns match input data. Only one column can match.",
                                   sep = ""),
                   class = "error_plottraj_format")
    } else if (length(attribute_by) == 0) {
      rlang::abort(message = paste(attribute_name, ": no columns match input data. One column must match.",
                                   sep = ""),
                   class = "error_plottraj_format")
    }
    attribute_vals <- attribute_input[[attribute_type]]
    names(attribute_vals) <- as.character(attribute_input[[attribute_by]])
  } else {
    rlang::abort(message = paste(attribute_name, ": ", attribute_type, " column not provided.",
                                 sep = ""),
                 class = "error_plottraj_format")
  }

  # Change legend decision if user overrides
  if (is.null(user_show_legend)) {
    final_show_legend <- show_legend
  } else {
    if (user_show_legend) {
      final_show_legend <- "legend"
    } else {
      final_show_legend <- "none"
    }
  }

  return(list(plotting_df,
              final_show_legend,
              attribute_by,
              attribute_vals
  ))
}
