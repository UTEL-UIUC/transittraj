#' Validates input to trajectory plotting functions.
#'
#' This function validates that an appropriate combination of trajectory
#' and distance_df are provided, and that they have the necessary features.
#' If a trajectory's inverse function is not present, the user will be warned
#' that interpolation may be time consuming. Internal function.
#'
#' @param trajectory A trajectory objcet
#' @param distance_df a DF with columns distance, event_timestamp,
#' and trip_id_performed
#' @param has_inv a boolean, does the traj object have inv fun?
#' @returns Error if requirements not met.
#' @keywords internal
plot_traj_input_validation <- function(trajectory, distance_df, has_inv) {

  if (!is.null(trajectory) & !is.null(distance_df)) {
    # - Check that both are not provided -
    rlang::abort(message = "Please provide only one of trajectory and distance_df.",
                 class = "error_plottraj_input")
  } else if (!is.null(trajectory)) {

    # - Check trajectory -
    # Is traj object
    if (!("avltrajectory_group" %in% class(trajectory))) {
      rlang::abort(message = "Unrecognized trajectory object. Please use get_trajectory_function() to generate a trajectory object.",
                   class = "error_plottraj_input")
    }
    # Has inverse
    if (!has_inv) {
      rlang::inform(message = "Trajectory does not contain inverse function. Interpolation will occur over entire observed time window, which may be slow.",
                    class = "inform_plottraj_input")
    }
  } else if (!is.null(distance_df)) {

    # - Check distance DF -
    # Check if required fields are present
    required_fields <- c("trip_id_performed", "event_timestamp", "distance")
    fields_present <- required_fields %in% names(distance_df)
    if (!all(fields_present)) {
      fields_missing <- required_fields[!fields_present]
      rlang::abort(message = paste("The following required fields are missing from the provided distance_df:\n",
                                   toString(fields_missing), sep = ""),
                   class = "error_plottraj_input")
    }
    # Check field data types
    if (!is.numeric(distance_df$distance)) {
      rlang::abort(message = "distance_df distance column is incorrect data type. Please provide numeric.",
                   class = "error_plottraj_input")
    }
  } else {

    # - If nothing is provided -
    rlang::abort(message = "Please provide one of trajectory and distance_df.",
                 class = "error_plottraj_input")
  }
}

#' Sets up plotting DF is trajectory is provided.
#'
#' This function uses a trajectory object to create a DF of time, distance
#' points for each trip. If inverse function is present, this will be used
#' to interpolate only over the appropriate distance range of each trip. If
#' not, interpolation will occur over the entire time range of the requested
#' trips. Internal function.
#'
#' @param trajectory A trajectory object
#' @param has_inv a boolean, does the traj object have inv fun?
#' @param timestep A numeric, time interval between interpolated poitns
#' @param distance_lims A vector of (minimum, maximum) distance to interpolate
#' @keywords internal
plot_traj_df_setup <- function(trajectory, has_inv, plot_trips,
                               timestep, distance_lims) {

  if (has_inv) {
    # --- If Inverse Fun ---
    # Get distance lims, either provided or from plot_trips range

    # If distance_lims not provided, create one over all trips
    if (is.null(distance_lims)) {
      trip_extremes <- get_trip_extremes(trajectory = trajectory,
                                         filter_trips = plot_trips)
      distance_lims <- c(min(trip_extremes$min_dist),
                         max(trip_extremes$max_dist))
    }
    # Interpolate
    interp_df <- predict.avltrajectory_group(object = trajectory,
                         trips = plot_trips,
                         distance_lims = distance_lims,
                         timestep = timestep) %>%
      dplyr::rename(distance = interp)

  } else {
    # --- No Inverse Fun ---
    # Must interpolate over entire range

    # Get range
    trip_extremes <- get_trip_extremes(trajectory = trajectory,
                                       filter_trips = plot_trips)
    time_seq <- seq(from = min(trip_extremes$min_time),
                    to = max(trip_extremes$max_time),
                    by = timestep)
    # Interpolate
    interp_df <- predict.avltrajectory_group(object = trajectory,
                         trips = plot_trips,
                         new_times = time_seq) %>%
      dplyr::rename(distance = interp)

    # Filter if needed
    if (!is.null(distance_lims)) {
      interp_df <- interp_df %>%
        dplyr::filter((distance >= distance_lims[1]) &
                        (distance <= distance_lims[2]))
    }
  }

  # Check that points remain after filtering
  if (dim(interp_df)[1] == 0) {
    rlang::abort(message = "No trip observations within trip or distance limit.",
                 class = "error_plottraj_input")
  }

  return(interp_df)
}

#' Set up dataframe & validate of point objects for vehicle animations
#'
#' Intended for internal use only.
#'
#' @param trajectory Single or grouped trajectory object.
#' @param distance_df AVL distance DF.
#' @param plot_trips Vector of trip_id_performed to plot.
#' @param timestep Time in seconds for interpolation.
#' @param distance_lims Vector of (minimum, maximum) distance to plot.
#' @param center_vehicles Should vehicles be centered
#' @param convert_to_timezone Should times be converted to timezones
#' @return plotting dataframe (trips_df)
#' @keywords internal
plot_trips_df_setup <- function(trajectory, distance_df,
                                plot_trips,
                                timestep,
                                distance_lims,
                                center_vehicles,
                                convert_to_timezone) {

  # --- Validation ---
  if ("avltrajectory_single" %in% class(trajectory)) {
    has_inv <- is.function(attr(trajectory, "inv_traj_fun"))
  } else if ("avltrajectory_group" %in% class(trajectory)) {
    has_inv <- is.function(attr(trajectory, "inv_traj_fun")[[1]])
  }
  plot_traj_input_validation(trajectory = trajectory,
                             distance_df = distance_df,
                             has_inv = has_inv)

  # --- Setup trips_df ---
  if (!is.null(trajectory)) {
    # - Setup for Traj -
    trips_df <- plot_traj_df_setup(trajectory = trajectory,
                                   has_inv = has_inv,
                                   plot_trips = plot_trips,
                                   distance_lims = distance_lims,
                                   timestep = timestep)
  } else {
    # - Setup for Dist DF -
    trips_df <- distance_df

    # Filter to plotting limits
    if (!is.null(distance_lims)) {
      trips_df <- trips_df %>%
        dplyr::filter((distance >= distance_lims[1]) &
                        (distance <= distance_lims[2]))
    }
    # Filter to plotting trips
    if (!is.null(plot_trips)) {
      trips_df <- trips_df %>%
        dplyr::filter(trip_id_performed %in% plot_trips)
    }
    # Check that points remain after filtering
    if (dim(trips_df)[1] == 0) {
      rlang::abort(message = "No trip observations within trip or distance limit.",
                   class = "error_plottraj_inputdata")
    }
  }

  # --- Timezone ---
  if (convert_to_timezone) {

    # Get timezone info
    if (!is.null(trajectory)) {
      # If traj, pull from obj
      agency_tz <- attr(trajectory, "agency_tz")
    } else if (class(distance_df$event_timestamp)[1] == "POSIXct") {
      # If df & posixct, pull from original input
      agency_tz <- attr(distance_df$event_timestamp, which = "tz")
    } else {
      # If neither, throw warning
      rlang::warn("No timezone information found in input distance_df. Using current system timezone.",
                  class = "warn_plottraj_inputtz")
      agency_tz <- ""
    }

    # Make conversion
    trips_df <- trips_df %>%
      dplyr::mutate(event_timestamp = as.POSIXct(event_timestamp,
                                                 tz = agency_tz))
  }

  # --- Center ---
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
#' @param distance_lims Vector of min & max distances
#' @return A DF of filtered & validated feature distances
#' @keywords internal
plot_feature_df_setup <- function(feature_distances,
                                  distance_lims) {

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

  # --- Filtering ---
  # Filter observations to distance limits
  if (!is.null(distance_lims)) {
    feature_distances <- feature_distances %>%
      dplyr::filter((distance >= distance_lims[1]) &
                      (distance <= distance_lims[2]))
  }

  # Check that feature values remain after filtering.
  if (dim(feature_distances)[1] == 0) {
    rlang::abort(message = "No features within distance limit.",
                 class = "error_plottraj_inputdata")
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
