#' Summarize an AVL trajectory object
#'
#' @description
#' This function creates and prints a list summarizing a single or grouped
#' trajectory object. If the input is a single trajectory, the trip's ID
#' and distance & time range will be printed. If the input is a grouped
#' trajectory, the number of trips and the distance & time range across
#' all trips will be printed. For both, the interpolating curve methods
#' will be printed.
#'
#' @param object A single or grouped trajectory object.
#' @param ... Other parameters (not used).
#' @return A list summarizing the attributes of a fit trajectory.
#' @export
#' @examples
#' # Get input data
#' lineE_traj_grouped <- new_transittraj_data("get_trajectory_fun")
#' lineE_traj_singles <- new_transittraj_data("get_trajectory_fun_single")
#'
#' # Run function: grouped trajectory object
#' summary(lineE_traj_grouped)
#'
#' # Run functions: store summary object
#' lineE_summ <- summary(lineE_traj_grouped)
#' print(lineE_summ$num_trips)
#'
#' # Run function: single trajectory object
#' summary(lineE_traj_singles[[2]])
summary.avltrajectory_group <- function(object, ...) {
  num_trips <- length(object)
  min_dist <- min(attr(object, "min_dist"))
  max_dist <- max(attr(object, "max_dist"))
  min_time <- min(attr(object, "min_time"))
  max_time <- max(attr(object, "max_time"))

  is_traj <- is.function(attr(object, "traj_fun")[[1]])
  traj_type <- attr(object, "traj_type")
  max_deriv <- attr(object, "max_deriv")
  is_inv <- is.function(attr(object, "inv_traj_fun")[[1]])
  inv_tol <- attr(object, "inv_tol")
  used_speeds <-attr(object, "used_speeds")

  summary_obj <- list(
    num_trips = num_trips,
    min_dist = min_dist,
    max_dist = max_dist,
    min_time = min_time,
    max_time = max_time,
    is_traj = is_traj,
    traj_type = traj_type,
    max_deriv = max_deriv,
    is_inv = is_inv,
    inv_tol = inv_tol,
    used_speeds = used_speeds
  )

  class(summary_obj) <- "summary.avltrajectory_group"
  summary_obj
}

#' @rdname summary.avltrajectory_group
#' @export
summary.avltrajectory_single <- function(object, ...) {
  trip_id <- as.vector(object)[1]
  min_dist <- attr(object, "min_dist")
  max_dist <- attr(object, "max_dist")
  min_time <- attr(object, "min_time")
  max_time <- attr(object, "max_time")

  is_traj <- is.function(attr(object, "traj_fun"))
  traj_type <- attr(object, "traj_type")
  max_deriv <- attr(object, "max_deriv")
  is_inv <- is.function(attr(object, "inv_traj_fun"))
  inv_tol <- attr(object, "inv_tol")
  used_speeds <- attr(object, "used_speeds")

  summary_obj <- list(
    trip_id = trip_id,
    min_dist = min_dist,
    max_dist = max_dist,
    min_time = min_time,
    max_time = max_time,
    is_traj = is_traj,
    traj_type = traj_type,
    max_deriv = max_deriv,
    is_inv = is_inv,
    inv_tol = inv_tol,
    used_speeds = used_speeds
  )

  class(summary_obj) <- "summary.avltrajectory_single"
  summary_obj
}

#' Print a trajectory summary.
#'
#' @description
#' Internal functions for printing group and single trajectory summaries.
#'
#' @param x A single or trajectory summary object, returned by summary().
#' @param ... Other parameters (not used).
#' @return Prints summary to console, invisibly returns input object.
#' @keywords internal
#' @export
#' @examples
#' lineE_traj <- new_transittraj_data("get_trajectory_fun")
#' lineE_summ <- summary(lineE_traj)
#'
#' print(lineE_summ)
print.summary.avltrajectory_group <- function(x, ...) {

  cat("------",
      "\nAVL Group Trajectory Object",
      "\n------",
      "\nNumber of trips: ", x$num_trips,
      "\nTotal distance range: ", x$min_dist, " to ", x$max_dist,
      "\nTotal time range: ", x$min_time, " to ", x$max_time,
      "\n------",
      "\nTrajectory function present: ", x$is_traj,
      "\n   --> Trajectory interpolation method: ", x$traj_type,
      "\n   --> Maximum derivative: ", x$max_deriv,
      "\n   --> Fit with speeds: ", x$used_speeds,
      "\nInverse function present: ", x$is_inv,
      "\n   --> Inverse function tolerance: ", x$inv_tol,
      "\n------",
      sep = "")

  invisible(x)
}

#' @rdname print.summary.avltrajectory_group
#' @keywords internal
#' @export
print.summary.avltrajectory_single <- function(x, ...) {

  cat("------",
      "\nAVL Single Trajectory Object",
      "\n------",
      "\nTrip ID: ", x$trip_id,
      "\nTrip distance range: ", x$min_dist, " to ", x$max_dist,
      "\nTrip time range: ", x$min_time, " to ", x$max_time,
      "\n------",
      "\nTrajectory function present: ", x$is_traj,
      "\n   --> Trajectory interpolation method: ", x$traj_type,
      "\n   --> Maximum derivative: ", x$max_deriv,
      "\n   --> Fit with speeds: ", x$used_speeds,
      "\nInverse function present: ", x$is_inv,
      "\n   --> Inverse function tolerance: ", x$inv_tol,
      "\n------",
      sep = "")

  invisible(x)
}

#' Print an AVL trajectory object
#'
#' @description
#' This function prints a one-line report for grouped or single trajectory
#' objects. For a single trajectory, the trip ID will be printed. For grouped
#' trajectories, the number of trips will be printed.
#'
#' @param x A single or grouped trajectory object.
#' @param ... Other parameters (not used).
#' @return A printing character string.
#' @export
#' @examples
#' # Get input data
#' lineE_traj_grouped <- new_transittraj_data("get_trajectory_fun")
#' lineE_traj_singles <- new_transittraj_data("get_trajectory_fun_single")
#'
#' # Print: Grouped trajectory object
#' print(lineE_traj_grouped)
#'
#' # Print: Single trajectory object
#' print(lineE_traj_singles[[2]])
print.avltrajectory_group <- function(x, ...) {
  print(paste("AVL group trajectory with ", length(x), " trips.",
              sep = ""))
}

#' @rdname print.avltrajectory_group
#' @export
print.avltrajectory_single <- function(x, ...) {
  print(paste("AVL single trajectory for trip ID ", as.vector(x),
              sep = ""))
}

#' Quickly plot an AVL trajectory
#'
#' This function generates a quick plot of a single or grouped trajectory
#' object. Using the trajectory function, the entire trajectory will be plotted
#' at a temporal resolution of 10 seconds. For grouped trajectories, a maximum
#' of 50 trips will be plotted. For more control over plotting and
#' formatting, see `plot_trajectory()`.
#'
#' @param x A trajectory object.
#' @param ... Other parameters (not used).
#' @return A `ggplot2` object.
#' @export
#' @examples
#' # Get input data
#' lineE_traj_grouped <- new_transittraj_data("get_trajectory_fun")
#' lineE_traj_singles <- new_transittraj_data("get_trajectory_fun_single")
#'
#' # Plot: Grouped trajectory object
#' plot(lineE_traj_grouped)
#'
#' # Plot: Single trajectory object
#' plot(lineE_traj_singles[[2]])
plot.avltrajectory_group <- function(x, ...) {
  # Get trips to plot
  # Constrain to first 50 only. User can use more customizable function if they want more.
  if (length(x) > 50) {
    rlang::warn(message = "Many trajectories detected. Plotting first 50 only. See plot_trajectory() for additional controls.",
                class = "warn_plotting_groupnum")
    plot_trips <- as.vector(x)[1:50]
  } else {
    plot_trips <- as.vector(x)
  }

  # Get DF
  trip_extremes <- get_trip_extremes(trajectory = x,
                                     filter_trips = plot_trips)
  plot_seq <- seq(from = min(trip_extremes$min_time),
                  to = max(trip_extremes$max_time),
                  by = 10)
  plot_df <- predict.avltrajectory_group(object = x, new_times = plot_seq,
                     trips = plot_trips) %>%
    dplyr::rename(distance = interp)

  # Generate color palette
  # Will be Viridis inferno. In ggplot, sample() is used to randomize each trajectory
  col_vector <- viridis::inferno(n = length(plot_trips))

  # Create plot
  traj_plot <- ggplot2::ggplot(data = plot_df) +
    ggplot2::geom_line(ggplot2::aes(x = event_timestamp, y = distance,
                                    color = factor(trip_id_performed)),
                       linewidth = 0.6, alpha = 0.8) +
    ggplot2::scale_color_manual(values = sample(col_vector, length(plot_trips)),
                                guide = "none") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Epoch Time (sec)",
                  y = "Distance",
                  title = "Many AVL Trajectories",
                  subtitle = paste("Trips ", plot_trips[1], " through ", plot_trips[length(plot_trips)],
                                   "\n(", length(plot_trips), " total)",
                                   sep = ""))
  traj_plot
}

#' @rdname plot.avltrajectory_group
#' @export
plot.avltrajectory_single <- function(x, ...) {
  # Creat DF for plotting
  plot_seq <- seq(from = attr(x, "min_time"),
                  to = attr(x, "max_time"),
                  by = 10)
  plot_df <- predict.avltrajectory_group(object = x, new_times = plot_seq) %>%
    dplyr::rename(distance = interp)

  # Create & return plot
  traj_plot <- ggplot2::ggplot(data = plot_df) +
    ggplot2::geom_line(ggplot2::aes(x = event_timestamp, y = distance),
                       linewidth = 1, color = "coral") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Epoch Time (sec)",
                  y = "Distance",
                  title = "Single AVL Trajectory",
                  subtitle = paste("Trip ", as.vector(x), sep = ""))
  traj_plot
}
