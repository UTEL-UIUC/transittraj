#' Internal generic for performing interpolation of distances from times.
#'
#' Performs interpolation of distance values from a DF of times & trip IDs.
#' A generic function, dispatches depending on whether trajectory is grouped
#' or single.
#'
#' @param trajectory Single or grouped trajectory object
#' @param new_times_trips DF with trip_id_performed event_timestamp, and deriv
#' @param ... other inputs, not used
#' @return A DF with appended column "interp" of distance (or deriv) values
#' @keywords internal
interpolate_distances <- function(trajectory,
                                  new_times_trips, ...) {
  UseMethod("interpolate_distances")
}

#' @rdname interpolate_distances
#' @keywords internal
#' @export
interpolate_distances.avltrajectory_single <- function(trajectory,
                                                       new_times_trips, ...) {

  # Pull traj fun
  trajectory_function <- attr(trajectory, "traj_fun")
  max_deriv <- max(new_times_trips$deriv)

  # Interpolate
  if (max_deriv == 0) {
    # Interpolate
    int_df <- new_times_trips %>%
      dplyr::mutate(interp = trajectory_function(event_timestamp))
  } else {
    # Interpolate
    int_df <- new_times_trips %>%
      dplyr::group_by(deriv) %>%
      dplyr::mutate(interp = trajectory_function(event_timestamp,
                                                 deriv = deriv[1])) %>%
      dplyr::ungroup()
  }
  return(int_df)
}

#' @rdname interpolate_distances
#' @keywords internal
#' @export
interpolate_distances.avltrajectory_group <- function(trajectory,
                                                      new_times_trips, ...) {

  # Pull traj fun
  trajectory_function <- attr(trajectory, "traj_fun")
  max_deriv <- max(new_times_trips$deriv)

  # Interpolate
  if (max_deriv == 0) {
    # If deriv is 0, do not pass it
    # Deriv should always default to 0. If function does not take in deriv at all, we would get an error if trying to pass it
    int_df <- new_times_trips %>%
      dplyr::mutate(interp = purrr::map2_dbl(trip_id_performed, event_timestamp,
                                             function(trip_id_performed, event_timestamp) {
                                               trajectory_function[[trip_id_performed]](event_timestamp) }))
  } else {
    int_df <- new_times_trips %>%
      dplyr::group_by(deriv) %>%
      dplyr::mutate(interp = purrr::map2_dbl(trip_id_performed, event_timestamp,
                                             function(trip_id_performed, event_timestamp) {
                                               trajectory_function[[trip_id_performed]](event_timestamp,
                                                                                        deriv = deriv[1]) })) %>%
      dplyr::ungroup()
  }

  return(int_df)
}


#' Internal generic for performing interpolation of times from distances.
#'
#' Performs interpolation of time values from a DF of distances & trip IDs.
#' A generic function, dispatches depending on whether trajectory is grouped
#' or single.
#'
#' @param trajectory Single or grouped trajectory object
#' @param new_dist_trips A DF with trip_id_performed and distance
#' @param ... other inputs, not used
#' @return A DF with appended column "interp" of event_timestamp values
#' @keywords internal
interpolate_times <- function(trajectory, new_dist_trips, ...) {
  UseMethod("interpolate_times")
}

#' @rdname interpolate_times
#' @keywords internal
#' @export
interpolate_times.avltrajectory_single <- function(trajectory,
                                                   new_dist_trips, ...) {
  # Pull inv traj fun
  inv_trajectory_function <- attr(trajectory, "inv_traj_fun")

  # Interpolate
  int_df <- new_dist_trips %>%
    dplyr::mutate(interp = inv_trajectory_function(distance))
  return(int_df)
}

#' @rdname interpolate_times
#' @keywords internal
#' @export
interpolate_times.avltrajectory_group <- function(trajectory,
                                                  new_dist_trips, ...) {

  # Pull inv traj fun
  inv_trajectory_function <- attr(trajectory, "inv_traj_fun")

  # Interpoalte
  int_df <- new_dist_trips %>%
    dplyr::mutate(interp = purrr::map2_dbl(trip_id_performed, distance,
                                           function(trip_id_performed, distance) {
                                             inv_trajectory_function[[trip_id_performed]](distance)}))

  return(int_df)
}
