#' Filter GTFS to a desired route(s) and direction(s).
#'
#' @description
#' This function returns a new `tidygtfs` object with only the information
#' relevant to your desired routes and directions. All fields included in the
#' input `gtfs` will be filtered. See `Details` for more information about
#' required files and fields
#'
#' @details
#' The following files and fields are required for this function:
#'
#' - `routes`: with `route_id` and `agency_id`
#'
#' - `agency`: with `agency_id`
#'
#' - `trips`: with `route_id`, `direction_id`, `shape_id`, `service_id`, and
#' `trip_id`
#'
#' - `stop_times`: with `stop_id` and `trip_id`
#'
#' The following files are optional. If they are included, the must include
#' the listed fields:
#'
#' - `stops`: with `stop_id`
#'
#' - `shapes`: with `shape_id`
#'
#' - `calendar`: with `service_id`
#'
#' - `calendar_dates`: with `service_id`
#'
#' - `transfers`: with `trip_id` and `stop_id`
#'
#' - `frequencies`: with `trip_id`
#'
#' - `fare_rules`: with `route_id`
#'
#' - `feed_info`
#'
#' For these optional files, the function will detect whether they are present.
#' If so, they will be filtered; if not, they will be left `NULL` in the new
#' GTFS. If any required file or field is missing, an error will be thrown
#' describing what is missing.
#'
#' @param gtfs A tidygtfs object.
#' @param route_ids A numeric vector or single numeric containing the desired
#' route ID(s).
#' @param dir_id Optional. A numeric vector or single numeric containing the
#' desired direction ID(s).
#' @return A tidygtfs object containing only information relevant to the desired
#'  route and direction.
#' @export
filter_by_route <- function(gtfs, route_ids, dir_id = NULL) {
  # --- Check GTFS is tidygtfs object ---
  if (!("tidygtfs" %in% class(gtfs))) {
    stop("Provided GTFS not a tidygtfs object.")
  }

  #  --- Check that required fields are present ---
  # For each file, we will check if it is present & has required fields for matching
  gtfs_val <- attr(gtfs, "validation_result")
  # Agency: Present & contains agency_id
  agency_present <- all(gtfs_val$file_provided_status[gtfs_val$file == "agency"])
  agency_fields <- c(all(gtfs_val$field_provided_status[gtfs_val$field == "agency_id" &
                                               gtfs_val$file == "agency"]))
  # Routes: Present & contains route_id, agency_id
  routes_present <- all(gtfs_val$file_provided_status[gtfs_val$file == "routes"])
  routes_fields <- c(all(gtfs_val$field_provided_status[gtfs_val$field == "agency_id" &
                                               gtfs_val$file == "routes"]),
    all(gtfs_val$field_provided_status[gtfs_val$field == "route_id" &
                                     gtfs_val$file == "routes"]))
  # Trips: Present & contains route_id, trip_id, direction_id, shape_id,
  # service_id
  trips_present <- all(gtfs_val$file_provided_status[gtfs_val$file == "trips"])
  trips_fields <- c(all(gtfs_val$field_provided_status[gtfs_val$field == "route_id" &
                                               gtfs_val$file == "routes"]),
    all(gtfs_val$field_provided_status[gtfs_val$field == "trip_id" &
                                     gtfs_val$file == "trips"]),
    all(gtfs_val$field_provided_status[gtfs_val$field == "direction_id" &
                                     gtfs_val$file == "trips"]),
    all(gtfs_val$field_provided_status[gtfs_val$field == "shape_id" &
                                     gtfs_val$file == "trips"]),
    all(gtfs_val$field_provided_status[gtfs_val$field == "service_id" &
                                     gtfs_val$file == "trips"]))
  # stop_times: Present & contains trip_id, stop_id
  stop_times_present <- all(gtfs_val$file_provided_status[gtfs_val$file == "stop_times"])
  stop_times_fields <- c(all(gtfs_val$field_provided_status[gtfs_val$field == "trip_id" &
                                               gtfs_val$file == "stop_times"]),
    all(gtfs_val$field_provided_status[gtfs_val$field == "stop_id" &
                                     gtfs_val$file == "stop_times"]))
  # stops: Optional & contains stop_id
  stops_present <- all(gtfs_val$file_provided_status[gtfs_val$file == "stops"])
  stops_fields <- c(all(gtfs_val$field_provided_status[gtfs_val$field == "stop_id" &
                                                    gtfs_val$file == "stops"]))
  # shapes: Optional & contains shape_id
  shapes_present <- all(gtfs_val$file_provided_status[gtfs_val$file == "shapes"])
  shapes_fields <- c(all(gtfs_val$field_provided_status[gtfs_val$field == "shape_id" &
                                                    gtfs_val$file == "shapes"]))
  # calendar: Optional & contains service_id
  calendar_present <- all(gtfs_val$file_provided_status[gtfs_val$file == "calendar"])
  calendar_fields <- c(all(gtfs_val$field_provided_status[gtfs_val$field == "service_id" &
                                                      gtfs_val$file == "calendar"]))
  # calendar_dates: Optional & contains service_id
  calendar_dates_present <- all(gtfs_val$file_provided_status[gtfs_val$file == "calendar_dates"])
  calendar_dates_fields <- c(all(gtfs_val$field_provided_status[gtfs_val$field == "service_id" &
                                                        gtfs_val$file == "calendar_dates"]))
  # frequencies: Optional & contains trip_id
  frequencies_present <- all(gtfs_val$file_provided_status[gtfs_val$file == "frequencies"])
  frequencies_fields <- c(all(gtfs_val$field_provided_status[gtfs_val$field == "trip_id" &
                                                            gtfs_val$file == "frequencies"]))
  # transfers: Optional & contains trip_id, stop_id
  transfers_present <- all(gtfs_val$file_provided_status[gtfs_val$file == "transfers"])
  transfers_fields <- c(all(gtfs_val$field_provided_status[gtfs_val$field == "trip_id" &
                                                               gtfs_val$file == "transfers"]),
                        all(gtfs_val$field_provided_status[gtfs_val$field == "stop_id" &
                                                             gtfs_val$file == "transfers"]))
  # fare_rules: Optional & contains route_id
  fare_rules_present <- all(gtfs_val$file_provided_status[gtfs_val$file == "fare_rules"])
  fare_rules_fields <- c(all(gtfs_val$field_provided_status[gtfs_val$field == "route_id" &
                                                             gtfs_val$file == "fare_rules"]))

  # --- Filtering ---
  # Routes
  if (routes_present) {
    if (all(routes_fields)) {
      # Get new route and agencies
      new_routes <- gtfs$routes %>%
        dplyr::filter(route_id %in% route_ids)

      # Check if new routes empty -- no matching route IDs in original GTFS
      if (dim(new_routes)[1] == 0) {
        stop("No matching route_ids in GTFS.")
      }

      new_agency_id <- new_routes %>%
        dplyr::pull(agency_id)
    } else {
      stop(paste("routes missing the following required fields: ",
                 c("agency_id", "route_id")[!routes_fields], sep = ""))
    }
  } else {
    stop("routes not present in GTFS.")
  }

  # Trips
  if (trips_present) {
    if (all(trips_fields)) {
      new_trips <- gtfs$trips %>%
        dplyr::filter(route_id %in% route_ids)
      if (!is.null(dir_id)) {
        # If given, filter for direction IDs
        new_trips <- new_trips %>%
          dplyr::filter(direction_id %in% dir_id)

        # Check if new trips empty -- no matching direction IDs in original GTFS
        if (dim(new_trips)[1] == 0) {
          stop("No matching direction_ids in GTFS.")
        }
      }
      new_trip_ids <- new_trips %>%
        dplyr::pull(trip_id)
      new_shape_ids <- new_trips %>%
        dplyr::pull(shape_id)
      new_service_ids <- new_trips %>%
        dplyr::pull(service_id)
    } else {
      stop(paste("trips missing the following required fields: ",
                 c("route_id", "trip_id", "direction_id", "shape_id",
                   "service_id")[!trips_fields], sep = ""))
    }
  } else {
    stop("trips not present in GTFS.")
  }

  # stop_times
  if (stop_times_present) {
    if (all(stop_times_fields)) {
      new_stop_times <- gtfs$stop_times %>%
        dplyr::filter(trip_id %in% new_trip_ids)
      new_stop_ids <- new_stop_times %>%
        dplyr::pull(stop_id)
    } else {
      stop(paste("stop_times missing the following required fields: ",
                 c("trip_id", "stop_id")[!stop_times_fields], sep = ""))
    }
  } else {
    stop("stop_times not present in GTFS.")
  }

  # stops
  if (stops_present) {
    if (all(stops_fields)) {
      new_stops <- gtfs$stops %>%
        dplyr::filter(stop_id %in% new_stop_ids)
    } else {
      stop(paste("stops missing the following required fields: ",
                 c("stop_id")[!stops_fields], sep = ""))
    }
  } else {
    new_stops <- NULL
  }

  # shapes
  if (shapes_present) {
    if (all(shapes_fields)) {
      new_shapes <- gtfs$shapes %>%
        dplyr::filter(shape_id %in% new_shape_ids)
    } else {
      stop(paste("shapes missing the following required fields: ",
                 c("shape_id")[!shapes_fields], sep = ""))
    }
  } else {
    new_shapes <- NULL
  }

  # agency
  if (agency_present) {
    if (all(agency_fields)) {
      new_agency <- gtfs$agency %>%
        dplyr::filter(agency_id %in% new_agency_id)
    } else {
      stop(paste("agency missing the following required fields: ",
                 c("agency_id")[!agency_fields], sep = ""))
    }
  } else {
    stop("agency not present in GTFS.")
  }

  # calendar
  if (calendar_present) {
    if (all(calendar_fields)) {
      new_calendar <- gtfs$calendar %>%
        dplyr::filter(service_id %in% new_service_ids)
    } else {
      stop(paste("calendar missing the following required fields: ",
                 c("service_id")[!calendar_fields], sep = ""))
    }
  } else {
    new_calendar <- NULL
  }

  # calendar_dates
  if (calendar_dates_present) {
    if (all(calendar_dates_fields)) {
      new_calendar_dates <- gtfs$calendar_dates %>%
        dplyr::filter(service_id %in% new_service_ids)
    } else {
      stop(paste("calendar_dates missing the following required fields: ",
                 c("service_id")[!calendar_dates_fields], sep = ""))
    }
  } else {
    new_calendar_dates <- NULL
  }

  # frequencies
  if (frequencies_present) {
    if (all(frequencies_fields)) {
      new_frequencies <- gtfs$frequencies %>%
        dplyr::filter(trip_id %in% new_trip_ids)
    } else {
      stop(paste("frequencies missing the following required fields: ",
                 c("trip_id")[!frequencies_fields], sep = ""))
    }
  } else {
    new_frequencies <- NULL
  }

  # transfers
  if (transfers_present) {
    if (all(transfers_fields)) {
      new_transfers <- gtfs$transfers %>%
        dplyr::filter((trip_id %in% new_trip_ids) &
                        (stop_id %in% new_stop_ids))
    } else {
      stop(paste("frequencies missing the following required fields: ",
                 c("trip_id")[!frequencies_fields], sep = ""))
    }
  } else {
    new_transfers <- NULL
  }

  # fare_rules
  if (fare_rules_present) {
    if (all(fare_rules_fields)) {
      new_fare_rules <- gtfs$fare_rules %>%
        dplyr::filter(route_id %in% route_ids)
    } else {
      stop(paste("fare_rules missing the following required fields: ",
                 c("route_id")[!fare_rules_fields], sep = ""))
    }
  } else {
    new_fare_rules <- NULL
  }

  # --- Compile into final new GTFS ---
  new_gtfs <- suppressWarnings(tidytransit::as_tidygtfs(list(
    agency = new_agency,
    calendar = new_calendar,
    routes = new_routes,
    shapes = new_shapes,
    stop_times = new_stop_times,
    stops = new_stops,
    trips = new_trips,
    calendar_dates = new_calendar_dates,
    frequencies = new_frequencies,
    transfers = new_transfers,
    fare_rules = new_fare_rules,
    feed_info = gtfs$feed_info,
    fare_attributes = gtfs$fare_attributes)))

  return(new_gtfs)
}

#' Get the geometry of a route shape.
#'
#' @description
#' This function returns an SF multilinestring of the route alignments from
#' GTFS shapes. Similar to tidytransit's `get_geometry()`, but allows filtering
#' by `shape_id` and projection to a new coordinate system. See `Details` for
#' requirements on the input GTFS.
#'
#' @details
#' A `shapes` file must be present in your GTFS object. This file must contain
#' at least the following fields:
#'
#' - `shape_id`
#'
#' - `shape_pt_lat`
#'
#' - `shape_pt_lon`
#'
#' - `shape_pt_sequence`
#'
#' @inheritParams filter_by_route
#' @param shape Optional. The GTFS shape_id to use. Can be a single value, or
#' a vector. Default is NULL, where all `shape_id`s in `gtfs` will be used.
#' @param project_crs Optional. A numeric EPSG identifer indicating the
#' coordinate system to use for spatial calculations. Consider setting to a
#' Euclidian projection, such as the appropriate UTM zone. Default is 4326 (WGS
#' 84 ellipsoid).
#' @return An SF multilinestring, with one multilinestring object per
#' `shape_id`.
#' @export
get_shape_geometry <- function(gtfs, shape = NULL, project_crs = 4326) {

  # --- Validate ---
  # Check if GTFS is tidygtfs object
  if (!("tidygtfs" %in% class(gtfs))) {
    stop("Provided GTFS not a tidygtfs object.")
  }

  # Check if shapes is present and has required fields
  gtfs_val <- attr(gtfs, "validation_result")
  shapes_present <- all(gtfs_val$file_provided_status[gtfs_val$file == "shapes"])
  shapes_fields <- c(all(gtfs_val$field_provided_status[gtfs_val$field == "shape_id" &
                                                          gtfs_val$file == "shapes"]),
                     all(gtfs_val$field_provided_status[gtfs_val$field == "shape_pt_sequence" &
                                                          gtfs_val$file == "shapes"]),
                     all(gtfs_val$field_provided_status[gtfs_val$field == "shape_pt_lat" &
                                                          gtfs_val$file == "shapes"]),
                     all(gtfs_val$field_provided_status[gtfs_val$field == "shape_pt_lon" &
                                                          gtfs_val$file == "shapes"]))
  if (!shapes_present) {
    stop("shapes not present in provided GTFS")
  }
  if (!all(shapes_fields)) {
    stop(paste("shapes missing the following required fields: ",
               c("shape_id", "shape_pt_sequence",
                 "shape_pt_lat", "shape_pt_lon")[!shapes_fields], sep = ""))
  }

  # --- Get geometry ---
  # If specific shape not provided, use all unique shape IDs
  if (is.null(shape)) {
    shape = unique(gtfs$shapes$shape_id)
  }

  # Get raw waypoints from GTFS
  shape_waypoints <- gtfs$shapes %>%
    dplyr::filter(shape_id %in% shape) %>%
    dplyr::rename(lat = shape_pt_lat,
                  lon = shape_pt_lon,
                  seq = shape_pt_sequence) %>%
    dplyr::arrange(shape_id, seq)

  # Convert raw waypoints to SF
  shape_sf <- sf::st_as_sf(shape_waypoints,
                           coords = c("lon", "lat"),
                           crs = 4326) %>%
    sf::st_transform(crs = project_crs) %>%
    dplyr::group_by(shape_id) %>%
    dplyr::summarize(do_union = FALSE) %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_cast("MULTILINESTRING")

  return(shape_sf)
}

#' Projects points to linear distances along a route shape.
#'
#' @description
#' This function takes spatial points and projects them onto a route, returning
#' the linear distance from the beginning terminal of the route.
#'
#' @inheritParams get_shape_geometry
#' @param shape_geometry The SF object to project onto. Must include the field
#' `shape_id`. See `get_shape_geometry()`.
#' @param points Can be either: a dataframe representing point coordinates,
#' with fields `longitude` and `latitude`; or, an SF or SFC point object.
#' @param original_crs Optional. A numeric EPSG identifier. If a dataframe is
#' provided for `points`, this will be used to define the coordinate system of
#' the longitude / latitude values. Default is 4326 (WGS 84 ellipsoid).
#' @return The `points` input (either dataframe or SF) with an appended column
#' for the linear distance along the route. If `points` is an SFC, a vector of
#' numeric distances is returned. Units are those of the spatial projection
#' used (e.g., meters if using UTM).
#' @export
project_onto_route <- function(shape_geometry, points,
                               original_crs = 4326, project_crs = 4326) {

  # Check length of shapes -- should only be one
  if (length(shape_geometry$shape_id) > 1) {
    stop("Please provide only one shape.")
  }

  # Get points SFC
  # If provided is dataframe with longitude and latitude, convert to SF first
  if(is.data.frame(points) & !("sf" %in% class(points))) {
    # If DF, check that has the required fields
    points_fields = c(("longitude" %in% names(points)),
                      ("latitude" %in% names(points)))
    if (!all(points_fields)) {
      stop("points missing the following required fields:",
           c("longitude", "latitude")[!points_fields], sep = " ")
    }
    # Convert to SF
    points_sfc <- points %>%
      sf::st_as_sf(coords = c("longitude", "latitude"),
                   crs = original_crs) %>%
      sf::st_transform(crs = project_crs) %>%
      sf::st_geometry()
  } else if ("sf" %in% class(points)) {
    points_sfc <- sf::st_geometry(points)
  } else if ("sfc" %in% class(points)) {
    points_sfc <- points
  } else {
    stop("Unrecognized points datatype. Please input dataframe, SF, or SFC.")
  }

  if (!all(sf::st_is(points_sfc, "POINT"))) {
    stop("Unrecognized points datatype. Please ensure features are points.")
  }

  # Get route line SFC
  line_sfc <- sf::st_geometry(shape_geometry)
  line_len <- sf::st_length(line_sfc)

  # Project and calculate distance
  dist_norm <- sf::st_line_project(line = line_sfc, point = points_sfc,
                                   normalized = TRUE)
  dist = dist_norm * line_len
  units(dist) <- NULL

  # If input points are SFC, no other attributes to return; just give dist
  if ("sfc" %in% class(points)) {
    return(dist)
  } else if ("sf" %in% class(points)) {
    # If input points are SF, drop geometry and add distance
    points_dist <- points %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(distance = dist)
  } else {
    points_dist <- points %>%
      dplyr::mutate(distance = dist)
  }
  return(points_dist)
}

#' Get the distances of stops along routes.
#'
#' @description
#' This function returns the linear distance of each stop along a route shape,
#' starting from the route's beginning terminal. Unless a `shape_geometry` is
#' provided, stops will be project onto all `shape_id`s that serve them. If a
#' `shape_geometry` is provided, the function will look only for stops served
#' by that shape.
#'
#' @inheritParams filter_by_route
#' @inheritParams get_shape_geometry
#' @param shape_geometry Optional. The SF object to project onto. Must include
#' the field `shape_id`. See `get_shape_geometry()`. Default is NULL, where
#' all shapes in `gtfs` will be used.
#' @return A dataframe containing `stop_id`, the `shape_id` it was projected
#' onto, and `distance`, in units of the spatial projection (e.g., meters if
#' using UTM).
#' @export
get_stop_distances <- function(gtfs, shape_geometry = NULL,
                               project_crs = 4326) {

  # --- Validate gtfs ---
  if (!("tidygtfs" %in% class(gtfs))) {
    stop("Provided GTFS not a tidygtfs object.")
  }
  validate_gtfs_input(gtfs,
                      table = "stops",
                      needed_fields = c("stop_id", "stop_lon", "stop_lat"))
  validate_gtfs_input(gtfs,
                      table = "trips",
                      needed_fields = c("shape_id"))

  # --- Spatial geometry ---
  # Route shape
  if (is.null(shape_geometry)) {
    shape_geometry <- get_shape_geometry(gtfs,
                                         project_crs = project_crs)
  } else {
    # If own shape_geometry provided, ensure it has shape_id
    # Needed to match to appropriate stops
    if (!("shape_id" %in% names(shape_geometry))) {
      stop("shape_id field not found in provided shape_geometry.")
    }
  }

  # Get correct shape_ids & trip_ids
  use_shape_ids <- unique(shape_geometry$shape_id)
  trip_shape_pairs <- gtfs$trips %>%
    dplyr::filter(shape_id %in% use_shape_ids) %>%
    dplyr::distinct(trip_id, shape_id)
  stop_shape_pairs <- gtfs$stop_times %>%
    dplyr::left_join(y = trip_shape_pairs, by = "trip_id") %>%
    dplyr::distinct(stop_id, shape_id)

  # Join stops and shapes
  stops_with_shapes <- gtfs$stops %>%
    # Associate each stop with its shape ID
    dplyr::left_join(y = stop_shape_pairs, by = "stop_id",
                     relationship = "one-to-many") %>%
    dplyr::select(stop_id, stop_lat, stop_lon, shape_id) %>%
    dplyr::filter(!is.na(shape_id))

  if (dim(stops_with_shapes)[1] == 0) {
    stop("No stops served by provided shapes.")
  }

  # Spatial
  stop_points <- stops_with_shapes %>%
    sf::st_as_sf(coords = c("stop_lon", "stop_lat"),
                 crs = 4326) %>%
    sf::st_transform(crs = project_crs)

  # Get distances at each stop point
  num_shapes <- length(shape_geometry$shape_id)
  shape_sfc <- sf::st_geometry(shape_geometry)
  if (num_shapes == 1) {
    # If only one shape
    current_shape_id <- shape_geometry$shape_id[1]
    current_stops <- stop_points %>%
      dplyr::filter(shape_id == current_shape_id)

    stop_dist_df <- project_onto_route(shape_geometry = shape_geometry,
                                       points = stop_points,
                                       project_crs = project_crs) %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(shape_id = current_shape_id)
  } else {
    # If multiple shapes, must project onto one shape at a time
    # Initialize list
    stop_dist_list <- vector("list", num_shapes)
    # Loop through shapes
    for (iter in 1:num_shapes) {
      current_shape_id <- shape_geometry$shape_id[iter]
      current_shape <- shape_sfc[iter]
      current_stops <- stop_points %>%
        dplyr::filter(shape_id == current_shape_id)
      if (dim(current_stops)[1] == 0) {
        next
      }

      stop_dist_list[[iter]] <- project_onto_route(shape_geometry = current_shape,
                                                   points = current_stops,
                                                   project_crs = project_crs) %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(shape_id = current_shape_id)
    }
    # Merge list into single dataframe
    stop_dist_df <- purrr::list_rbind(stop_dist_list)
  }

  return(stop_dist_df)
}

#' Function to quickly validate whether input GTFS has required tables and
#' fields within those tables.
#'
#' Not intended for external use.
validate_gtfs_input <- function(gtfs, table, needed_fields) {

  # Pull validation table
  gtfs_val <- attr(gtfs, "validation_result")

  # Check table presence
  table_present <- all(gtfs_val %>%
                         dplyr::filter(file == table) %>%
                         dplyr::pull(file_provided_status))
  if (!table_present) {
    rlang::abort(message = paste("Table ", table, " missing from input GTFS",
                                 sep = ""),
                 class = "error_gtfsval_missing_table")
  }

  # Check field presence
  fields_present <- gtfs_val %>%
    dplyr::filter(file == table) %>%
    dplyr::filter(field %in% needed_fields) %>%
    dplyr::select(field, field_provided_status) %>%
    dplyr::arrange(match(field, needed_fields))

  if (!all(fields_present$field_provided_status)) {
    missing_fields <- fields_present %>%
      dplyr::filter(!field_provided_status) %>%
      dplyr::pull(field)
    rlang::abort(message = paste(c("The following fields are missing from",
                                   table, ":", missing_fields),
                                 collapse = " "),
                 class = "error_gtfsval_missing_fields")
  }
}
