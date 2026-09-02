#' Filter GTFS to a desired route(s) and direction(s)
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
#' The following files are optional. If included, they must include
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
#' @param gtfs A `tidygtfs` object.
#' @param route_ids A vector containing the desired route ID(s).
#' @param dir_id Optional. A vector containing the desired direction ID(s).
#' @return A tidygtfs object containing only information relevant to the desired
#' route and direction.
#' @export
#' @examples
#' # Set my parameters
#' my_route <- "804"
#' my_dir <- 0
#'
#' # Filter WMATA GTFS
#' lineE_gtfs <- filter_by_route(gtfs = lacmta_gtfs,
#'                               route_ids = my_route,
#'                               dir_id = my_dir)
#' summary(lineE_gtfs)
filter_by_route <- function(gtfs, route_ids, dir_id = NULL) {

  # --- Validate fields ---
  gtfs_val <- tidytransit::validate_gtfs(gtfs)
  # routes: route_id, agency_id
  validate_gtfs_input(gtfs = gtfs,
                      table = "routes",
                      needed_fields = c("route_id", "agency_id"))

  # agency: agency_id
  validate_gtfs_input(gtfs = gtfs,
                      table = "agency",
                      needed_fields = c("agency_id"))

  # trips: route_id, direction_id, shape_id, trip_id, service_id
  validate_gtfs_input(gtfs = gtfs,
                      table = "trips",
                      needed_fields = c("route_id", "direction_id", "shape_id",
                                 "trip_id", "service_id"))

  # stop_times: route_id, agency_id
  validate_gtfs_input(gtfs = gtfs,
                      table = "stop_times",
                      needed_fields = c("stop_id", "trip_id"))

  # stops: stop_id (optional)
  stops_present <- ifelse(test = all(gtfs_val %>%
                                       dplyr::filter(file == "stops") %>%
                                       dplyr::pull(file_provided_status)),
                          yes = (dim(gtfs$stops)[1] > 0),
                          no = FALSE)
  if (stops_present) {
    validate_gtfs_input(gtfs = gtfs,
                        table = "stops",
                        needed_fields = c("stop_id"))
  }

  # shapes: shape_id (optional)
  shapes_present <- ifelse(test = all(gtfs_val %>%
                                                dplyr::filter(file == "shapes") %>%
                                                dplyr::pull(file_provided_status)),
                                   yes = (dim(gtfs$shapes)[1] > 0),
                                   no = FALSE)
  if (shapes_present) {
    validate_gtfs_input(gtfs = gtfs,
                        table = "shapes",
                        needed_fields = c("shape_id"))
  }

  # calendar: service_id (optional)
  calendar_present <- ifelse(test = all(gtfs_val %>%
                                                dplyr::filter(file == "calendar") %>%
                                                dplyr::pull(file_provided_status)),
                                   yes = (dim(gtfs$calendar)[1] > 0),
                                   no = FALSE)
  if (calendar_present) {
    validate_gtfs_input(gtfs = gtfs,
                        table = "calendar",
                        needed_fields = c("service_id"))
  }

  # calendar_dates: service_id (optional)
  calendar_dates_present <- ifelse(test = all(gtfs_val %>%
                                           dplyr::filter(file == "calendar_dates") %>%
                                           dplyr::pull(file_provided_status)),
                              yes = (dim(gtfs$calendar_dates)[1] > 0),
                              no = FALSE)
  if (calendar_dates_present) {
    validate_gtfs_input(gtfs = gtfs,
                        table = "calendar_dates",
                        needed_fields = c("service_id"))
  }

  # transfers: trip_id, stop_id (optional)
  transfers_present <- ifelse(test = all(gtfs_val %>%
                                       dplyr::filter(file == "transfers") %>%
                                       dplyr::pull(file_provided_status)),
                          yes = (dim(gtfs$transfers)[1] > 0),
                          no = FALSE)
  if (transfers_present) {
    validate_gtfs_input(gtfs = gtfs,
                        table = "transfers",
                        needed_fields = c("from_trip_id", "to_trip_id",
                                          "from_stop_id", "to_stop_id"))
  }

  # frequencies: service_id (optional)
  frequencies_present <- ifelse(test = all(gtfs_val %>%
                                           dplyr::filter(file == "frequencies") %>%
                                           dplyr::pull(file_provided_status)),
                              yes = (dim(gtfs$frequencies)[1] > 0),
                              no = FALSE)
  if (frequencies_present) {
    validate_gtfs_input(gtfs = gtfs,
                        table = "frequencies",
                        needed_fields = c("trip_id"))
  }

  # fare_rules: route_id (optional)
  fare_rules_present <- ifelse(test = all(gtfs_val %>%
                                           dplyr::filter(file == "fare_rules") %>%
                                           dplyr::pull(file_provided_status)),
                              yes = (dim(gtfs$fare_rules)[1] > 0),
                              no = FALSE)
  if (fare_rules_present) {
    validate_gtfs_input(gtfs = gtfs,
                        table = "fare_rules",
                        needed_fields = c("route_id"))
  }

  # --- Filtering Required ---
  # Routes
  new_routes <- gtfs$routes %>%
    dplyr::filter(route_id %in% route_ids)
  # Check if new routes empty -- no matching route IDs in original GTFS
  if (dim(new_routes)[1] == 0) {
    rlang::abort(message = "No matching route_ids in GTFS.",
                 class = "error_gtfsfilt_none")
  }
  new_agency_id <- new_routes %>%
    dplyr::pull(agency_id)

  # agency
  new_agency <- gtfs$agency %>%
    dplyr::filter(agency_id %in% new_agency_id)

  # Trips
  new_trips <- gtfs$trips %>%
    dplyr::filter(route_id %in% route_ids)
  if (!is.null(dir_id)) {
    # If given, filter for direction IDs
    new_trips <- new_trips %>%
      dplyr::filter(direction_id %in% dir_id)
    # Check if new trips empty -- no matching direction IDs in original GTFS
    if (dim(new_trips)[1] == 0) {
      rlang::abort(message = "No matching direction_ids in GTFS route.",
                   class = "error_gtfsfilt_none")
    }
  }
  new_trip_ids <- new_trips %>%
    dplyr::pull(trip_id)
  new_shape_ids <- new_trips %>%
    dplyr::pull(shape_id)
  new_service_ids <- new_trips %>%
    dplyr::pull(service_id)

  # stop_times
  new_stop_times <- gtfs$stop_times %>%
    dplyr::filter(trip_id %in% new_trip_ids)
  new_stop_ids <- new_stop_times %>%
    dplyr::pull(stop_id)

  # --- Create initial list ---
  new_gtfs <- list(
    agency = new_agency,
    routes = new_routes,
    trips = new_trips,
    stop_times = new_stop_times)

  # --- Filter not required ---
  # stops
  if (stops_present) {
    new_stops <- gtfs$stops %>%
      dplyr::filter(stop_id %in% new_stop_ids)
    new_gtfs <- append(new_gtfs,
                       list(stops = new_stops))
  }

  # shapes
  if (shapes_present) {
    new_shapes <- gtfs$shapes %>%
      dplyr::filter(shape_id %in% new_shape_ids)
    new_gtfs <- append(new_gtfs,
                       list(shapes = new_shapes))
  }

  # calendar
  if (calendar_present) {
    new_calendar <- gtfs$calendar %>%
      dplyr::filter(service_id %in% new_service_ids)
    new_gtfs <- append(new_gtfs,
                       list(calendar = new_calendar))
  }

  # calendar_dates
  if (calendar_dates_present) {
    new_calendar_dates <- gtfs$calendar_dates %>%
      dplyr::filter(service_id %in% new_service_ids)
    new_gtfs <- append(new_gtfs,
                       list(calendar_dates = new_calendar_dates))
  }

  # frequencies
  if (frequencies_present) {
    new_frequencies <- gtfs$frequencies %>%
      dplyr::filter(trip_id %in% new_trip_ids)
    new_gtfs <- append(new_gtfs,
                       list(frequencies = new_frequencies))
  }

  # transfers
  if (transfers_present) {
    new_transfers <- gtfs$transfers %>%
      dplyr::filter((trip_id %in% new_trip_ids) &
                      (stop_id %in% new_stop_ids))
    new_gtfs <- append(new_gtfs,
                       list(transfers = new_transfers))
  }

  # fare_rules
  if (fare_rules_present) {
    new_fare_rules <- gtfs$fare_rules %>%
      dplyr::filter(route_id %in% route_ids)
    new_gtfs <- append(new_gtfs,
                       list(fare_rules = new_fare_rules))
  }

  # --- Compile into final new GTFS ---
  new_tidygtfs <- tidytransit::as_tidygtfs(new_gtfs)
  return(new_tidygtfs)
}

#' Get the geometry of a route shape
#'
#' @description
#' This function returns an SF multilinestring of the route alignments from
#' GTFS shapes. Similar to `tidytransit::get_geometry()`, but allows filtering
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
#' @param shape Optional. A vector of GTFS `shape_id`s to pull.
#' Default is NULL, where all `shape_id`s in `gtfs` will be used.
#' @param project_crs Optional. A numeric EPSG identifer indicating the
#' coordinate system to use for spatial calculations. Consider setting to a
#' Euclidian projection, such as the appropriate UTM zone. Default is 4326 (WGS
#' 84 ellipsoid).
#' @return An SF multilinestring, with one multilinestring object per
#' `shape_id`.
#' @export
#' @examples
#' # Set my parameters
#' my_shape <- "804EB_RC_221121"
#' my_crs = 32611
#'
#' # Get shape from WMATA GTFS
#' lineE_shape <- get_shape_geometry(gtfs = lacmta_gtfs,
#'                                   shape = my_shape,
#'                                   project_crs = my_crs)
#' print(lineE_shape)
get_shape_geometry <- function(gtfs, shape = NULL, project_crs = 4326) {

  # --- Validate ---
  # Check if GTFS is tidygtfs object
  validate_gtfs_input(gtfs,
                      table = "shapes",
                      needed_fields = c("shape_id", "shape_pt_sequence",
                                        "shape_pt_lat", "shape_pt_lon"))

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

  # Check that shape exists
  if (dim(shape_waypoints)[1] == 0) {
    rlang::abort(message = "No matching shape_ids in GTFS.",
                 class = "error_gtfsshape_none")
  }

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

#' Project points to linear distances along a route
#'
#' @description
#' This function takes spatial points and projects them onto a route (i.e.,
#' "snaps" them to the nearest point on the shape), returning
#' the linear distance of each point along the route shape, starting from
#' the route's beginning terminal.
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
#' set in `project_crs` (e.g., meters if using WGS UTM).
#' @export
#' @examples
#' # Set my parameters
#' my_crs <- 32611
#'
#' # Get shape data
#' lineE_shape <- new_transittraj_data("get_shape_geometry")
#'
#' # Set points of interest
#' my_points <- data.frame(longitude = c(-118.270924, -118.230056),
#'                         latitude = c(34.033895, 34.047884),
#'                         poi_name = c("Flower & Washington",
#'                                      "1st St Viaduct"))
#'
#' # Run project_onto_route
#' my_points_proj <- project_onto_route(shape_geometry = lineE_shape,
#'                                      points = my_points,
#'                                      project_crs = my_crs)
#' head(my_points_proj)
project_onto_route <- function(shape_geometry, points,
                               original_crs = 4326, project_crs = 4326) {


  # --- Validate shape geometry ---
  validate_shape_geometry(shape_geometry,
                          require_shape_id = TRUE,
                          max_length = 1,
                          match_crs = project_crs)

  # --- Points ---
  # If provided is dataframe with longitude and latitude, convert to SF first
  if(is.data.frame(points) & !("sf" %in% class(points))) {
    # If DF, check that has the required fields
    points_fields = c(("longitude" %in% names(points)),
                      ("latitude" %in% names(points)))
    if (!all(points_fields)) {
      rlang::abort(message = paste(c("points missing the following required fields:",
                                     c("longitude", "latitude")[!points_fields]),
                                   collapse = " "),
                   class = "error_pointsval_fields")
    }
    # Convert to SFC
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
    rlang::abort(message = "Unrecognized points datatype. Please input dataframe, SF, or SFC.",
                 class = "error_pointsval_datatype")
  }

  # Check that SFC is points
  if (!all(sf::st_is(points_sfc, "POINT"))) {
    rlang::abort(message = "Unrecognized points datatype. Please ensure features are points.",
                 class = "error_pointsval_geomtype")
  }

  # Convert cleaned SFC points to geos
  points_geos <- geos::as_geos_geometry(points_sfc)

  # --- Route ---
  shape_geos <- geos::as_geos_geometry(shape_geometry)

  # --- Projection ---
  dist <- geos::geos_project(geom2 = points_geos,
                             geom1 = shape_geos)

  # --- Cleanup ---
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

#' Get the distances of stops along routes
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
#' the field `shape_id`. See `get_shape_geometry()`. Default is `NULL`, where
#' all shapes in `gtfs` will be used.
#' @return A dataframe containing `stop_id`, the `shape_id` it was projected
#' onto, and `distance`, in units of the spatial projection (e.g., meters if
#' using WGS UTM).
#' @export
#' @examples
#' # Set my parameters
#' my_shape <- "804EB_RC_221121"
#' my_crs <- 32611
#' my_route <- "804"
#' my_dir <- 0
#'
#' # Get needed GTFS data
#' lineE_gtfs <- filter_by_route(gtfs = lacmta_gtfs, route_ids = my_route,
#'                               dir_id = 0)
#' lineE_shape <- get_shape_geometry(gtfs = lacmta_gtfs, shape = my_shape,
#'                                   project_crs = my_crs)
#'
#' # Run stop distances function
#' lineE_stop_dists <- get_stop_distances(gtfs = lineE_gtfs,
#'                                        shape_geometry = lineE_shape,
#'                                        project_crs = my_crs) %>%
#'    dplyr::select(-c(stop_desc, stop_url, tpis_name, location_type))
#' head(lineE_stop_dists)
get_stop_distances <- function(gtfs, shape_geometry = NULL,
                               project_crs = 4326) {

  # --- Validate gtfs ---
  validate_gtfs_input(gtfs,
                      table = "stops",
                      needed_fields = c("stop_id", "stop_lon", "stop_lat"))
  validate_gtfs_input(gtfs,
                      table = "trips",
                      needed_fields = c("shape_id", "trip_id"))

  # --- Validate shape_geometry ---
  if (is.null(shape_geometry)) {
    # If not provided, take using shape_geometry -- should be OK
    shape_geometry <- get_shape_geometry(gtfs,
                                         project_crs = project_crs)
  } else {
    # If provided by user, validate
    validate_shape_geometry(shape_geometry,
                            require_shape_id = TRUE,
                            max_length = Inf,
                            match_crs = project_crs)
  }

  # --- Spatial geometry ---
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
    dplyr::filter(!is.na(shape_id))

  if (dim(stops_with_shapes)[1] == 0) {
    rlang::abort(message = "No stops served by provided shapes.",
                 class = "error_stopdists_inputshapes")
  }

  # Spatial
  stop_points <- stops_with_shapes %>%
    sf::st_as_sf(coords = c("stop_lon", "stop_lat"),
                 crs = 4326) %>%
    sf::st_transform(crs = project_crs)

  # Get distances at each stop point
  num_shapes <- length(shape_geometry$shape_id)
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
      current_shape <- shape_geometry %>%
        dplyr::filter(shape_id == current_shape_id)
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

#' Generate a Leaflet viewer of GTFS routes and stops
#'
#' @description
#' This function generates a simple Leaflet-based interactive map viewer of a
#' GTFS. This function is intended for quick and easy visualization of a GTFS
#' feed. As such, formatting options are relatively limited.
#'
#' @details
#'
#' ## Route Shapes and Stops
#'
#' The primary goal of this function is to visualize and explore each GTFS
#' shape, including its associated `route_id` and `direction_id`. This function
#' will plot all shapes and stops present in the input `gtfs`. To plot only
#' a specific route or direction, first filter the feed using
#' `filter_by_route()`.
#'
#' Routes have both pop-ups and hover labels. The hover label shows the
#' shapes's `route_id` (from the `trips` file). The pop-up will show the
#' `route_id`, `direction_id`, and `shape_id`.
#'
#' Stops also have both pop-ups and hover labels. The hover label will show the
#' point's `stop_id` (from the `stops` file). The pop-up will show the
#' `stop_name` and `stop_id`.
#'
#' ## Formatting
#'
#' Two formatting options are available through this function: basemaps
#' and route color palettes.
#'
#' The `background` parameter allows you to customize the background basemap.
#' Esri's light grey canvas is the default, as it
#' is excellent for providing geographic context while still allowing the
#' routes to stand out. To see the available options, type
#' `leaflet::providers$` into your console.
#'
#' The route colors can be customized in two different ways:
#'
#' - Using the `gtfs`'s colors. Typically, the `routes` file in a GTFS feed
#' will contain a field `route_color`; this is the color you see in most
#' public-facing mapping/navigation applications (e.g., Google Maps,
#' Transit App, etc.). If this is present in the input `gtfs` feed, setting
#' `color_palette = "gtfs"` will use this field to color each shape.
#'
#' - Using a named color palette. Without `gtfs` colors, this function
#' assigns colors categorically (using `leaflet::colorFactor()`). To set
#' the palette, input a string corresponding to a palette name from
#' `RColorBrewer`, a palette name from `viridis`, a vector of color names (with
#' the same length as the number of shapes), or some other color function. See
#' Leaflet's
#' [colors vignette](https://rstudio.github.io/leaflet/articles/colors.html)
#' for more information.
#'
#' @param gtfs A tidytransit GTFS object.
#' @param background Optional. A string for the background of the transit map,
#' from Leaflet's provider library (see `leaflet::providers$`). Default is
#' Esri's light gray canvas (`"Esri.WorldGrayCanvas"`).
#' @param color_palette Optional. A string for the Leaflet color palette to
#' color routes. If `"gtfs"`, will use color codes in the GTFS `routes` file.
#' Default is `"Dark2"`.
#' @return A Leaftlet object.
#' @export
#' @examples
#' plot_interactive_gtfs(gtfs = lacmta_gtfs,
#'                       color_palette = "gtfs")
plot_interactive_gtfs <- function(gtfs,
                                  background = "Esri.WorldGrayCanvas",
                                  color_palette = "Dark2") {

  # --- Validate input GTFS ---
  validate_gtfs_input(gtfs,
                      table = "stops",
                      needed_fields = c("stop_id", "stop_lon", "stop_lat",
                                        "stop_name"))
  validate_gtfs_input(gtfs,
                      table = "trips",
                      needed_fields = c("shape_id", "direction_id", "route_id"))
  if (tolower(color_palette) == "gtfs") {
    validate_gtfs_input(gtfs,
                        table = "routes",
                        needed_fields = c("route_color"))
  }

  # --- Get GTFS geometries ---
  # Routes
  shape_geometry <- get_shape_geometry(gtfs = gtfs)

  # Stops
  stops_sf <- gtfs$stops %>%
    dplyr::select(stop_name, stop_id, stop_lon, stop_lat) %>%
    sf::st_as_sf(coords = c("stop_lon", "stop_lat"),
                 crs = 4326)


  # --- Formatting ---
  # Should equal dimension of shape_geometry, assuming each shape_id has
  # one direction & route
  shapes_info <- gtfs$trips %>%
    dplyr::distinct(shape_id, direction_id, route_id)
  shape_geometry <- shape_geometry %>%
    dplyr::left_join(y = shapes_info, by = "shape_id")

  # Popups & labels
  route_popup <- paste("Route: ", shape_geometry$route_id,
                       "<br>Direction: ", shape_geometry$direction_id,
                       "<br>Shape: ", shape_geometry$shape_id,
                       sep = "")
  route_hover <- as.character(shape_geometry$route_id)
  stop_popup <- paste(stops_sf$stop_name, " (", stops_sf$stop_id, ")", sep = "")
  stop_hover <- as.character(stops_sf$stop_id)

  # Colors
  if (tolower(color_palette) == "gtfs") {
    # Get color codes and append #
    route_colors <- gtfs$routes %>%
      dplyr::select(route_id, route_color) %>%
      dplyr::mutate(route_color = paste("#", route_color, sep = ""))

    # Create palette
    route_pal <- leaflet::colorFactor(route_colors$route_color, route_colors$route_id)
  } else {
    # Create palette
    route_pal <- leaflet::colorFactor(color_palette, shape_geometry$route_id)
  }

  # --- Create map ---
  interactive_map <- leaflet::leaflet() %>%
    leaflet::addPolylines(data = shape_geometry, # Route alignment
                          color = ~route_pal(route_id),
                          opacity = 1,
                          label = route_hover,
                          popup = route_popup) %>%
    leaflet::addLegend(data = shape_geometry, # Legend
                       position ="bottomright",
                       pal = route_pal,
                       values = ~route_id,
                       title = "Route ID") %>%
    leaflet::addCircleMarkers(data = stops_sf, # Stops
                              fillColor = "white",
                              fillOpacity = 1,
                              color = "black",
                              opacity = 1,
                              weight = 1,
                              radius = 3,
                              label = stop_hover,
                              popup = stop_popup) %>%
    leaflet::addProviderTiles(background) # Basemap

  return(interactive_map)
}

#' Get a dataframe of all service dates and their service IDs from a GTFS
#'
#' This function returns a dataframe with each date covered by a GTFS and the
#' `service_id` run on that date. This data is extracted from the `calendar.txt`
#' and `calendar_dates.txt` files, depending on how the GTFS is structured. See
#' `Details` for a discussion.
#'
#' @details
#' The GTFS standard allows for two different structurings of `calendar.txt`
#' and `calendar_dates.txt`:
#'
#' - Standard service in `calendar.txt`, with exceptions in
#' `calendar_dates.txt`. Here, `calendar.txt` will list the standard service ID
#' by weekday (e.g., Monday, Tuesday, etc.), and `calendar_dates.txt` lists
#' specific dates which are exceptions to this. In this scenario,
#' `get_gtfs_service_dates()` will get enumerate all weekdays and dates
#' in `calendar.txt`, and assign the correct `service_id` to it, depending on
#' if the date is listed as an exception in `calendar_dates.txt`.
#'
#' - All dates of service are enumerated in `calendar_dates.txt`, and
#' `calendar.txt` is not used. In this scenario, `get_gtfs_service_dates()`
#' will simply filter, clean, and return this table.
#'
#' Use the input parameter `use_calendar_table` to control which method to use.
#' If `use_calendar_table = "calendar"`, the former method will be used; if
#' `use_calendar_table = "calendar_dates"`, the latter will be used. To
#' restrict the date enumeration to only a specific window, set `date_min`
#' and `date_max`.
#'
#' This function is also intended for GTFS feeds with only one service ID per
#' day. Some GTFS providers (including `lacmta_gtfs`) have unique `service_id`s
#' by route, and thus service dates do not have unique `service_id`s.
#' Consider filtering your GTFS to a single route before using this function
#' (see `filter_by_route()`). If there are multiple service IDs on a given
#' day, the first appearing will be returned.
#'
#' @param gtfs A tidygtfs object.
#' @param date_min Optional. The starting (earliest possible) `Date` object for
#' the returned dataframe. Default is `NULL`, where the earliest date in the
#' GTFS will be used.
#' @param date_max Optional. The ending (latest possible) `Date` object for the
#' returned dataframe. Default is `NULL`, where the latest date in the GTFS
#' will be used.
#' @param use_calendar_table Optional. Should the GTFS's `calendar.txt` or
#' `calendar_dates.txt` be used for the feasible date range? Must be
#' `"calendar"` or `"calendar_dates"`. Default is `"calendar"`.
#' @return A dataframe with `Date` column `date` and character column
#' `service_id`.
#' @export
#' @examples
#' # Set parameters
#' study_date <- as.Date("2026-05-27")
#'
#' # Get needed input data
#' lineE_gtfs <- filter_by_route(gtfs = lacmta_gtfs, route_ids = "804",
#'                               dir_id = 0)
#'
#' # Run function: get service ID by day in date range
#' study_service_ids <- get_gtfs_service_dates(gtfs = lineE_gtfs,
#'                                             date_min = study_date,
#'                                             date_max = study_date,
#'                                             use_calendar_table = "calendar")
#' print(study_service_ids)
get_gtfs_service_dates <- function(gtfs,
                                   date_min = NULL, date_max = NULL,
                                   use_calendar_table = "calendar") {

  # --- Initial validation ---
  # use_calendar_table
  if (!use_calendar_table %in% c("calendar", "calendar_dates")) {
    rlang::abort(message = "Unrecognized use_calendar_table type. Please input either \"calendar\" or \"calendar_dates\".",
                 class = "error_gtfsdate_inputdata")
  }
  # date_min & date_max
  if (!("Date" %in% class(date_min) | is.null(date_min))) {
    rlang::abort(message = "Unrecognized date_min data type. Please input Date class (see as.Date()).",
                 class = "error_gtfsdate_inputdata")
  }
  if (!("Date" %in% class(date_max) | is.null(date_max))) {
    rlang::abort(message = "Unrecognized date_max data type. Please input Date class (see as.Date()).",
                 class = "error_gtfsdate_inputdata")
  }
  if ((!is.null(date_min)) & (!is.null(date_max))) {
    # If both date_min & date_max are provided, check that min occurs before max
    if (date_min > date_max) {
      rlang::abort(message = "Input date_min occurs after input date_max.",
                   class = "error_gtfsdate_inputdata")
    }
  }


  if (use_calendar_table == "calendar") {
    # --- Using calendar.txt ---

    # - Validate GTFS -
    # calendar_dates: service_id, date
    validate_gtfs_input(gtfs,
                        table = "calendar_dates",
                        needed_fields = c("date", "service_id",
                                          "exception_type"))
    gtfs$calendar_dates$date <- as.Date(gtfs$calendar_dates$date)
    # calendar:
    validate_gtfs_input(gtfs,
                        table = "calendar",
                        needed_fields = c("start_date", "end_date",
                                          "service_id",
                                          "monday",
                                          "tuesday",
                                          "wednesday",
                                          "thursday",
                                          "friday",
                                          "saturday",
                                          "sunday"))
    gtfs$calendar$start_date <- as.Date(gtfs$calendar$start_date)
    gtfs$calendar$end_date <- as.Date(gtfs$calendar$end_date)

    # - Check dates -
    gtfs_date_min <- min(as.Date(gtfs$calendar$start_date))
    gtfs_date_max <- max(as.Date(gtfs$calendar$end_date))
    if (!is.null(date_min)) {
      if ((date_min > gtfs_date_max) | (date_min < gtfs_date_min)) {
        rlang::abort(message = "Input date_min occurs after GTFS date range.",
                     class = "error_gtfsdate_inputdata")
      }
    } else {
      date_min <- gtfs_date_min
    }
    if (!is.null(date_max)) {
      if ((date_max < gtfs_date_min) | (date_max > gtfs_date_max)) {
        rlang::abort(message = "Input date_max occurs before GTFS date range.",
                     class = "error_gtfsdate_inputdata")
      }
    } else {
      date_max <- gtfs_date_max
    }

    # Get scheduled & exception service_ids for each date
    service_ids_by_wkday <- data.frame(wkday = c("monday",
                                                 "tuesday",
                                                 "wednesday",
                                                 "thursday",
                                                 "friday",
                                                 "saturday",
                                                 "sunday"),
                                       sched_id = c(gtfs$calendar$service_id[which(gtfs$calendar$monday == 1)[1]],
                                                    gtfs$calendar$service_id[which(gtfs$calendar$tuesday == 1)[1]],
                                                    gtfs$calendar$service_id[which(gtfs$calendar$wednesday == 1)[1]],
                                                    gtfs$calendar$service_id[which(gtfs$calendar$thursday == 1)[1]],
                                                    gtfs$calendar$service_id[which(gtfs$calendar$friday == 1)[1]],
                                                    gtfs$calendar$service_id[which(gtfs$calendar$saturday == 1)[1]],
                                                    gtfs$calendar$service_id[which(gtfs$calendar$sunday == 1)[1]]))
    service_exceptions <- gtfs$calendar_dates %>%
      dplyr::filter(exception_type == 1) %>%
      dplyr::rename(excep_id = service_id) %>%
      dplyr::select(-exception_type)

    # Get DF of all dates and their correct ID (scheduled service_id or exception service_id)
    all_dates <- data.frame(date = seq(from = date_min, to = date_max, by = 1)) %>%
      dplyr::mutate(wkday = tolower(weekdays(date))) %>%
      dplyr::left_join(y = service_ids_by_wkday, by = "wkday",
                       relationship = "many-to-one") %>%
      dplyr::left_join(y = service_exceptions, by = "date") %>%
      dplyr::mutate(service_id = dplyr::if_else(condition = is.na(excep_id),
                                         true = sched_id,
                                         false = excep_id)) %>%
      dplyr::select(-c(sched_id, excep_id, wkday))

  } else {
    # --- Using calendar_dates.txt ---

    # - Validate GTFS -
    # calendar_dates: service_id, date
    validate_gtfs_input(gtfs,
                        table = "calendar_dates",
                        needed_fields = c("date", "service_id"))
    gtfs$calendar_dates$date <- as.Date(gtfs$calendar_dates$date)

    # - Check dates -
    gtfs_date_min <- min(as.Date(gtfs$calendar_dates$date))
    gtfs_date_max <- max(as.Date(gtfs$calendar_dates$date))
    if (!is.null(date_min)) {
      if (date_min > gtfs_date_max) {
        rlang::abort(message = "Input date_min occurs after GTFS date range.",
                     class = "error_gtfsdate_inputdata")
      }
    } else {
      date_min <- gtfs_date_min
    }
    if (!is.null(date_max)) {
      if (date_max < gtfs_date_min) {
        rlang::abort(message = "Input date_max occurs before GTFS date range.",
                     class = "error_gtfsdate_inputdata")
      }
    } else {
      date_max <- gtfs_date_max
    }

    all_dates <- gtfs$calendar_dates %>%
      dplyr::filter((date >= date_min) & (date <= date_max)) %>%
      dplyr::select(service_id, date)
  }

  return(all_dates)
}
