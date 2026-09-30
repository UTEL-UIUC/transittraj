# transittraj 1.1.0

*Release: 9/2/2026*

* Rewritten backend of spatial functions (`get_linear_distances()` and
`project_onto_route()`) to use `geos`:

  * For simplicity, all user-facing inputs and outputs still utilize `sf`; only
  backend code has changed.
  
  * Performance is improved substantially; up to an order-of-magnitude
  processing time improvements for large datasets in `get_linear_distances()`.

* Carto basemaps now require an API key. Removed reliance on or
references to Carto in:

  * Data workflow vignettes mapping raw AVL (`vignette("avl-data-workflow")`
  and `vignette("articles/avl-data-workflow-la")`).
  
  * Default basemap for `plot_animated_map()`.

# transittraj 1.0.0

*Release: 8/11/2026*

* Publication on CRAN

* Completed automated testing suite

# transittraj 0.1.4

*Release: 6/7/2026*

More substantial improvements to trajectory prediction via `predict()`, with
some smaller bug fixes related to visualization.

* Trajectory prediction:

  * Vectorization of `deriv` input for `new_times` and `distance_lims`/
  `timestep` approach to prediction (#14). Now, a `predict()` output with either
  input type will include a column `deriv`, indicating the derivative that
  row corresponds to. If a vector of length > 1 is input to `deriv` (e.g.,
  `deriv = c(0, 1)`), then each time & trip pair will have multiple
  rows (e.g., a row with `deriv = 0` followed by a row with `deriv = 1`,
  where for each row, `interp` will correspond to distance or speed,
  respectively).

  * By default, `predict()` will interpolate at the times or distances provided
  for all trips in the trajectory object. Even if an input dataframe (via
  `new_distances` or `new_times`) already had point-trip pairs (i.e., had a
  `trip_id_performed` column), each row would be duplicated for all trips in
  the trajectory object (#14). This has been changed, so that if
  `new_distances` or `new_times` already has a column `trip_id_performed`,
  that row will only be interpolated for that trip.

* Visualization:

  * `plot_animated_line()` would throw an error when setting up
  the x-axis of the ggplot if `feature_distances` was not provided (#16).
  This has been resolved, and a plot can now be created without features.
  
  * `plot_interactive_gtfs()`'s `color_palette` input is now case-insensitive
  when set to `"gtfs"` (#13).

# transittraj 0.1.3

*Release: 4/14/2026*

* Trajectory grouping: new function `group_trajectories()` allows the user
to group together a list of single trajectory objects, or split apart a
single grouped trajectory object. Read more at `help(group_trajectories)`.
  
* `predict()` for trajectories:

  * New input parameters for trajectory `predict()` methods include
  `distance_lims` and `timestep`. These allow the user to interpolate at a
  specific time interval over a specific region of space. Read more at
  `vignette("articles/intro-trajectories-la")`.
  
  * Substantial refactoring of internal functions to improve cleanliness
  and readability.

# transittraj 0.1.2

*Release: 3/25/2026*

* Added examples to all function documentation

* GTFS helper functions:

  * New function `get_gtfs_service_dates()`: Returns a dataframe of dates and
  their `service_id`s, built from a GTFS feed's `calendar.txt` and/or
  `calendar_dates.txt` file, depending on how the agency has structured these
  files. Read more at `help(get_gtfs_service_dates)`.
  
  * `get_gtfs_trajectory_fun()` now works for both methods of constructing
  `calendar.txt` and `calendar_dates.txt` (#2), with refactoring through the
  new `get_gtfs_service_dates()`. Read more at `help(get_gtfs_trajectory_fun)`.

  * `get_stop_distances()` will now return all columns in `stops.txt` (#4)
  
* Plotting functions now include input parameters `feature_legend`,
`veh_legend`, and `traj_legend` to override whether a layer's legend will
appear on the plot. Read more at `help(plot_trajectory)` and
`help(plot_animated_line)`.

# transittraj 0.1.1

*Release: 3/13/2026*

* Refactoring of `plot_trajectory()`, and redesign of how the function
chooses points to interpolate over (#3).

  * If the user specifies a `distance_lim`, the function will attempt to use
  an inverse trajectory function to find appropriate starting and ending
  timepoints for interpolation; if an inverse function does not exist, the user
  will be informed that interpolation must occur over the entire trip.
  
  * Performance is improved substantially, and performance scales well with
  `plot_trips` length, `distance_lim` range, and `timestep` resolution.
  Most everyday trajectory plots generate in less than 1 second, and large
  plots (with hundreds, or even thousands, of trajectories) generate in just a
  few seconds.

  * Improved error handling in `plot_trajectory()`. Error messages should now
  better inform the user if filtering (i.e., `plot_trips` and `distance_lim`)
  does not contain any data points.

* Refactoring of `predict()`.

  * New exported function `get_trip_extremes()` for extracting the time and
  distance range of each trip stored in a trajectory object. Primarily used
  by `predict()` and plotting functions, but may be useful for users. Users can
  filter the output dataframe to desired trips. Check out
  `help(get_trip_extremes)` for more information. 

  * Improved performance through restructured table operations when pairing
  new interpolating points with individual trips (#3).
  
  * New dedicated internal validating function for `new_times` and
  `new_distances` to reduce code duplication and improve error messages (#2).

# transittraj 0.1.0

*Release: 2/27/2026*

* Initial release of `transittraj` for public use.
