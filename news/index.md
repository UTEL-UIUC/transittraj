# Changelog

## transittraj 1.0.0

*Release: 8/11/2026*

- Publication on CRAN

- Completed automated testing suite

## transittraj 0.1.4

*Release: 6/7/2026*

More substantial improvements to trajectory prediction via
[`predict()`](https://rdrr.io/r/stats/predict.html), with some smaller
bug fixes related to visualization.

- Trajectory prediction:

  - Vectorization of `deriv` input for `new_times` and `distance_lims`/
    `timestep` approach to prediction
    ([\#14](https://github.com/UTEL-UIUC/transittraj/issues/14)). Now, a
    [`predict()`](https://rdrr.io/r/stats/predict.html) output with
    either input type will include a column `deriv`, indicating the
    derivative that row corresponds to. If a vector of length \> 1 is
    input to `deriv` (e.g., `deriv = c(0, 1)`), then each time & trip
    pair will have multiple rows (e.g., a row with `deriv = 0` followed
    by a row with `deriv = 1`, where for each row, `interp` will
    correspond to distance or speed, respectively).

  - By default, [`predict()`](https://rdrr.io/r/stats/predict.html) will
    interpolate at the times or distances provided for all trips in the
    trajectory object. Even if an input dataframe (via `new_distances`
    or `new_times`) already had point-trip pairs (i.e., had a
    `trip_id_performed` column), each row would be duplicated for all
    trips in the trajectory object
    ([\#14](https://github.com/UTEL-UIUC/transittraj/issues/14)). This
    has been changed, so that if `new_distances` or `new_times` already
    has a column `trip_id_performed`, that row will only be interpolated
    for that trip.

- Visualization:

  - [`plot_animated_line()`](https://obrien-ben.github.io/transittraj/reference/plot_animated_line.md)
    would throw an error when setting up the x-axis of the ggplot if
    `feature_distances` was not provided
    ([\#16](https://github.com/UTEL-UIUC/transittraj/issues/16)). This
    has been resolved, and a plot can now be created without features.

  - [`plot_interactive_gtfs()`](https://obrien-ben.github.io/transittraj/reference/plot_interactive_gtfs.md)’s
    `color_palette` input is now case-insensitive when set to `"gtfs"`
    ([\#13](https://github.com/UTEL-UIUC/transittraj/issues/13)).

## transittraj 0.1.3

*Release: 4/14/2026*

- Trajectory grouping: new function
  [`group_trajectories()`](https://obrien-ben.github.io/transittraj/reference/group_trajectories.md)
  allows the user to group together a list of single trajectory objects,
  or split apart a single grouped trajectory object. Read more at
  [`help(group_trajectories)`](https://obrien-ben.github.io/transittraj/reference/group_trajectories.md).

- [`predict()`](https://rdrr.io/r/stats/predict.html) for trajectories:

  - New input parameters for trajectory
    [`predict()`](https://rdrr.io/r/stats/predict.html) methods include
    `distance_lims` and `timestep`. These allow the user to interpolate
    at a specific time interval over a specific region of space. Read
    more at
    [`vignette("articles/intro-trajectories-la")`](https://obrien-ben.github.io/transittraj/articles/intro-trajectories-la.md).

  - Substantial refactoring of internal functions to improve cleanliness
    and readability.

## transittraj 0.1.2

*Release: 3/25/2026*

- Added examples to all function documentation

- GTFS helper functions:

  - New function
    [`get_gtfs_service_dates()`](https://obrien-ben.github.io/transittraj/reference/get_gtfs_service_dates.md):
    Returns a dataframe of dates and their `service_id`s, built from a
    GTFS feed’s `calendar.txt` and/or `calendar_dates.txt` file,
    depending on how the agency has structured these files. Read more at
    [`help(get_gtfs_service_dates)`](https://obrien-ben.github.io/transittraj/reference/get_gtfs_service_dates.md).

  - [`get_gtfs_trajectory_fun()`](https://obrien-ben.github.io/transittraj/reference/get_gtfs_trajectory_fun.md)
    now works for both methods of constructing `calendar.txt` and
    `calendar_dates.txt`
    ([\#2](https://github.com/UTEL-UIUC/transittraj/issues/2)), with
    refactoring through the new
    [`get_gtfs_service_dates()`](https://obrien-ben.github.io/transittraj/reference/get_gtfs_service_dates.md).
    Read more at
    [`help(get_gtfs_trajectory_fun)`](https://obrien-ben.github.io/transittraj/reference/get_gtfs_trajectory_fun.md).

  - [`get_stop_distances()`](https://obrien-ben.github.io/transittraj/reference/get_stop_distances.md)
    will now return all columns in `stops.txt`
    ([\#4](https://github.com/UTEL-UIUC/transittraj/issues/4))

- Plotting functions now include input parameters `feature_legend`,
  `veh_legend`, and `traj_legend` to override whether a layer’s legend
  will appear on the plot. Read more at
  [`help(plot_trajectory)`](https://obrien-ben.github.io/transittraj/reference/plot_trajectory.md)
  and
  [`help(plot_animated_line)`](https://obrien-ben.github.io/transittraj/reference/plot_animated_line.md).

## transittraj 0.1.1

*Release: 3/13/2026*

- Refactoring of
  [`plot_trajectory()`](https://obrien-ben.github.io/transittraj/reference/plot_trajectory.md),
  and redesign of how the function chooses points to interpolate over
  ([\#3](https://github.com/UTEL-UIUC/transittraj/issues/3)).

  - If the user specifies a `distance_lim`, the function will attempt to
    use an inverse trajectory function to find appropriate starting and
    ending timepoints for interpolation; if an inverse function does not
    exist, the user will be informed that interpolation must occur over
    the entire trip.

  - Performance is improved substantially, and performance scales well
    with `plot_trips` length, `distance_lim` range, and `timestep`
    resolution. Most everyday trajectory plots generate in less than 1
    second, and large plots (with hundreds, or even thousands, of
    trajectories) generate in just a few seconds.

  - Improved error handling in
    [`plot_trajectory()`](https://obrien-ben.github.io/transittraj/reference/plot_trajectory.md).
    Error messages should now better inform the user if filtering (i.e.,
    `plot_trips` and `distance_lim`) does not contain any data points.

- Refactoring of [`predict()`](https://rdrr.io/r/stats/predict.html).

  - New exported function
    [`get_trip_extremes()`](https://obrien-ben.github.io/transittraj/reference/get_trip_extremes.md)
    for extracting the time and distance range of each trip stored in a
    trajectory object. Primarily used by
    [`predict()`](https://rdrr.io/r/stats/predict.html) and plotting
    functions, but may be useful for users. Users can filter the output
    dataframe to desired trips. Check out
    [`help(get_trip_extremes)`](https://obrien-ben.github.io/transittraj/reference/get_trip_extremes.md)
    for more information.

  - Improved performance through restructured table operations when
    pairing new interpolating points with individual trips
    ([\#3](https://github.com/UTEL-UIUC/transittraj/issues/3)).

  - New dedicated internal validating function for `new_times` and
    `new_distances` to reduce code duplication and improve error
    messages ([\#2](https://github.com/UTEL-UIUC/transittraj/issues/2)).

## transittraj 0.1.0

*Release: 2/27/2026*

- Initial release of `transittraj` for public use.
