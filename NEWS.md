# transittraj 0.1.1.9000 (development version)

*Latest commit: 3/16/2026*

* Adding examples to function documentation

* New function `get_gtfs_service_dates()`: Returns a dataframe of dates and
their `service_id`s, build from a GTFS feed's `calendar.txt` and/or
`calendar_dates.txt` file, depending on how the agency has structured these
files.

* `get_gtfs_trajectory_fun()` now works for both methods of constructing
`calendar.txt` and `calendar_dates.txt` (#2), with refactoring through the new
`get_gtfs_service_dates()`. Read more at `help(get_gtfs_service_dates)`.

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
