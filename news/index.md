# Changelog

## transittraj 0.1.1.9000 (development version)

*Latest commit: 3/13/2026*

- Adding examples to function documentation

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
