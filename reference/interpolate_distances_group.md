# Distance interpolation for group trajectories

Not intended for external use.

## Usage

``` r
interpolate_distances_group(
  trip_extremes = NULL,
  new_times = NULL,
  new_times_trips = NULL,
  trajectory_function,
  deriv
)
```

## Arguments

- trip_extremes:

  DF of max and min distance values

- new_times:

  DF of new time points, not paired with trips

- new_times_trips:

  DF of already-paired trip IDs & timepoints

- trajectory_function:

  trajectory function list

- deriv:

  derivative to use

## Value

DF of interpolated values
