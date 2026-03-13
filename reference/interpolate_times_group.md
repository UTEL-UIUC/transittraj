# Time interpolation for grouped trajectories

Not intended for external use.

## Usage

``` r
interpolate_times_group(
  trip_extremes = NULL,
  new_distances = NULL,
  new_dist_trips = NULL,
  inv_trajectory_function
)
```

## Arguments

- trip_extremes:

  DF of max and min time values

- new_distances:

  DF of new distance points

- new_dist_trips:

  DF of new trip & distance pairs

- inv_trajectory_function:

  Inverse trajectory function list

## Value

DF of interpolated values
