# Get the distance and time range of each trip in a trajectory object.

This function extracts the time and distance ranges stored in a
trajectory object and formats them into a dataframe for each use. The
dataframe can be filtered to a desired set of `trip_id_performed`s.

## Usage

``` r
get_trip_extremes(trajectory, filter_trips = NULL)
```

## Arguments

- trajectory:

  A trajectory object.

- filter_trips:

  Optional. A vector of `trip_id_performed`s to filter the dataframe to.
  At least one must of `filter_trips` must be present in `trajectory`.
  Default is `NULL`, where all `trip_id_performed`s in `trajectory` are
  returned.

## Value

A dataframe with the columns `trip_id_performed`, `min_time`,
`max_time`, `min_dist`, and `max_dist`.
