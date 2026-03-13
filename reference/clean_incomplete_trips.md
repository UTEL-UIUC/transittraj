# Filters out entire trips which do not meet distance or duration requirements.

This function identifies trips that do not meet some acceptable duration
and distance traveled ranges, or that have large time or distance gaps
in the middle. Violating trips will be removed.

## Usage

``` r
clean_incomplete_trips(
  distance_df,
  max_trip_distance = Inf,
  min_trip_distance = -Inf,
  max_trip_duration = Inf,
  min_trip_duration = -Inf,
  max_distance_gap = Inf,
  max_time_gap = Inf,
  return_removals = FALSE
)
```

## Arguments

- distance_df:

  A dataframe of linearized AVL data. Must include `trip_id_performed`,
  `event_timestamp`, and `distance`.

- max_trip_distance:

  Optional. The maximum distance traveled over one trip, in units of
  input `distance`. Default is Inf.

- min_trip_distance:

  Optional. The minimum distance traveled over one trip, in units of
  input `distance`. Default is -Inf.

- max_trip_duration:

  Optional. The maximum event_timestamp duration of one trip, in
  seconds. Default is Inf.

- min_trip_duration:

  Optional. The minimum event_timestamp duration of one trip, in
  seconds. Default is -Inf.

- max_distance_gap:

  Optional. The maximum change in distance between two observations, in
  units of input `distance`. Default is Inf.

- max_time_gap:

  Optional. The maximum time between two observations, in seconds.
  Default is Inf.

- return_removals:

  Optional. A boolean, should the function return a dataframe of trips
  removed and why? Default is `FALSE`.

## Value

The input distance_df, with violating trips removed. If
`return_removals = TRUE`, a dataframe of trips removed and why.

## Examples

``` r
# Set my parameters
my_min_dist <- 500
my_max_gap <- 200

# Get input data
c53_no_jumps <- new_transittraj_data("clean_jumps")

# Run function
c53_clean_trips <- clean_incomplete_trips(distance_df = c53_no_jumps,
                                          min_trip_distance = my_min_dist,
                                          max_distance_gap = my_max_gap)
head(c53_clean_trips)
#> # A tibble: 0 × 11
#> # ℹ 11 variables: location_ping_id <chr>, vehicle_id <chr>,
#> #   trip_id_performed <chr>, service_date <date>, route_id <chr>,
#> #   direction_id <dbl>, speed <dbl>, trip_stop_sequence <dbl>,
#> #   event_timestamp <dttm>, stop_id <int>, distance <dbl>
```
