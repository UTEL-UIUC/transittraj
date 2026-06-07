# Filter out entire trips which do not meet distance or duration requirements

This function identifies trips that do not meet some acceptable duration
and distance traveled ranges, or that have large time or distance gaps
in the middle. Violating trips are removed.

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
  input `distance`. Default is `Inf`.

- min_trip_distance:

  Optional. The minimum distance traveled over one trip, in units of
  input `distance`. Default is `-Inf`.

- max_trip_duration:

  Optional. The maximum duration of one trip, in seconds. Default is
  `Inf`.

- min_trip_duration:

  Optional. The minimum duration of one trip, in seconds. Default is
  `-Inf`.

- max_distance_gap:

  Optional. The maximum change in distance between two observations, in
  units of input `distance`. Default is `Inf`.

- max_time_gap:

  Optional. The maximum time between two observations, in seconds.
  Default is `Inf`.

- return_removals:

  Optional. A boolean, should the function return a dataframe of trips
  removed and why? Default is `FALSE`.

## Value

The input `distance_df`, with violating trips removed. If
`return_removals = TRUE`, a dataframe of trips removed and why.

## Examples

``` r
# Set my parameters
my_min_dist <- 1000
my_max_gap <- 1000

# Get input data
lineE_no_jumps <- new_transittraj_data("clean_jumps")
dim(lineE_no_jumps)
#> [1] 3085   10

# Run function
lineE_clean_trips <- clean_incomplete_trips(distance_df = lineE_no_jumps,
                                            min_trip_distance = my_min_dist,
                                            max_distance_gap = my_max_gap)
dim(lineE_clean_trips)
#> [1] 2250   10
head(lineE_clean_trips)
#> # A tibble: 6 × 10
#>   location_ping_id               service_date trip_id_performed speed vehicle_id
#>   <chr>                          <chr>        <chr>             <dbl> <chr>     
#> 1 4af122e0b668d6821335d641a89ad… 2026-05-27   63383915           1.74 1047-1048…
#> 2 ef3b602e52fe3556a7539491e7792… 2026-05-27   63383915           3.31 1047-1048…
#> 3 a940808be7f3a59066c981bffe3e5… 2026-05-27   63383915           2.15 1047-1048…
#> 4 6df05dfca51b44f25d403356de5a3… 2026-05-27   63383915           0    1047-1048…
#> 5 5326947f997dad696a09f510d4857… 2026-05-27   63383915           0    1047-1048…
#> 6 0eeafa189aab82fe0bff169a9dc58… 2026-05-27   63383915           0    1047-1048…
#> # ℹ 5 more variables: event_timestamp <dttm>, direction_id <int>,
#> #   shape_id <chr>, route_id <chr>, distance <dbl>
```
