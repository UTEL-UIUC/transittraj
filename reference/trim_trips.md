# Removes observations occurring before a trip's minimum distance, or after a trip's maximum distance.

Sometimes observations will be recorded under a trip ID while a vehicle
is still traveling in the opposite direction. Conversely, a trip may
continue recording as it begins traversing the opposite direction. This
function attempts to remove these observations by identifying each
trip's minimum (beginning) and maximum (ending) distance, then filtering
to only observations after and before these points. For both ends, the
first occurrence of the beginning/maximum value is used.

## Usage

``` r
trim_trips(distance_df, trim_type = "both", return_removals = FALSE)
```

## Arguments

- distance_df:

  A dataframe of linearized AVL data. Must include `trip_id_performed`,
  `event_timestamp`, and `distance`.

- trim_type:

  Optional. A string, indicating whether the beginning of trips, end of
  trips, or both beginning and end of trips should be trimmed. Must be
  one of "beginning", "end", or "both". Default is "beginning".

- return_removals:

  Optional. A boolean, should the function return a dataframe of points
  removed and why? Default is `FALSE`.

## Value

The input `distance_df` with violating points removed. If
`return_removals = TRUE`, a dataframe with observations removed and why.

## Examples

``` r
# Get input data
lineE_clean_trips <- new_transittraj_data("clean_incomplete_trips")
dim(lineE_clean_trips)
#> [1] 2250   10

# Run function
lineE_trimmed <- trim_trips(distance_df = lineE_clean_trips)
dim(lineE_trimmed)
#> [1] 2130   10
head(lineE_trimmed)
#> # A tibble: 6 × 10
#>   location_ping_id               service_date trip_id_performed speed vehicle_id
#>   <chr>                          <chr>        <chr>             <dbl> <chr>     
#> 1 0eeafa189aab82fe0bff169a9dc58… 2026-05-27   63383915              0 1047-1048…
#> 2 3436b03ae5feecd4947d7a0d1d49d… 2026-05-27   63383915              0 1047-1048…
#> 3 02939a6bc750b44fad6c842fa8be6… 2026-05-27   63383915              0 1047-1048…
#> 4 1a479dd51a750a1e37b7be112ee00… 2026-05-27   63383915              0 1047-1048…
#> 5 55ecfed9996432eeca7dfcb016701… 2026-05-27   63383915              0 1047-1048…
#> 6 822d16b82b32c422ce9d25ec61fe6… 2026-05-27   63383915              0 1047-1048…
#> # ℹ 5 more variables: event_timestamp <dttm>, direction_id <int>,
#> #   shape_id <chr>, route_id <chr>, distance <dbl>
```
