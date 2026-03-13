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
c53_clean_trips <- new_transittraj_data("clean_incomplete_trips")
dim(c53_clean_trips)
#> [1] 627  11

# Run function
c53_trimmed <- trim_trips(distance_df = c53_clean_trips)
dim(c53_trimmed)
#> [1] 625  11
head(c53_trimmed)
#> # A tibble: 6 × 11
#>   location_ping_id vehicle_id trip_id_performed service_date route_id
#>   <chr>            <chr>      <chr>             <date>       <chr>   
#> 1 12620            2836       1306100           2026-02-16   C53     
#> 2 12647            2836       1306100           2026-02-16   C53     
#> 3 12728            2836       1306100           2026-02-16   C53     
#> 4 12809            2836       1306100           2026-02-16   C53     
#> 5 12890            2836       1306100           2026-02-16   C53     
#> 6 12971            2836       1306100           2026-02-16   C53     
#> # ℹ 6 more variables: direction_id <dbl>, speed <dbl>,
#> #   trip_stop_sequence <dbl>, event_timestamp <dttm>, stop_id <int>,
#> #   distance <dbl>
```
