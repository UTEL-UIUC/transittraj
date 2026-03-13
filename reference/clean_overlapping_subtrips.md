# Removes trips with multiple overlapping operators or vehicles assigned to the same trip number.

In some AVL vendors, multiple vehicles or operators may be logged to the
same trip ID at the same time. This may be acceptable in some scenarios
(e.g., a vehicle/operator tradeoff mid-trip). Other times, it may be an
error, with these distinct (trip, vehicle, operator) truples running
simulataneously. This function identifies both scenarios, and gives the
option to remove one or both.

## Usage

``` r
clean_overlapping_subtrips(
  distance_df,
  check_operator = FALSE,
  remove_single_observations = TRUE,
  remove_non_overlapping = FALSE,
  return_removals = FALSE
)
```

## Arguments

- distance_df:

  A dataframe of linearized AVL data. Must include `event_timestamp`,
  `trip_id_performed`, and `vehicle_id`. Optionally, may include
  `operator_id`.

- check_operator:

  Optional. A boolean, should overlaps of multiple `operator_id`s be
  checked for? Default is FALSE.

- remove_single_observations:

  Optional. A boolean, should subtrips with only one observation be
  removed? Default is TRUE.

- remove_non_overlapping:

  Optional. A boolean, should trips with multiple vehicles or operators
  that do not overlap be removed? Default is FALSE.

- return_removals:

  Optional. A boolean, should the function return a dataframe of trips
  removed and why? Default is FALSE.

## Value

The input distance_df, with violating trips removed. If return_removals
= TRUE, a dataframe with trip IDs and the reason why it was identified
for removal.

## Examples

``` r
# Get input data
c53_dists <- new_transittraj_data("get_linear_distances")

# Run function
c53_no_overlaps <- clean_overlapping_subtrips(distance_df = c53_dists)
head(c53_no_overlaps)
#>   location_ping_id vehicle_id trip_id_performed service_date route_id
#> 1               25       5539          13300100   2026-02-16      C53
#> 2               52       5539          13300100   2026-02-16      C53
#> 3              106       5539          13300100   2026-02-16      C53
#> 4              187       5539          13300100   2026-02-16      C53
#> 5              268       5539          13300100   2026-02-16      C53
#> 6              349       5539          13300100   2026-02-16      C53
#>   direction_id  speed trip_stop_sequence     event_timestamp stop_id distance
#> 1            0 7.9248                 19 2026-02-16 10:58:25    3679 4159.641
#> 2            0 6.4008                 20 2026-02-16 10:58:36   17578 4249.421
#> 3            0 3.9624                 20 2026-02-16 10:59:06   17578 4292.869
#> 4            0 3.9624                 20 2026-02-16 10:59:31   17578 4318.141
#> 5            0 1.2192                 22 2026-02-16 11:00:01   17417 4495.498
#> 6            0 2.4384                 22 2026-02-16 11:00:31   17417 4507.870
```
