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
lineE_dists <- new_transittraj_data("get_linear_distances")
dim(lineE_dists)
#> [1] 3268   10

# Run function
lineE_no_overlaps <- clean_overlapping_subtrips(distance_df = lineE_dists)
dim(lineE_no_overlaps)
#> [1] 3104   10
head(lineE_no_overlaps)
#>                   location_ping_id service_date trip_id_performed    speed
#> 1 4af122e0b668d6821335d641a89ad312   2026-05-27          63383915 1.743456
#> 2 ef3b602e52fe3556a7539491e7792c74   2026-05-27          63383915 3.308096
#> 3 a940808be7f3a59066c981bffe3e537a   2026-05-27          63383915 2.145792
#> 4 6df05dfca51b44f25d403356de5a3e0a   2026-05-27          63383915 0.000000
#> 5 5326947f997dad696a09f510d4857d2c   2026-05-27          63383915 0.000000
#> 6 0eeafa189aab82fe0bff169a9dc587f7   2026-05-27          63383915 0.000000
#>       vehicle_id     event_timestamp direction_id        shape_id route_id
#> 1 1047-1048-1185 2026-05-27 05:48:58            0 804EB_RC_221121      804
#> 2 1047-1048-1185 2026-05-27 05:49:19            0 804EB_RC_221121      804
#> 3 1047-1048-1185 2026-05-27 05:49:40            0 804EB_RC_221121      804
#> 4 1047-1048-1185 2026-05-27 05:49:59            0 804EB_RC_221121      804
#> 5 1047-1048-1185 2026-05-27 05:50:20            0 804EB_RC_221121      804
#> 6 1047-1048-1185 2026-05-27 05:50:40            0 804EB_RC_221121      804
#>    distance
#> 1 197.58271
#> 2  99.22546
#> 3  98.72317
#> 4  62.66861
#> 5  83.11011
#> 6  31.67278
```
