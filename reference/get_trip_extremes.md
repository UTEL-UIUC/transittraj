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

## Examples

``` r
# Get input data
c53_traj <- new_transittraj_data("get_trajectory_fun")

# Run function
c53_extremes <- get_trip_extremes(c53_traj)
print(c53_extremes)
#>   trip_id_performed   min_dist max_dist   min_time   max_time
#> 1           1306100 0.04191312 15356.17 1771262290 1771267268
#> 2          13437100 0.00000000 15365.46 1771258111 1771263913
#> 3          35294100 0.04191312 15363.48 1771259264 1771264338
```
