# Retrieve an object from a particular step of `transittraj`'s workflow

This function runs `transittraj`'s AVL cleaning and trajectory
reconstruction workflow up until a certain point (as defined by
`func_output`), then returns the object at that point. A subset of the
`lacmta_avl` dataset is used. This is primarily intended for use in
testing and examples. The workflow applied here is the same as what is
in
[`vignette("articles/data-workflow-la")`](https://obrien-ben.github.io/transittraj/articles/data-workflow-la.md).

## Usage

``` r
new_transittraj_data(func_output = NULL)
```

## Arguments

- func_output:

  The `transittraj` function to return an output for. Should be a string
  corresponding to the function name. Default is `NULL`, where a vector
  of allowed inputs will be returned.

## Value

The object returned by the specified function.

## Examples

``` r
# Get AVL data after projection onto route
lineE_dists <- new_transittraj_data("get_linear_distances")
head(lineE_dists)
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

# Get a full, fit trajectory
lineE_traj <- new_transittraj_data("get_trajectory_fun")
summary(lineE_traj)
#> ------
#> AVL Group Trajectory Object
#> ------
#> Number of trips: 11
#> Total distance range: 0.6366599 to 35292.87
#> Total time range: 1779886240 to 1779898116
#> ------
#> Trajectory function present: TRUE
#>    --> Trajectory interpolation method: monoH.FC
#>    --> Maximum derivative: 3
#>    --> Fit with speeds: TRUE
#> Inverse function present: TRUE
#>    --> Inverse function tolerance: 0.01
#> ------
```
