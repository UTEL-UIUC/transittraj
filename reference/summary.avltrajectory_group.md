# Summary function for AVL trajectories.

This function prints a summary for grouped or single trajectory object
If the input is a single trajectory, the trip's ID and distance & time
range will be printed. If the input is a grouped trajectory, the number
of trips and the distance & time range across all trips will be printed.
For both, the interpolating curve methods will be printed.

## Usage

``` r
# S3 method for class 'avltrajectory_group'
summary(object, ...)

# S3 method for class 'avltrajectory_single'
summary(object, ...)
```

## Arguments

- object:

  A single or grouped trajectory object.

- ...:

  Other parameters (not used).

## Value

A summary character string.

## Examples

``` r
# Get input data
lineE_traj_grouped <- new_transittraj_data("get_trajectory_fun")
lineE_traj_singles <- new_transittraj_data("get_trajectory_fun_single")

# Run function: grouped trajectory object
summary(lineE_traj_grouped)
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

# Run function: single trajectory object
summary(lineE_traj_singles[[2]])
#> ------
#> AVL Single Trajectory Object
#> ------
#> Trip ID: 63383917
#> Trip distance range: 7.863145 to 35227.39
#> Trip time range: 1779887281 to 1779892057
#> ------
#> Trajectory function present: TRUE
#>    --> Trajectory interpolation method: monoH.FC
#>    --> Maximum derivative: 3
#>    --> Fit with speeds: TRUE
#> Inverse function present: TRUE
#>    --> Inverse function tolerance: 0.01
#> ------
```
