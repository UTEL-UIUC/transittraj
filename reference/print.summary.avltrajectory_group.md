# Print a trajectory summary.

Internal functions for printing group and single trajectory summaries.

## Usage

``` r
# S3 method for class 'summary.avltrajectory_group'
print(x, ...)

# S3 method for class 'summary.avltrajectory_single'
print(x, ...)
```

## Arguments

- x:

  A single or trajectory summary object, returned by summary().

- ...:

  Other parameters (not used).

## Value

Prints summary to console, invisibly returns input object.

## Examples

``` r
lineE_traj <- new_transittraj_data("get_trajectory_fun")
lineE_summ <- summary(lineE_traj)

print(lineE_summ)
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
