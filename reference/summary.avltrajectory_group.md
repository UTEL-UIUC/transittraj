# Summarize an AVL trajectory object

This function creates and prints a list summarizing a single or grouped
trajectory object. If the input is a single trajectory, the trip's ID
and distance & time range will be printed. If the input is a grouped
trajectory, the number of trips and the distance & time range across all
trips will be printed. For both, the interpolating curve methods will be
printed.

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

A list summarizing the attributes of a fit trajectory.

## Examples

``` r
# Get input data
lineE_traj_grouped <- new_transittraj_data("get_trajectory_fun")
lineE_traj_singles <- new_transittraj_data("get_trajectory_fun_single")

# Run function: grouped trajectory object
summary(lineE_traj_grouped)
#> $num_trips
#> [1] 11
#> 
#> $min_dist
#> [1] 0.6366599
#> 
#> $max_dist
#> [1] 35292.87
#> 
#> $min_time
#> [1] 1779886240
#> 
#> $max_time
#> [1] 1779898116
#> 
#> $is_traj
#> [1] TRUE
#> 
#> $traj_type
#> [1] "monoH.FC"
#> 
#> $max_deriv
#> [1] 3
#> 
#> $is_inv
#> [1] TRUE
#> 
#> $inv_tol
#> [1] 0.01
#> 
#> $used_speeds
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "summary.avltrajectory_group"

# Run functions: store summary object
lineE_summ <- summary(lineE_traj_grouped)
print(lineE_summ$num_trips)
#> [1] 11

# Run function: single trajectory object
summary(lineE_traj_singles[[2]])
#> $trip_id
#> [1] "63383917"
#> 
#> $min_dist
#> [1] 7.863145
#> 
#> $max_dist
#> [1] 35227.39
#> 
#> $min_time
#> [1] 1779887281
#> 
#> $max_time
#> [1] 1779892057
#> 
#> $is_traj
#> [1] TRUE
#> 
#> $traj_type
#> [1] "monoH.FC"
#> 
#> $max_deriv
#> [1] 3
#> 
#> $is_inv
#> [1] TRUE
#> 
#> $inv_tol
#> [1] 0.01
#> 
#> $used_speeds
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "summary.avltrajectory_single"
```
