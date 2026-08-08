# Group existing trajectory objects or split them apart

Trajectory objects hold the trajectory functions, and related
information, from one or more trip IDs. This function groups the fit
trajectories from multiple trips into one object, or splits a grouped
object into many single trajectory objects, one for each trip. See
[`help(get_trajectory_fun)`](https://obrien-ben.github.io/transittraj/reference/get_trajectory_fun.md)
for more information.

## Usage

``` r
group_trajectories(trajectories, grouping)
```

## Arguments

- trajectories:

  A trajectory object to operate on. Can be a list of single
  trajectories, a list of grouped trajectories, or one grouped
  trajectory.

- grouping:

  A character string, either `"group"` to group all trajectories in
  `trajectories`, or `"split"` to split `trajectories` into a list of
  single trajectories.

## Examples

``` r
# Get input data
lineE_mono <- new_transittraj_data("make_monotonic")

# Fit a list of single trajectory functions
lineE_traj_singles <- get_trajectory_fun(distance_df = lineE_mono,
                                         return_group_function = FALSE)

# Show sample singles
print(length(lineE_traj_singles))
#> [1] 11
print(lineE_traj_singles[[2]])
#> [1] "AVL single trajectory for trip ID 63383917"

# Run function: group singles
lineE_traj_grouped <- group_trajectories(trajectories = lineE_traj_singles,
                                         grouping = "group")
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

# Run function: split apart again
lineE_traj_singles_2 <- group_trajectories(trajectories = lineE_traj_grouped,
                                           grouping = "split")
print(length(lineE_traj_singles_2))
#> [1] 11
print(lineE_traj_singles_2[[2]])
#> [1] "AVL single trajectory for trip ID 63383917"
```
