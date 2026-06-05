# Group existing trajectory objects or split them apart.

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

# Run function: split apart again
lineE_traj_singles_2 <- group_trajectories(trajectories = lineE_traj_grouped,
                                           grouping = "split")
print(length(lineE_traj_singles_2))
#> [1] 11
print(lineE_traj_singles_2[[2]])
#> [1] "AVL single trajectory for trip ID 63383917"
```
