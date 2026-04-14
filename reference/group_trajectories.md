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
c53_mono <- new_transittraj_data("make_monotonic")

# Fit a list of single trajectory functions
c53_traj_singles <- get_trajectory_fun(distance_df = c53_mono,
                                       return_group_function = FALSE)

# Show sample singles
print(length(c53_traj_singles))
#> [1] 3
print(c53_traj_singles[[2]])
#> [1] "AVL single trajectory for trip ID 13437100"

# Run function: group singles
c53_traj_grouped <- group_trajectories(trajectories = c53_traj_singles,
                                       grouping = "group")
summary(c53_traj_grouped)
#> ------
#> AVL Group Trajectory Object
#> ------
#> Number of trips: 3
#> Total distance range: 0 to 15365.46
#> Total time range: 1771258111 to 1771267268
#> ------
#> Trajectory function present: TRUE
#>    --> Trajectory interpolation method: monoH.FC
#>    --> Maximum derivative: 3
#>    --> Fit with speeds: TRUE
#> Inverse function present: TRUE
#>    --> Inverse function tolerance: 0.01
#> ------

# Run function: split apart again
c53_traj_singles_2 <- group_trajectories(trajectories = c53_traj_grouped,
                                         grouping = "split")
print(length(c53_traj_singles_2))
#> [1] 3
print(c53_traj_singles_2[[2]])
#> [1] "AVL single trajectory for trip ID 13437100"
```
