# Print function for AVL trajectories

This function prints a one-line report for grouped or single trajectory
objects. For a single trajectory, the trip ID will be printed. For
grouped trajectories, the number of trips will be printed.

## Usage

``` r
# S3 method for class 'avltrajectory_group'
print(x, ...)

# S3 method for class 'avltrajectory_single'
print(x, ...)
```

## Arguments

- x:

  A single or grouped trajectory object.

- ...:

  Other parameters (not used).

## Value

A printing character string.

## Examples

``` r
# Get input data
lineE_traj_grouped <- new_transittraj_data("get_trajectory_fun")
lineE_traj_singles <- new_transittraj_data("get_trajectory_fun_single")

# Print: Grouped trajectory object
print(lineE_traj_grouped)
#> [1] "AVL group trajectory with 11 trips."

# Print: Single trajectory object
print(lineE_traj_singles[[2]])
#> [1] "AVL single trajectory for trip ID 63383917"
```
