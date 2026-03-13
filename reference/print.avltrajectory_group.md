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
c53_traj <- new_transittraj_data("get_trajectory_fun")

# Print
print(c53_traj)
#> [1] "AVL group trajectory with 3 trips."
```
