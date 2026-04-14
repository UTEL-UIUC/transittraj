# Get a single trajectory object based on an index.

From a grouped trajectory object and given index number, will return the
single trajectory object at that index. Internal function. Not intended
for external use.

## Usage

``` r
get_traj_index(
  group_traj,
  index_num,
  new_traj_type,
  new_inv_tol,
  new_max_deriv,
  new_used_speeds,
  new_agency_tz
)
```

## Arguments

- group_traj:

  A transittraj avltrajectory_group object.

- index_num:

  Number indicating index to pull trajectory from

- new_traj_type:

  Interp method character string

- new_inv_tol:

  Tolerance used in numeric inverse

- new_max_deriv:

  Max derivative allowed

- new_used_speeds:

  Whether speeds were used

- new_agency_tz:

  Agency's timezone as Olson name

## Value

Single trajectory object
