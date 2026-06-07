# Interpolate time or distance points using AVL trajectories

This function uses a fit interpolating curve stored in a grouped or
single trajectory object to find new points along each trip's
trajectory. Depending on whether `new_times` or `new_distances` is
provided, the function will utilize the direct or inverse trajectory
function.

## Usage

``` r
# S3 method for class 'avltrajectory_group'
predict(
  object,
  new_times = NULL,
  new_distances = NULL,
  distance_lims = NULL,
  timestep = NULL,
  deriv = 0,
  trips = NULL,
  ...
)
```

## Arguments

- object:

  The single or grouped trajectory object.

- new_times:

  Optional. A vector of numeric timepoints, or a dataframe with at least
  the column `event_timestamp` of new timepoints to interpolate at. May
  also contain the column `trip_id_performed`, which will interpolate
  distances at each trip and time row pair. Default is `NULL`.

- new_distances:

  Optional. A vector of numeric distances, or a dataframe with at least
  the column `distance` of new distances to interpolate at. May also
  contain the column `trip_id_performed`, which will interpolate times
  at each trip and distance row pair. Default is `NULL`.

- distance_lims:

  Optional. A vector of `(minimum, maximum)` distance bounds over which
  to interpolate at a given timestep. If provided, `timestep` must also
  be provided. Default is `NULL`.

- timestep:

  Optional. A single numeric indicating the time interval between
  successive interpolating steps when defining `distance_lims`. If
  provided, `distance_lims` must also be provided. Default is `NULL`.

- deriv:

  Optional. The derivative with which to calculate at. May only be set
  if `new_times` or `distance_lims`/`timestep` is provided, and not if
  `new_distances` is provided. Default is `0` (i.e., position).

- trips:

  Optional. A vector of `trip_id_performed`s to interpolate for. Default
  is `NULL`, which will use all trips found in the trajectory object
  (or, if include, in the `trip_id_performed` column of `new_times` or
  `new_distances`).

- ...:

  Other parameters (not used).

## Value

The input dataframe, with an additional column `interp` of the
interpolated values requested, and an additional `trip_id_performed`
column will all trips for which that point is within range.

## Details

This function is the recommended way to use a fit trajectory function.
It has a few key features:

### Interpolation

There are three ways to interpolate: finding distance from times (direct
trajectory function), times from distance (inverse trajectory function),
or timesteps over a distance range (both inverse and direct trajectory
function). For the former two, either a vector or dataframe of
`new_times` or `new_distances` may be provided. If a dataframe is
provided, it must contain the column `event_timestamp` or `distance`,
and all additional columns will be preserved through the interpolation.

#### Distances from Times

If `new_times` is provided, the function will find the `distance` of
each trip at each point in time. If a dataframe is provided, it must
contain the column `event_timestamp`. This will use the trajectory's
direct function. When using `new_times`, a `deriv` value can also be set
greater than 0. See below for a more detailed discussion.

#### Times from Distances

If `new_distances` is provided, the function will find the
`event_timestamp` of each trip at each point in space. If a dataframe is
provided, it must contain the column `distance`. This will use the
trajectory's inverse function. When using `new_distances`, a `deriv`
value cannot be set greater than 0. See below for a more detailed
discussion.

#### Time & Distance Pairs from Distance Bounds

Oftentimes, you may want to interpolate by small timesteps over a
defined region of space. This can be done by setting `distance_lims` and
`timestep`. The function will use the trajectory's inverse function to
find each trip's entrance and exit time through `distance_lims`, then
create a sequence between these entrance and exit times with a step of
`timestep`. Finally, the trajectory's direct function is used to find
the distance at each of these timepoints. A `deriv` value can also be
set greater than 0 for the final direct interpolation.

If you have a well-defined region of space, this approach allows you to
interpolate vehicle positions at a very tight timescale over a large
number of trips efficiently. You could alternatively use `new_times` to
interpolate over the entire time range of all trips (which wouldn't
require an inverse function), though this may require orders of
magnitude more points and would be substantially less efficient.

### Finding Derivatives

Depending on the `interp_method` used when fitting the trajectory
object, a derivative may be able to be found:

- `interp_method = "linear"`: This will not allow derivatives. This is
  because, at each observation, the piecewise linear function is not
  differentiable.

- `interp_method` is a spline from
  [`stats::splinefun()`](https://rdrr.io/r/stats/splinefun.html): This
  will typically be differentiable up to the third degree (i.e.,
  `deriv = 0` is position, `deriv = 1` is speed, etc.).

The derivative returned (as column `interp`) is the derivative of
distance with respect to time. This means the first derivative is
velocity, second is acceleration, and third is jerk. The derivative is
taken from the direct trajectory, not the inverse, and the inverse
trajectory cannot be used to find derivatives. This means that if
`new_distances` is provided, `deriv` must equal 0. If starting from
distance values, but derivatives are desired, consider interpolating for
timepoints first, then using these as `new_times` to find the
derivative.

### Prevents Extrapolation

By default, many interpolating curves provided by R and `stats` will
allow extrapolation (i.e., the input of an `event_timestamp` or
`distance` beyond the original time or space domain of the trip). In
general, this will not be reasonable for transit vehicles: time points
should be constrained by the time that a trip has actually been
observed, and distances should be constrained to the part of a route a
trip actually ran.

This function uses the maximum and minimum time and distance values
stored in the trajectory object to identify if an input `new_times` or
`new_distances` is beyond the domain/range of each trip individually.
The returned output will only include `interp` values for trips within
the domain/range of the input.

### Accessing the Raw Trajectory Function

Because of the above features and protections, it is recommend that
these [`predict()`](https://rdrr.io/r/stats/predict.html) functions are
used to access the fit trajectory and inverse trajectory functions.
However, if the raw function itself is desired, it can be accessed using
`attr(trajectory, "traj_fun")` or `attr(trajectory, "inv_traj_fun")`.
For a group trajectory object, these will return lists of individual
trip functions indexed by `trip_id_performed`; for single trajectory
objects, these will return the single function for that trip.

## Examples

``` r
# Set my parameters
my_times = seq(from = 1779890000,
               to = 1779893600,
               by = 180)
my_distances = seq(from = 100,
                   to = 35000,
                   by = 5000)
my_distance_lims = c(500, 600)
my_timestep = 10

# Get input data
lineE_traj <- new_transittraj_data("get_trajectory_fun")

# Run function: get distances from times
interp_dists <- predict(object = lineE_traj,
                        new_times = my_times)
dim(interp_dists)
#> [1] 115   3
head(interp_dists)
#>   event_timestamp trip_id_performed       interp
#> 1      1779890000          63383915 24448.633529
#> 2      1779890000          63383917 19442.787256
#> 3      1779890000          63383949     5.021461
#> 4      1779890000          63383991 22745.011225
#> 5      1779890000          63384002  8811.392101
#> 6      1779890000          63384022   487.996271

# Run function: get speeds from times
interp_speeds <- predict(object = lineE_traj,
                         new_times = my_times,
                         deriv = 1)
dim(interp_speeds)
#> [1] 115   3
head(interp_speeds)
#>   event_timestamp trip_id_performed       interp
#> 1      1779890000          63383915 1.627226e+01
#> 2      1779890000          63383917 8.896095e+00
#> 3      1779890000          63383949 5.714286e-05
#> 4      1779890000          63383991 2.101088e+00
#> 5      1779890000          63384002 1.734515e+01
#> 6      1779890000          63384022 1.519963e+01

# Run function: get times from distances
interp_times <- predict(object = lineE_traj,
                        new_distances = my_distances)
dim(interp_times)
#> [1] 70  3
head(interp_times)
#>   distance trip_id_performed     interp
#> 1      100          63383915 1779887121
#> 2      100          63383917 1779888079
#> 3      100          63383949 1779890558
#> 4      100          63383991 1779887571
#> 5      100          63384002 1779889061
#> 6      100          63384022 1779889605

# Run function: get time & distance pairs given distance bounds
interp_time_dist_pairs <- predict(object = lineE_traj,
                                  distance_lims = my_distance_lims,
                                  timestep = my_timestep)
dim(interp_time_dist_pairs)
#> [1] 10  3
head(interp_time_dist_pairs)
#> # A tibble: 6 × 3
#>   trip_id_performed event_timestamp interp
#>   <chr>                       <dbl>  <dbl>
#> 1 63383915              1779887157.   500.
#> 2 63383917              1779888133.   500.
#> 3 63383949              1779890607.   500.
#> 4 63383991              1779887611.   500.
#> 5 63384002              1779889141.   500.
#> 6 63384022              1779890001.   500.
```
