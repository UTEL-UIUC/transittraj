# Interpolate time or distance points using AVL trajectories.

Using a function stored in a grouped or single trajectory object, new
points will be interpolated along a trajectory. Depending on whether
new_times or new_distances is provided, the function will utilize the
direct or inverse trajectory function.

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
  the column `"event_timestamp"` of new timepoints to interpolate at.
  Default is `NULL`.

- new_distances:

  Optional. A vector of numeric distances, or a dataframe with at least
  the column `"distance"` of new distances to interpolate at. Default is
  `NULL`.

- distance_lims:

  Optional. A vector of (minimum, maximum) distance bounds over which to
  interpolate at a given timestep. If provided, `timestep` must also be
  provided. Default is `NULL`.

- timestep:

  Optional. A single numeric indicating the time interval between
  successive interpolating steps when defining `distance_lims`. If
  provided, `distance_lims` must also be provided. Default is `NULL`.

- deriv:

  Optional. The derivative with which to calculate at. Default is `0`.

- trips:

  Optional. A vector of `trip_id_performed`s to interpolate for. Default
  is `NULL`, which will use all trips found in the trajectory object.

- ...:

  Other parameters (not used).

## Value

The input dataframe, with an additional column `"interp"` of the
interpolated values requested, and an additional `"trip_id_performed"`
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
direct function. When using `new_times`, a `deriv` value can also be
set. See below for a more detailed discussion.

#### Times from Distances

If `new_distances` is provided, the function will find the
`event_timestamp` of each trip at each point in space. If a dataframe is
provided, it must contain the column `distance`. This will use the
trajectory's inverse function. When using `new_distances`, a `deriv`
value cannot be set. See below for a more detailed discussion.

#### Time & Distance Pairs from Distance Bounds

Oftentimes, you may want to interpolate by small timesteps over a
defined region of space. This can be done by setting `distance_lims` and
`timestep`. The function will use the trajectory's inverse function to
find each trip's entrance and exit time through `distance_lims`, then
create a sequence between these entrance and exit times with a step of
`timestep`. Finally, the trajectory's direct function is used to find
the distance at each of these timepoints. A `deriv` value can also be
set for the final direct interpolation.

If you have a well-defined region of space, this approach allows you to
interpolate vehicle positions at a very tight timescale over a large
number of trips efficiently. You could alternatively use `new_times` to
interpolate over the entire time range of all trips (which wouldn't
require an inverse function), though this may require orders of
magnitude more points and would be substantially less efficient.

### Finding Derivatives

Depending on the `interp_method` used when fitting the trajectory
object, a its derivative may be able to be found:

- `interp_method = "linear"`. This will not allow derivatives. This is
  because, at each observation, the piecewise linear function is not
  differentiable.

- `interp_method` is a spline from
  [`stats::splinefun()`](https://rdrr.io/r/stats/splinefun.html). This
  will typically be differentiable up to the third degree.

The derivative returned (as column `interp`) is the derivative of
distance with respect to time. This means the first derivative is
velocity, second is acceleration, and third is jerk. The derivative is
taken from the direct trajectory, not the inverse, and the inverse
trajectory cannot be used to find derivatives. This means that if
`new_distances` is provided, `deriv ` must equal 0. If starting from
distance values, but derivatives are desired, consider interpolating for
timepoints first, then using these as `new_times` to find the
derivative.

### Prevents Extrapolation

By default, many fit interpolating curves will allow extrapolation
(i.e., the input of an `event_timestamp` beyond the original time domain
of the trip). In general, this will not be reasonable for transit
vehicles: time points should be constrained by the time that a trip has
actually been observed, and distances should be constrained to the part
of a route a trip actually ran.

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
my_times = seq(from = 1771260000,
               to = 1771264000,
               by = 180)
my_distances = seq(from = 0,
                   to = 15000,
                   by = 1000)
my_distance_lims = c(500, 600)
my_timestep = 10

# Get input data
c53_traj <- new_transittraj_data("get_trajectory_fun")

# Run function: get distances from times
interp_dists <- predict(object = c53_traj,
                        new_times = my_times)
dim(interp_dists)
#> [1] 55  3
head(interp_dists)
#>   event_timestamp trip_id_performed    interp
#> 1      1771260000          13437100 2738.0345
#> 2      1771260000          35294100  113.3561
#> 3      1771260180          13437100 3131.5815
#> 4      1771260180          35294100  895.2449
#> 5      1771260360          13437100 4254.6383
#> 6      1771260360          35294100 1594.6588

# Run function: get speeds from times
interp_speeds <- predict(object = c53_traj,
                         new_times = my_times,
                         deriv = 1)
dim(interp_speeds)
#> [1] 55  3
head(interp_speeds)
#>   event_timestamp trip_id_performed     interp
#> 1      1771260000          13437100 13.2568830
#> 2      1771260000          35294100  3.0495604
#> 3      1771260180          13437100  0.1946919
#> 4      1771260180          35294100  0.7279382
#> 5      1771260360          13437100  2.8749519
#> 6      1771260360          35294100  1.9051702

# Run function: get times from distances
interp_times <- predict(object = c53_traj,
                        new_distances = my_distances)
dim(interp_times)
#> [1] 46  3
head(interp_times)
#>   distance trip_id_performed     interp
#> 1        0          13437100 1771258111
#> 2     1000           1306100 1771262889
#> 3     1000          13437100 1771259592
#> 4     1000          35294100 1771260233
#> 5     2000           1306100 1771263094
#> 6     2000          13437100 1771259831

# Run function: get time & distance pairs given distance bounds
interp_time_dist_pairs <- predict(object = c53_traj,
                                  distance_lims = my_distance_lims,
                                  timestep = my_timestep)
dim(interp_time_dist_pairs)
#> [1] 6 3
head(interp_time_dist_pairs)
#> # A tibble: 6 × 3
#>   trip_id_performed event_timestamp interp
#>   <chr>                       <dbl>  <dbl>
#> 1 1306100               1771262779.   500.
#> 2 1306100               1771262789.   532.
#> 3 1306100               1771262799.   558.
#> 4 13437100              1771259465.   500.
#> 5 35294100              1771260085.   500.
#> 6 35294100              1771260095.   578.
```
