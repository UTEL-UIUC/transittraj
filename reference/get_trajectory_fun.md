# Fit continuous trajectory interpolating curves from AVL data

This function fits a continuous vehicle trajectory to cleaned AVL points
(see
[`vignette("articles/data-workflow-la")`](https://obrien-ben.github.io/transittraj/articles/data-workflow-la.md)).
This function operates as a "function factory", returning a function
(closure) which takes a timestamp and returns each trip's position. A
separate curve is fit for each trip, and stored in a special trajectory
object class. The default interpolating method is a velocity-informed
piecewise cubic interpolating polynomial, but linear interpolation and
other spline-based techniques are also supported. See `Details` for a
discussion.

## Usage

``` r
get_trajectory_fun(
  distance_df,
  interp_method = "monoH.FC",
  use_speeds = TRUE,
  find_inverse_function = TRUE,
  inv_tol = 0.01,
  return_group_function = TRUE
)
```

## Arguments

- distance_df:

  A dataframe of linearized AVL data. Must include `trip_id_performed`,
  `event_timestamp`, and `distance`. If `use_speed = TRUE`, must also
  include `speed`.

- interp_method:

  Optional. The type of interpolation function to be fit. Either
  `"linear"`, or a spline method from
  [`stats::splinefun()`](https://rdrr.io/r/stats/splinefun.html).
  Default is `"monoH.FC"`.

- use_speeds:

  Optional. A boolean, should curves be constrained by observed AVL
  speeds? Should only be used with `interp_method = "monoH.FC"`, but
  `monoH.FC` does not require speeds. Default is `TRUE`.

- find_inverse_function:

  Optional. A boolean, should the numeric inverse function (time ~
  distance) be calculated? Default is `TRUE`.

- inv_tol:

  Optional. A numeric in the units of input `distance`, the tolerance
  used when calculating the numeric inverse function. Default is `0.01`.

- return_group_function:

  Optional. A boolean, should the returned trajectory object be grouped
  into a single function? If FALSE, will return a list (indexed by
  `trip_id_performed`) of single trajectory objects. Default is `TRUE`.

## Value

If `return_group_function = TRUE`, a grouped trajectory object. If
`FALSE`, a list of single trajectory objects, indexed by their
`trip_id_performed`.

## Details

### Interpolating Methods

The goal of this function is to fit a continuous function representing a
vehicle's distance traveled as a function of time, for each trip. This
function supports to types of interpolating curves:

- Linear interpolation, for `interp_method = "linear"`. This will fit a
  simple linear function, ignorant to recorded `speed` values.

- Spline interpolation, for `interp_method` set to any method supported
  by [`stats::splinefun()`](https://rdrr.io/r/stats/splinefun.html)
  (i.e., `"fmm"`, `"natural"`, `"periodic"`, `"monoH.FC"`, or
  `"hyman"`.) Only `interp_method = "monoH.FC"` supports use of recorded
  `speed` values.

By default, `interp_method = "monoH.FC"` and `use_speeds = TRUE`. This
will yield a continuous, differentiable, monotonic, and invertible
trajectory, and is the standard transit trajectory interpolation
technique. If the input `distance` and `speed` values satisfy
Fritsch-Carlson, the interpolating function is guaranteed to be
montonic. See
[`make_monotonic()`](https://obrien-ben.github.io/transittraj/reference/make_monotonic.md)
and
[`validate_monotonicity()`](https://obrien-ben.github.io/transittraj/reference/validate_monotonicity.md).

Note that `use_speeds = TRUE` requires `interp_method = "monoH.FC"`, but
`interp_method = "monoH.FC"` does not require `use_speeds = TRUE`. In
the latter scenario, a "velocity-ignorant' Fritsch-Carlson interpolating
function can be created. If input `distance` values are monotonic, this
curve is guaranteed to be monotonic.

### Inverse Functions

Often times, we are concerned not with the position of a vehicle at a
particular time, but the time at which a vehicle crosses a specific
point in space. This can be accomplished by computing an inverse
trajectory function. If `find_inverse_function = TRUE` (the default), a
numeric inverse to the fit trajectory function will be found, with a
tolerance controlled by `inv_tol`.

Because the inverse function is numerical, it can be found for any type
of interpolating curve (linear or spline). However, the input data must
be strictly monotonic for the trajectory curve to be invertible. If
`find_inverse_function = TRUE`, this will be verified before proceeding
(see
[`validate_monotonicity()`](https://obrien-ben.github.io/transittraj/reference/validate_monotonicity.md)).

### The Trajectory Object

A trajectory function does not exist by itself; rather, it requires the
context about the trip it describes, as well as its inverse function. As
such, `get_trajectory_fun()` returns an AVL trajectory object. If
`return_group_function = TRUE` (the default), the function will return a
single object containing:

- A vector of `trip_id_performed`s present in `distance_df`.

- A list of fit trajectory functions (closures), one per trip, indexed
  by their `trip_id_performed`.

- A list of fit inverse trajectory functions (closures), one per trip,
  indexed by their `trip_id_performed`.

- Information about how the trajectory and inverse trajectory functions
  were fit, including `interp_method`, `use_speeds`, and `inv_tol`.

- A vector each for the minimum distances, maximum distances, minimum
  times, and maximum times of each trip. These inform the domain and
  range of the trajectory function and its inverse, preventing
  extrapolation beyond the time or distance range actually served by a
  trip.

- The agency's timezone (see
  [`OlsonNames()`](https://rdrr.io/r/base/timezones.html)), as extracted
  from the `event_timestamp` column.

Alternatively, if `return_group_function = FALSE`, a separate trajectory
object will be returned for each trip, as a list of objects indexed by
their `trip_id_performed`.

More information about the trajectory object classes and how to use them
is available at
[`vignette("articles/intro-trajectories-la")`](https://obrien-ben.github.io/transittraj/articles/intro-trajectories-la.md).

## Examples

``` r
# Get input data
lineE_mono <- new_transittraj_data("make_monotonic")

# Run function: grouped trajectory object
lineE_traj_grouped <- get_trajectory_fun(distance_df = lineE_mono)
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

# Run function: list of single trajectory objects
lineE_traj_singles <- get_trajectory_fun(distance_df = lineE_mono,
                                         return_group_function = FALSE)
length(lineE_traj_singles)
#> [1] 11
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
