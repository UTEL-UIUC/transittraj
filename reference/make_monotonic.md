# Correct distance observations, and optionally speeds, to be weakly or strictly monotonic

Due to error in GPS position and speed measurements, raw AVL data is
often not monotonic, creating difficulties for advanced analyses. This
function presents a variety of options to correct data, resulting in
distance values, and optionally speeds, which form a strictly or weakly
monotonic curve. See `Details` for more information.

## Usage

``` r
make_monotonic(
  distance_df,
  correct_speed = FALSE,
  add_distance_error = 0,
  direction = "bidirectional",
  return_changes = FALSE
)
```

## Arguments

- distance_df:

  A dataframe of linearized AVL data. Must include `trip_id_performed`,
  `event_timestamp`, and `distance`. If `correct_speed = TRUE`, must
  also include `speed`.

- correct_speed:

  Optional. A boolean, should speeds be corrected to meet adjusted
  distances and Fritsch-Carlson conditions? Default is `FALSE`.

- add_distance_error:

  Optional. If non-zero, each "flat" observation will be adjusted by
  this amount forwards, in units of input `distance`. Default is `0`.

- direction:

  Optional. The step direction for monotonic enforcement. One of
  `"forward"`, `"backward"`, or `"bidirectional"`. Default is
  `"bidirectional"`.

- return_changes:

  Optional. Should a dataframe of each observation changed be returned?
  Default is `FALSE`.

## Value

The input `distance_df` with distances and speeds adjusted. If
`return_changes = TRUE`, a dataframe with observations changed.

## Details

There are two primary types of monotonicity:

- Weak, allowing the function to be increasing or constant

- Strict, forcing the function to always increase and never stand still.

While we know that, in practice, transit vehicles rarely move backwards
(i.e., reverse) along their route, random noise in GPS observations will
introduce non-monotonicities (i.e., drifting backwards). Weak
monotonicity most accurately describes real transit vehicle
trajectories: we expect the vehicle to either move forwards, or stand
still at a stop. However, strict monotonicity is a convenient
mathematical property that allows us to find the inverse trajectory
(i.e., retrieve time as a function of distance).

There are three ways in which monotonicity can be enforced upon a
function's position:

- Forward-stepping: Beginning at the beginning of a trip, iterate
  forwards in time through distance observations. If the distance ever
  drifts backward, it will be "pulled up" to the cumulative maximum
  distance observed by that point.

- Backward-stepping: Beginning at the end of a trip, iterate backwards
  in time through distance observations. If the distance ever drifts
  forward, it will be "pushed down" to the cumulative minimum distance
  observed by that point.

- Bidirectional: Because noise in the distance observations occurs
  randomly in both directions, during stationary periods,
  forward-stepping will bias the trajectory positively (i.e., the
  trajectory is ahead of the actual vehicle position), and
  backward-stepping will bias the trajectory negatively (i.e., the
  trajectory is behind the actual vehicle position). Additionally,
  forward-stepping can disrupt the acceleration cycle at the end of a
  stationary period, and backward-stepping can disrupt the deceleration
  cycle at the beginning of a stationary period. The bidirectional
  approach remedies this by performing both directions of correction on
  the original data, then averaging the two outcomes.

Choose between the two monotonicity types by setting
`add_distance_error`. If `add_distance_error = 0` (the default), a
weakly monotonic trajectory is returned; otherwise, the trajectory will
be strictly monotonic, with a perturbation of `add_distance_error` added
(for forward-stepping) or subtracted (for backward-stepping) in flat
regions. Choose between the two step directions using `direction`.

In addition to distance corrections, some applications (e.g., fitting a
velocity-informed interpolation spline) require speeds to satisfy
certain monotonic conditions. If `correct_speed = TRUE`, the following
corrections will be made:

- For strict monotonicity (`add_distance_error > 0`), speeds must be
  non-zero. At each point where the recorded `speed == 0`, the speed
  will be replaced by `add_distance_error` divided by the time between
  that point and the previous point.

- For both strict and weak monotonicity, speeds will be adjusted to meet
  the Fritsch-Carlson (1980) constraints. Often, only a handful of input
  `speed` values will be adjusted.

If recorded speed values are not present, set `correct_speed = FALSE`.
However, if you are interested in later fitting a velocity-informed
interpolating curve, such as Fritsch-Carlson's piecewise cubic
polynomials, consider setting `correct_speed = TRUE` to guarantee a
monotonic interpolating curve.

After using this function to perform corrections, use
[`validate_monotonicity()`](https://obrien-ben.github.io/transittraj/reference/validate_monotonicity.md)
to check if weak, strict, and Fritsch-Carlson speed conditions are met.

## References

Fritsch, F. N., and R. E. Carlson. 1980. “Monotone Piecewise Cubic
Interpolation.” SIAM Journal on Numerical Analysis.
https://doi.org/10.1137/0717021.

Robbennolt, Jake, Sirajum Munira, and Stephen D. Boyles. 2026. “A
Comparative Study of Spline-Based Trajectory Reconstruction Methods
Across Varying Automatic Vehicle Location Data Densities.” Paper
presented at 2026 Transportation Research Board Annual Meeting, January
11. http://arxiv.org/abs/2509.00119.

## Examples

``` r
# Set my parameters
my_dist_err = 0.001

# Get input data
lineE_trimmed <- new_transittraj_data("trim_trips")

# Run function
lineE_mono <- make_monotonic(distance_df = lineE_trimmed,
                             add_distance_error = my_dist_err,
                             correct_speed = TRUE)
head(lineE_mono)
#> # A tibble: 6 × 10
#>   location_ping_id service_date trip_id_performed vehicle_id event_timestamp    
#>   <chr>            <chr>        <chr>             <chr>      <dttm>             
#> 1 0eeafa189aab82f… 2026-05-27   63383915          1047-1048… 2026-05-27 05:50:40
#> 2 3436b03ae5feecd… 2026-05-27   63383915          1047-1048… 2026-05-27 05:50:59
#> 3 02939a6bc750b44… 2026-05-27   63383915          1047-1048… 2026-05-27 05:51:16
#> 4 1a479dd51a750a1… 2026-05-27   63383915          1047-1048… 2026-05-27 05:51:41
#> 5 55ecfed9996432e… 2026-05-27   63383915          1047-1048… 2026-05-27 05:51:56
#> 6 822d16b82b32c42… 2026-05-27   63383915          1047-1048… 2026-05-27 05:52:17
#> # ℹ 5 more variables: direction_id <int>, shape_id <chr>, route_id <chr>,
#> #   distance <dbl>, speed <dbl>
```
