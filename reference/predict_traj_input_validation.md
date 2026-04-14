# Internal function to validate inputs to trajectory prediction methods.

Checks that the proper combination of inputs is provided. Should be one
of: new_times; new_distances; distance_lims AND timestep. If latter or
new_distances, trajectory must also have inverse function. Derivative is
also checked against maximum allowed.

## Usage

``` r
predict_traj_input_validation(
  new_times,
  new_distances,
  distance_lims,
  timestep,
  has_inv,
  deriv,
  max_deriv
)
```

## Arguments

- new_times:

  A DF or vector of new time values, or `NULL`

- new_distances:

  A DF or vector of new distance values, or `NULL`

- distance_lims:

  A vector of min, max distance, or `NULL`

- timestep:

  An integer for interpolation timestep, or `NULL`

- has_inv:

  Boolean, does traj have inv fun?

- deriv:

  User-requested derivative

- max_deriv:

  Maximum derivative supported by traj fun

## Value

Throws error only if not all OK
