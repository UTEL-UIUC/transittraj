# Internal function to set up dataframe for interpolating timesteps between distance limits.

Internal function to set up dataframe for interpolating timesteps
between distance limits.

## Usage

``` r
predict_traj_setup_dist_lims(
  trajectory,
  trip_extremes,
  distance_lims,
  timestep
)
```

## Arguments

- trajectory:

  trajectory object

- trip_extremes:

  DF of trip time & distance extremes

- distance_lims:

  a vector of (min, max) distance

- timestep:

  time interval for interpolation

## Value

DF of trip IDs & times to interpolate at
