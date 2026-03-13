# Internal function for `plot_df_setup()` to interpolate.

Uses a trajectory object to interpolate over of the time range by-trip
provided through `trip_time_extremes`. For internal use only.

## Usage

``` r
interp_df_setup(trajectory, trip_time_extremes, timestep)
```

## Arguments

- trajectory:

  A trajectory object.

- trip_time_extremes:

  For grouped, a DF with trip_id_performed, min_time, and max_time; for
  single, a vector with min, max time

- timestep:

  Numeric of timestep for interpolation

## Value

A dataframe of trips_df for plotting functions
