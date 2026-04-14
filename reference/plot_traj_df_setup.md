# Sets up plotting DF is trajectory is provided.

This function uses a trajectory object to create a DF of time, distance
points for each trip. If inverse function is present, this will be used
to interpolate only over the appropriate distance range of each trip. If
not, interpolation will occur over the entire time range of the
requested trips. Internal function.

## Usage

``` r
plot_traj_df_setup(trajectory, has_inv, plot_trips, timestep, distance_lims)
```

## Arguments

- trajectory:

  A trajectory object

- has_inv:

  a boolean, does the traj object have inv fun?

- timestep:

  A numeric, time interval between interpolated poitns

- distance_lims:

  A vector of (minimum, maximum) distance to interpolate
