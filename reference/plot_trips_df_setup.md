# Set up dataframe & validate of point objects for vehicle animations

Intended for internal use only.

## Usage

``` r
plot_trips_df_setup(
  trajectory,
  distance_df,
  plot_trips,
  timestep,
  distance_lims,
  center_vehicles,
  convert_to_timezone
)
```

## Arguments

- trajectory:

  Single or grouped trajectory object.

- distance_df:

  AVL distance DF.

- plot_trips:

  Vector of trip_id_performed to plot.

- timestep:

  Time in seconds for interpolation.

- distance_lims:

  Vector of (minimum, maximum) distance to plot.

- center_vehicles:

  Should vehicles be centered

- convert_to_timezone:

  Should times be converted to timezones

## Value

plotting dataframe (trips_df)
