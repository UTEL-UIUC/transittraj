# Validates input to trajectory plotting functions.

This function validates that an appropriate combination of trajectory
and distance_df are provided, and that they have the necessary features.
If a trajectory's inverse function is not present, the user will be
warned that interpolation may be time consuming. Internal function.

## Usage

``` r
plot_traj_input_validation(trajectory, distance_df, has_inv)
```

## Arguments

- trajectory:

  A trajectory objcet

- distance_df:

  a DF with columns distance, event_timestamp, and trip_id_performed

- has_inv:

  a boolean, does the traj object have inv fun?
