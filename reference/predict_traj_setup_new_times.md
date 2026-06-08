# Internal function to set up dataframe for interpolating distances from times

Internal function to set up dataframe for interpolating distances from
times

## Usage

``` r
predict_traj_setup_new_times(new_times, trip_extremes, deriv)
```

## Arguments

- new_times:

  new event_timestamps to interpolate at

- trip_extremes:

  DF of trip time & distance extremes

- deriv:

  vector of numeric derivs to interpolate at

## Value

DF of trip IDs & times to interpolate at
