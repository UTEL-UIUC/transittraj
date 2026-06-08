# Internal generic for performing interpolation of distances from times.

Performs interpolation of distance values from a DF of times & trip IDs.
A generic function, dispatches depending on whether trajectory is
grouped or single.

## Usage

``` r
interpolate_distances(trajectory, new_times_trips)

# S3 method for class 'avltrajectory_single'
interpolate_distances(trajectory, new_times_trips)

# S3 method for class 'avltrajectory_group'
interpolate_distances(trajectory, new_times_trips)
```

## Arguments

- trajectory:

  Single or grouped trajectory object

- new_times_trips:

  DF with trip_id_performed and event_timestamp

## Value

A DF with appended column "interp" of distance (or deriv) values
