# Internal generic for performing interpolation of distances from times.

Performs interpolation of distance values from a DF of times & trip IDs.
A generic function, dispatches depending on whether trajectory is
grouped or single.

## Usage

``` r
interpolate_distances(trajectory, new_times_trips, ...)

# S3 method for class 'avltrajectory_single'
interpolate_distances(trajectory, new_times_trips, ...)

# S3 method for class 'avltrajectory_group'
interpolate_distances(trajectory, new_times_trips, ...)
```

## Arguments

- trajectory:

  Single or grouped trajectory object

- new_times_trips:

  DF with trip_id_performed event_timestamp, and deriv

- ...:

  other inputs, not used

## Value

A DF with appended column "interp" of distance (or deriv) values

## Examples

``` r
nt <- data.frame(trip_id_performed = c("63383915"),
                 event_timestamp = 1779890000,
                 deriv = c(0))
lineE_traj <- new_transittraj_data("get_trajectory_fun")

interp <- transittraj:::interpolate_distances(trajectory = lineE_traj,
                                 new_times_trips = nt)
head(interp)
#>   trip_id_performed event_timestamp deriv   interp
#> 1          63383915      1779890000     0 24448.63
```
