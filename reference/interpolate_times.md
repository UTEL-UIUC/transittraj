# Internal generic for performing interpolation of times from distances.

Performs interpolation of time values from a DF of distances & trip IDs.
A generic function, dispatches depending on whether trajectory is
grouped or single.

## Usage

``` r
interpolate_times(trajectory, new_dist_trips, ...)

# S3 method for class 'avltrajectory_single'
interpolate_times(trajectory, new_dist_trips, ...)

# S3 method for class 'avltrajectory_group'
interpolate_times(trajectory, new_dist_trips, ...)
```

## Arguments

- trajectory:

  Single or grouped trajectory object

- new_dist_trips:

  A DF with trip_id_performed and distance

- ...:

  other inputs, not used

## Value

A DF with appended column "interp" of event_timestamp values

## Examples

``` r
nd <- data.frame(trip_id_performed = c("63383915"),
                 distance = 24448.63)
lineE_traj <- new_transittraj_data("get_trajectory_fun")

interp <- transittraj:::interpolate_times(trajectory = lineE_traj,
                             new_dist_trips = nd)
head(interp)
#>   trip_id_performed distance     interp
#> 1          63383915 24448.63 1779890000
```
