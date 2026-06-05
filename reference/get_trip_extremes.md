# Get the distance and time range of each trip in a trajectory object.

This function extracts the time and distance ranges stored in a
trajectory object and formats them into a dataframe for each use. The
dataframe can be filtered to a desired set of `trip_id_performed`s.

## Usage

``` r
get_trip_extremes(trajectory, filter_trips = NULL)
```

## Arguments

- trajectory:

  A trajectory object.

- filter_trips:

  Optional. A vector of `trip_id_performed`s to filter the dataframe to.
  At least one must of `filter_trips` must be present in `trajectory`.
  Default is `NULL`, where all `trip_id_performed`s in `trajectory` are
  returned.

## Value

A dataframe with the columns `trip_id_performed`, `min_time`,
`max_time`, `min_dist`, and `max_dist`.

## Examples

``` r
# Get input data
lineE_traj <- new_transittraj_data("get_trajectory_fun")

# Run function
lineE_extremes <- get_trip_extremes(lineE_traj)
print(lineE_extremes)
#>    trip_id_performed     min_dist max_dist   min_time   max_time
#> 1           63383915 3.167278e+01 35292.87 1779886240 1779891480
#> 2           63383917 7.863145e+00 35227.39 1779887281 1779892057
#> 3           63383949 5.010461e+00 35192.59 1779889773 1779894642
#> 4           63383991 7.197114e+00 34686.08 1779887038 1779891459
#> 5           63384002 5.770401e+00 35151.47 1779887757 1779892976
#> 6           63384022 5.484959e+00 27125.84 1779889141 1779892840
#> 7           63384063 6.366599e-01 34684.70 1779892098 1779896499
#> 8           63384093 2.690944e+04 35234.77 1779886677 1779887777
#> 9           63384094 1.831250e+01 35207.69 1779893803 1779898116
#> 10          63384135 3.869933e+00 35243.03 1779888197 1779892675
#> 11          63384143 3.001684e+01 35255.01 1779892496 1779897459
```
