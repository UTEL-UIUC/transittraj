# Returns an object from a particular step of `transittraj`'s workflow.

This function runs `transittraj`'s AVL cleaning and trajectory
reconstruction workflow up until a certain point (as defined by
`func_output`), then returns the object at that point. A subset of the
`wmata_avl` dataset is used.

## Usage

``` r
new_transittraj_data(func_output = NULL)
```

## Arguments

- func_output:

  The `transittraj` function to return an output for. Should be a string
  corresponding to the function name. Default is `NULL`, where a vector
  of allowed inputs will be returned.

## Value

The object returned by the specified function.

## Details

This is primarily intended for use in testing and examples. The workflow
applied here is the same as what is in `vignette("data-workflow")`.

## Examples

``` r
# Get AVL data after projection onto route
c53_dists <- new_transittraj_data("get_linear_distances")
head(c53_dists)
#>   location_ping_id vehicle_id trip_id_performed service_date route_id
#> 1               25       5539          13300100   2026-02-16      C53
#> 2               52       5539          13300100   2026-02-16      C53
#> 3              106       5539          13300100   2026-02-16      C53
#> 4              187       5539          13300100   2026-02-16      C53
#> 5              268       5539          13300100   2026-02-16      C53
#> 6              349       5539          13300100   2026-02-16      C53
#>   direction_id  speed trip_stop_sequence     event_timestamp stop_id distance
#> 1            0 7.9248                 19 2026-02-16 10:58:25    3679 4159.641
#> 2            0 6.4008                 20 2026-02-16 10:58:36   17578 4249.421
#> 3            0 3.9624                 20 2026-02-16 10:59:06   17578 4292.869
#> 4            0 3.9624                 20 2026-02-16 10:59:31   17578 4318.141
#> 5            0 1.2192                 22 2026-02-16 11:00:01   17417 4495.498
#> 6            0 2.4384                 22 2026-02-16 11:00:31   17417 4507.870
```
