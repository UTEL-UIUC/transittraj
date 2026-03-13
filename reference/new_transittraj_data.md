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
#> 1             1586       5516          13437100   2026-02-16      C53
#> 2             1667       5516          13437100   2026-02-16      C53
#> 3             1694       5516          13437100   2026-02-16      C53
#> 4             1775       5516          13437100   2026-02-16      C53
#> 5             2018       5516          13437100   2026-02-16      C53
#> 6             2261       5516          13437100   2026-02-16      C53
#>   direction_id  speed trip_stop_sequence     event_timestamp stop_id distance
#> 1            0 6.4008                  2 2026-02-16 11:08:31   13111  0.00000
#> 2            0 0.0000                  2 2026-02-16 11:09:01   13111  2.08491
#> 3            0 0.0000                  2 2026-02-16 11:09:11   13111  2.08491
#> 4            0 0.0000                  2 2026-02-16 11:09:41   13111  2.08491
#> 5            0 0.0000                  2 2026-02-16 11:11:12   13111  2.08491
#> 6            0 0.0000                  2 2026-02-16 11:12:43   13111  2.08491
```
