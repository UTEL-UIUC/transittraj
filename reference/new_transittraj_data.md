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
new_transittraj_data("get_linear_distances")
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 322061.8 ymin: 4301418 xmax: 329233.3 ymax: 4310353
#> Projected CRS: WGS 84 / UTM zone 18N
#> # A tibble: 1 × 2
#>   shape_id                                                              geometry
#>   <chr>                                                    <MULTILINESTRING [m]>
#> 1 C53:04   ((327507.1 4301484, 327505.6 4301478, 327504.9 4301473, 327504 43014…
```
