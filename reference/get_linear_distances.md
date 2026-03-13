# Linearizes latitude-longitude GPS points to a provided route shape.

This functions projects raw AVL data, as GPS latitude-longitude points,
onto a provided route geometry, returning the distance of that point
along the shape from the beginning terminal.

## Usage

``` r
get_linear_distances(
  avl_df,
  shape_geometry,
  clip_buffer = NULL,
  original_crs = 4326,
  project_crs = 4326
)
```

## Arguments

- avl_df:

  A dataframe of raw AVL data. Must include at least `longitude` and
  `latitude` columns. See
  [`validate_tides()`](https://obrien-ben.github.io/transittraj/reference/validate_tides.md).

- shape_geometry:

  The SF object to project onto. Must be only one shape. See
  [`get_shape_geometry()`](https://obrien-ben.github.io/transittraj/reference/get_shape_geometry.md).

- clip_buffer:

  Optional. The distance, in units of the used spatial projection, to
  clip the GPS points. Only points within this distance of the
  `shape_geometry` will be kept. Default is NULL, where no clip will be
  applied.

- original_crs:

  Optional. A numeric EPSG identifier. If a dataframe is provided for
  `points`, this will be used to define the coordinate system of the
  longitude / latitude values. Default is 4326 (WGS 84 ellipsoid).

- project_crs:

  Optional. A numeric EPSG identifer indicating the coordinate system to
  use for spatial calculations. Consider setting to a Euclidian
  projection, such as the appropriate UTM zone. Default is 4326 (WGS 84
  ellipsoid).

## Value

The input `avl_df` with `latitude` and `longitude` columns replaced by a
`distance` column, in the units of the spatial projection used (e.g.,
meters if using UTM).

## Examples

``` r
# Set my parameters
my_buffer <- 50 # meters
my_crs <- 32618

# Get input data
c53_avl <- new_transittraj_data("c53_avl")
c53_shape <- new_transittraj_data("get_shape_geometry")

# Run function
c53_dists <- get_linear_distances(avl_df = c53_avl,
                                  shape_geometry = c53_shape,
                                  clip_buffer = my_buffer,
                                  project_crs = my_crs)
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
