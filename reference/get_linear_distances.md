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
dim(c53_avl)
#> [1] 709  12

# Run function
c53_dists <- get_linear_distances(avl_df = c53_avl,
                                  shape_geometry = c53_shape,
                                  clip_buffer = my_buffer,
                                  project_crs = my_crs)
dim(c53_dists)
#> [1] 639  11
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
