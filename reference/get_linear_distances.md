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
my_crs <- 32611

# Get input data
lineE_avl <- new_transittraj_data("lineE_avl")
lineE_shape <- new_transittraj_data("get_shape_geometry")
dim(lineE_avl)
#> [1] 3318   11

# Run function
lineE_dists <- get_linear_distances(avl_df = lineE_avl,
                                    shape_geometry = lineE_shape,
                                    clip_buffer = my_buffer,
                                    project_crs = my_crs)
dim(lineE_dists)
#> [1] 3268   10
head(lineE_dists)
#>                   location_ping_id service_date trip_id_performed    speed
#> 1 4af122e0b668d6821335d641a89ad312   2026-05-27          63383915 1.743456
#> 2 ef3b602e52fe3556a7539491e7792c74   2026-05-27          63383915 3.308096
#> 3 a940808be7f3a59066c981bffe3e537a   2026-05-27          63383915 2.145792
#> 4 6df05dfca51b44f25d403356de5a3e0a   2026-05-27          63383915 0.000000
#> 5 5326947f997dad696a09f510d4857d2c   2026-05-27          63383915 0.000000
#> 6 0eeafa189aab82fe0bff169a9dc587f7   2026-05-27          63383915 0.000000
#>       vehicle_id     event_timestamp direction_id        shape_id route_id
#> 1 1047-1048-1185 2026-05-27 05:48:58            0 804EB_RC_221121      804
#> 2 1047-1048-1185 2026-05-27 05:49:19            0 804EB_RC_221121      804
#> 3 1047-1048-1185 2026-05-27 05:49:40            0 804EB_RC_221121      804
#> 4 1047-1048-1185 2026-05-27 05:49:59            0 804EB_RC_221121      804
#> 5 1047-1048-1185 2026-05-27 05:50:20            0 804EB_RC_221121      804
#> 6 1047-1048-1185 2026-05-27 05:50:40            0 804EB_RC_221121      804
#>    distance
#> 1 197.58271
#> 2  99.22546
#> 3  98.72317
#> 4  62.66861
#> 5  83.11011
#> 6  31.67278
```
