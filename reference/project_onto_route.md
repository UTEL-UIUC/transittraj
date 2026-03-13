# Projects points to linear distances along a route shape.

This function takes spatial points and projects them onto a route,
returning the linear distance from the beginning terminal of the route.

## Usage

``` r
project_onto_route(
  shape_geometry,
  points,
  original_crs = 4326,
  project_crs = 4326
)
```

## Arguments

- shape_geometry:

  The SF object to project onto. Must include the field `shape_id`. See
  [`get_shape_geometry()`](https://obrien-ben.github.io/transittraj/reference/get_shape_geometry.md).

- points:

  Can be either: a dataframe representing point coordinates, with fields
  `longitude` and `latitude`; or, an SF or SFC point object.

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

The `points` input (either dataframe or SF) with an appended column for
the linear distance along the route. If `points` is an SFC, a vector of
numeric distances is returned. Units are those of the spatial projection
used (e.g., meters if using UTM).

## Examples

``` r
# Set my parameters
my_crs <- 32618

# Get shape data
c53_shape <- new_transittraj_data("get_shape_geometry")

# Set points of interest
my_points <- data.frame(longitude = c(-76.990038, -77.036289),
                        latitude = c(38.871335, 38.917054),
                        poi_name = c("11th St Bridge", "16th & U"))

# Run project_onto_route
project_onto_route(shape_geometry = c53_shape,
                   points = my_points,
                   project_crs = my_crs)
#>   longitude latitude       poi_name  distance
#> 1 -76.99004 38.87134 11th St Bridge  5422.615
#> 2 -77.03629 38.91705       16th & U 13402.281
```
