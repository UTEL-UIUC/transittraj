# Get the distances of stops along routes.

This function returns the linear distance of each stop along a route
shape, starting from the route's beginning terminal. Unless a
`shape_geometry` is provided, stops will be project onto all `shape_id`s
that serve them. If a `shape_geometry` is provided, the function will
look only for stops served by that shape.

## Usage

``` r
get_stop_distances(gtfs, shape_geometry = NULL, project_crs = 4326)
```

## Arguments

- gtfs:

  A tidygtfs object.

- shape_geometry:

  Optional. The SF object to project onto. Must include the field
  `shape_id`. See
  [`get_shape_geometry()`](https://obrien-ben.github.io/transittraj/reference/get_shape_geometry.md).
  Default is `NULL`, where all shapes in `gtfs` will be used.

- project_crs:

  Optional. A numeric EPSG identifer indicating the coordinate system to
  use for spatial calculations. Consider setting to a Euclidian
  projection, such as the appropriate UTM zone. Default is 4326 (WGS 84
  ellipsoid).

## Value

A dataframe containing `stop_id`, the `shape_id` it was projected onto,
and `distance`, in units of the spatial projection (e.g., meters if
using UTM).

## Examples

``` r
# Set my parameters
my_shape <- "804EB_RC_221121"
my_crs <- 32611
my_route <- "804"
my_dir <- 0

# Get needed GTFS data
lineE_gtfs <- filter_by_route(gtfs = lacmta_gtfs, route_ids = my_route,
                              dir_id = 0)
lineE_shape <- get_shape_geometry(gtfs = lacmta_gtfs, shape = my_shape,
                                  project_crs = my_crs)

# Run stop distances function
lineE_stop_dists <- get_stop_distances(gtfs = lineE_gtfs,
                                       shape_geometry = lineE_shape,
                                       project_crs = my_crs)
head(lineE_stop_dists)
#> # A tibble: 6 × 10
#>   stop_id stop_code stop_name    stop_desc stop_url location_type parent_station
#>   <chr>   <chr>     <chr>        <chr>     <chr>            <int> <chr>         
#> 1 80121   80121     Pico Station NA        NA                   0 80121S        
#> 2 80122   80122     7th Street … NA        NA                   0 80122S        
#> 3 80123   80123     LATTC / Ort… NA        NA                   0 80123S        
#> 4 80124   80124     Jefferson /… NA        NA                   0 80124S        
#> 5 80125   80125     Expo Park /… NA        NA                   0 80125S        
#> 6 80126   80126     Expo / Verm… NA        NA                   0 80126S        
#> # ℹ 3 more variables: tpis_name <chr>, shape_id <chr>, distance <dbl>
```
