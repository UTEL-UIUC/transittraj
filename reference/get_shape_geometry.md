# Get the geometry of a route shape.

This function returns an SF multilinestring of the route alignments from
GTFS shapes. Similar to tidytransit's `get_geometry()`, but allows
filtering by `shape_id` and projection to a new coordinate system. See
`Details` for requirements on the input GTFS.

## Usage

``` r
get_shape_geometry(gtfs, shape = NULL, project_crs = 4326)
```

## Arguments

- gtfs:

  A tidygtfs object.

- shape:

  Optional. The GTFS shape_id to use. Can be a single value, or a
  vector. Default is NULL, where all `shape_id`s in `gtfs` will be used.

- project_crs:

  Optional. A numeric EPSG identifer indicating the coordinate system to
  use for spatial calculations. Consider setting to a Euclidian
  projection, such as the appropriate UTM zone. Default is 4326 (WGS 84
  ellipsoid).

## Value

An SF multilinestring, with one multilinestring object per `shape_id`.

## Details

A `shapes` file must be present in your GTFS object. This file must
contain at least the following fields:

- `shape_id`

- `shape_pt_lat`

- `shape_pt_lon`

- `shape_pt_sequence`

## Examples

``` r
# Set my parameters
my_shape <- "C53:04"
my_crs = 32618

# Get shape from WMATA GTFS
c53_shape <- get_shape_geometry(gtfs = wmata_gtfs,
                                shape = my_shape,
                                project_crs = my_crs)
print(c53_shape)
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
