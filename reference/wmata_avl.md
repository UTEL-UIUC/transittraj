# WMATA Bus Automatic Vehicle Location Data

This dataset is an archive of WMATA's public GTFS-realtime feed,
accessed from their developer API. It has been reformatted from GTFS-rt
to match the TIDES standard for both fields and datatypes. This dataset
is intended to be used alongside the static GTFS feed provided in
`wmata_gtfs`.

## Usage

``` r
wmata_avl
```

## Format

### `wmata_avl`

A dataframe with 20,777 rows and 12 columns.

- location_ping_id:

  A unique ID for each row

- vehicle_id:

  An ID corresponding to each vehicle

- trip_id_performed:

  Trip IDs, matching those in GTFS

- service_date:

  The data of the trip's beginning

- route_id:

  Route IDs, matching those in GTFS

- direction_id:

  Direction IDs, matching those in GTFS

- latitude, longitude:

  The GPS ping longitude and latitude

- speed:

  The recorded speed, in meters per second

- trip_stop_sequence:

  The stop number the vehicle is approaching

- event_timestamp:

  POSIXct time objects

- stop_id:

  Stop IDs the vehicles are approaching, matching those in GTFS

## Source

<https://developer.wmata.com/>

## Details

The dataset contains three bus routes, with two directions for each:

- D40: Georgia Ave

- C53: U St/Congress Heights

- D96: Mass Ave to Bethesda

## Examples

``` r
# Print the header
head(wmata_avl)
#>   location_ping_id vehicle_id trip_id_performed service_date route_id
#> 1                0       4582          30095100   2026-02-16      D96
#> 2                1       5461          18632100   2026-02-16      C53
#> 3                2       5463            698100   2026-02-16      C53
#> 4                3       5464          14078100   2026-02-16      C53
#> 5                4       5466          25836100   2026-02-16      C53
#> 6                5       5470           8560100   2026-02-16      C53
#>   direction_id latitude longitude   speed trip_stop_sequence
#> 1            1 38.93342 -77.07974  8.8392                 36
#> 2            0 38.92353 -77.05198  0.0000                 63
#> 3            1 38.91215 -77.01222  7.9248                 19
#> 4            0 38.91594 -77.02111 10.9728                 52
#> 5            0 38.84528 -76.98762  0.0000                  2
#> 6            1 38.91702 -77.03714  0.0000                  9
#>       event_timestamp stop_id
#> 1 2026-02-16 10:58:35    7752
#> 2 2026-02-16 10:58:09    7219
#> 3 2026-02-16 10:58:23   17574
#> 4 2026-02-16 10:58:31    6843
#> 5 2026-02-16 10:58:27   13111
#> 6 2026-02-16 10:58:14    6879

# Filter the data
c53_avl <- wmata_avl %>%
    dplyr::filter((route_id == "C53") & (direction_id == 0))
c53_shape <- get_shape_geometry(gtfs = wmata_gtfs,
                                shape = "C53:04",
                                project_crs = 32618)
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

# Use in the AVL cleaning workflow
c53_dists <- get_linear_distances(avl_df = c53_avl,
                                  shape_geometry = c53_shape,
                                  clip_buffer = 50,
                                  project_crs = 32618)
head(c53_dists)
#>   location_ping_id vehicle_id trip_id_performed service_date route_id
#> 1                1       5461          18632100   2026-02-16      C53
#> 2                3       5464          14078100   2026-02-16      C53
#> 3                4       5466          25836100   2026-02-16      C53
#> 4                6       5473           8428100   2026-02-16      C53
#> 6                8       5481           1115100   2026-02-16      C53
#> 7                9       5479            842100   2026-02-16      C53
#>   direction_id   speed trip_stop_sequence     event_timestamp stop_id
#> 1            0  0.0000                 63 2026-02-16 10:58:09    7219
#> 2            0 10.9728                 52 2026-02-16 10:58:31    6843
#> 3            0  0.0000                  2 2026-02-16 10:58:27   13111
#> 4            0  4.5720                 63 2026-02-16 10:58:02    7219
#> 6            0  7.9248                 41 2026-02-16 10:58:10   28286
#> 7            0  8.8392                 29 2026-02-16 10:58:24    4520
#>       distance
#> 1 1.534625e+04
#> 2 1.205652e+04
#> 3 4.191312e-02
#> 4 1.535750e+04
#> 6 9.512327e+03
#> 7 6.551790e+03
```
