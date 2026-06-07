# LA Metro AVL Data

This dataset contains TIDES-formatted automatic vehicle location (AVL)
data from the Los Angeles County Metropolitan Transportation Authority
(LACMTA), or Metro. Pings for all Lines A and E trips starting between
6:00 and 8:00 AM on May 27, 2026 are included. The data was accessed via
Caltrans's [open-source bucket](https://tides.dds.dot.ca.gov/) of TIDES
`vehicle_locations` tables. This dataset is inteded to be used alongside
the static GTFS feed provided in `lacmta_gtfs`.

## Usage

``` r
lacmta_avl
```

## Format

### `lacmta_avl`

A dataframe with 14,179 rows and 11 columns.

- location_ping_id:

  A unique ID for each row.

- service_date:

  The date of the trip's beginning.

- trip_id_performed:

  Trip IDs, matching those in GTFS.

- latitude, longitude:

  The GPS ping longitude and latitude.

- speed:

  The recorded speed, in meters per second.

- vehicle_id:

  An ID corresponding to each vehicle.

- event_timestamp:

  POSIXct time objects, including the day, time, and local timezone.

- direction_id:

  Direction IDs, matching those in GTFS. For Line A, `0` is northbound
  and `1` is soutbound; for Line E, `0` is eastbound and `1` is
  westbound.

- shape_id:

  Shape IDs, matching those in GTFS. Each route and direction has one
  shape ID.

- route_id:

  Route IDs, matching those in GTFS. `"801"` is Line A, and `"804"` is
  Line E.

## Source

<https://tides.dds.dot.ca.gov/>

## Details

The dataset contains two light rail routes, with two directions for
each:

- Line A: Pomona North to Downtown Long Beach

- Line E: Downtown Santa Monica to Atlantic

## Examples

``` r
# Print the header
head(lacmta_avl)
#>                   location_ping_id service_date trip_id_performed latitude
#> 1 4af122e0b668d6821335d641a89ad312   2026-05-27          63383915 34.01514
#> 2 ef3b602e52fe3556a7539491e7792c74   2026-05-27          63383915 34.01448
#> 3 a940808be7f3a59066c981bffe3e537a   2026-05-27          63383915 34.01448
#> 4 6df05dfca51b44f25d403356de5a3e0a   2026-05-27          63383915 34.01421
#> 5 5326947f997dad696a09f510d4857d2c   2026-05-27          63383915 34.01437
#> 6 0eeafa189aab82fe0bff169a9dc587f7   2026-05-27          63383915 34.01397
#>   longitude    speed     vehicle_id     event_timestamp direction_id
#> 1 -118.4904 1.743456 1047-1048-1185 2026-05-27 05:48:58            0
#> 2 -118.4911 3.308096 1047-1048-1185 2026-05-27 05:49:19            0
#> 3 -118.4911 2.145792 1047-1048-1185 2026-05-27 05:49:40            0
#> 4 -118.4913 0.000000 1047-1048-1185 2026-05-27 05:49:59            0
#> 5 -118.4912 0.000000 1047-1048-1185 2026-05-27 05:50:20            0
#> 6 -118.4915 0.000000 1047-1048-1185 2026-05-27 05:50:40            0
#>          shape_id route_id
#> 1 804EB_RC_221121      804
#> 2 804EB_RC_221121      804
#> 3 804EB_RC_221121      804
#> 4 804EB_RC_221121      804
#> 5 804EB_RC_221121      804
#> 6 804EB_RC_221121      804

# Filter the data
lineE_avl <- lacmta_avl %>%
    dplyr::filter((route_id == "804") & (direction_id == 0))
print(unique(lineE_avl$shape_id))
#> [1] "804EB_RC_221121"

# Use in the AVL cleaning workflow
lineE_shape <- get_shape_geometry(gtfs = lacmta_gtfs,
                                  shape = "804EB_RC_221121",
                                  project_crs = 32611)
lineE_dists <- get_linear_distances(avl_df = lineE_avl,
                                    shape_geometry = lineE_shape,
                                    clip_buffer = 50,
                                    project_crs = 32611)
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
