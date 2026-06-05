# LA Metro GTFS

This dataset is a portion of the rail General Transit Feed Specification
(GTFS) from the Los Angeles County Metropolitan Transportation Authority
(LACMTA), or Metro. This feed version was first published on May 27,
2026, and was valid through May 28, 2026. This dataset is intended to be
used alongside the TIDES AVL data provided in `lacmta_avl`.

## Usage

``` r
lacmta_gtfs
```

## Format

### `lacmta_gtfs`

A tidytransit object (list) with 8 files.

- agency:

  The GTFS `agency.txt` file.

- routes:

  The GTFS `routes.txt` file.

- trips:

  The GTFS `trips.txt` file.

- stop_times:

  The GTFS `stop_times.txt` file.

- stops:

  The GTFS `stops.txt` file.

- shapes:

  The GTFS `shapes.txt` file.

- calendar:

  The GTFS `calendar.txt` file.

- calendar_dates:

  The GTFS `calendar_dates.txt` file.

- fare_rules:

  The GTFS `fare_rules.txt` file.

## Source

<https://www.transit.land/feeds/f-9q5-metro~losangeles~rail>

## Details

The GTFS feed has been filtered to two light rail routes, with two
directions for each, on one service date (May 27, 2026):

- Line A: Pomona North to Downtown Long Beach

- Line E: Downtown Santa Monica to Atlantic

## Examples

``` r
# Print the tidytransit summary
summary(lacmta_gtfs)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, fare_rules, shapes, calendar, calendar_dates, stops
#> agency       Metro - Los Angeles
#> service      from 2026-05-27 to 2026-05-27
#> uses         stop_times (no frequencies)
#> # routes       2
#> # trips      483
#> # stop_ids    72
#> # stop_names  72
#> # shapes       4

# Filter by route & direction
my_route <- "804"
my_dir <- 0
lineE_gtfs <- filter_by_route(gtfs = lacmta_gtfs,
                              route_ids = my_route,
                              dir_id = my_dir)
summary(lineE_gtfs)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, fare_rules, shapes, calendar, calendar_dates, stops
#> agency       Metro - Los Angeles
#> service      from 2026-05-27 to 2026-06-05
#> uses         stop_times (no frequencies)
#> # routes       1
#> # trips      123
#> # stop_ids    29
#> # stop_names  29
#> # shapes       1

# Extract route alignments
lineE_shapes <- get_shape_geometry(gtfs = lineE_gtfs)
print(lineE_shapes)
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -118.4915 ymin: 34.01365 xmax: -118.1531 ymax: 34.05471
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 2
#>   shape_id                                                              geometry
#>   <chr>                                                    <MULTILINESTRING [°]>
#> 1 804EB_RC_221121 ((-118.4915 34.01365, -118.4914 34.01389, -118.491 34.01436, …
```
