# Understanding Data Inputs

## Introduction

This vignette introduces you to the input data used by `transittraj`.
For most projects, there are two important data sources:

- *Automatic vehicle location (AVL) data*: A set of GPS
  latitude-longitude points describing a transit vehicle’s location over
  time. This package requires that AVL data follow the
  [TIDES](https://tides-transit.org/main/) `vehicle_location` table
  schema.

- *GTFS feed*: The routes, trips, and schedules a transit vehicle
  follows. The most important part is the `shapes` file, which tells us
  the route alignment we expect our AVL data to follow.

We will introduce and discuss these data sources using the two public
datasets included with `transittraj`: An archive of LA Metro’s AVL data,
`lacmta_avl`, and an archive of LA Metro’s static GTFS feed,
`lacmta_gtfs`.

Before we begin, let’s add some needed packages:

``` r

library(transittraj)
library(tidytransit)
```

## Exploring Input Data

### Automatic Vehicle Location Data

`transittraj` is designed to clean and process AVL data. Unfortunately,
AVL data does not have a widely-adopted standardized format. In the
interest of standardizing inputs to the package’s functions,
`transittraj` is designed to take in files adhering to the TIDES
`vehicle_location` standard table schema. TIDES is a new open standard
intended to standardize transit data types not covered by GTFS. The
TIDES `vehicle_location` table schema is described
[here](https://tides-transit.org/main/tables/#vehicle-locations).

Let’s see what TIDES AVL data looks like. The `lacmta_avl` dataset
provided with `transittraj` is from Caltrans’s open-source TIDES
dataset:

``` r

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
```

Due to the low adoption of TIDES, it is possible your AVL data is not in
a TIDES-compliant table. In most cases, a table can be easily converted
by changing the column names and datatypes. If you’re starting with an
archive of GTFS-rt data, [this Python
script](https://github.com/evansiroky/gtfs-rt-to-tides) may help you
convert it to TIDES.

Once you have a TIDES table, we can validate its compliance using
`validatde_tides()`. This will check whether the needed fields are
present, and whether they have the correct data type.

``` r

tides_val <- validate_tides(avl_df = lacmta_avl)
tides_val
#>      required_field required_field_type field_present actual_field_type
#> 1  location_ping_id           character          TRUE         character
#> 2 trip_id_performed           character          TRUE         character
#> 3   event_timestamp             POSIXct          TRUE           POSIXct
#> 4        vehicle_id           character          TRUE         character
#> 5       operator_id           character         FALSE              <NA>
#> 6         longitude             numeric          TRUE           numeric
#> 7          latitude             numeric          TRUE           numeric
#> 8          distance             numeric         FALSE              <NA>
#> 9             speed             numeric          TRUE           numeric
#>   field_type_ok field_ok
#> 1          TRUE     TRUE
#> 2          TRUE     TRUE
#> 3          TRUE     TRUE
#> 4          TRUE     TRUE
#> 5            NA    FALSE
#> 6          TRUE     TRUE
#> 7          TRUE     TRUE
#> 8            NA    FALSE
#> 9          TRUE     TRUE
```

This dataset does not meet two requirements: first, it is missing an
`operator_id` column. This is okay; `operator_id` is not required by any
`transittraj` functions, though there are some that benefit from it.
Second, the dataset is missing `distance`. This is something we’ll
calculate in the cleaning process.

Read more about `transittraj`’s data requirements using
[`help(validate_tides)`](https://obrien-ben.github.io/transittraj/reference/validate_tides.md).
Before running, each `transittraj` function will check that your input
dataframe has the fields and data types that that function requires.

### GTFS Feed

A GTFS feed gives us information we need to effectively use our AVL
data. `transittraj` is designed to use `tidygtfs` objects from the
`tidytransit` package. Let’s look at the GTFS object `lacmta_gtfs`,
which complements the `lacmta_gtfs` dataset we saw above:

``` r

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
```

We recommend checking out
[`tidytransit`](https://r-transit.github.io/tidytransit/) for functions
to read, write, and manipulate GTFS feeds. `transittraj` offers a
handful of additional helper functions for working with GTFS, most
notably
[`filter_by_route()`](https://obrien-ben.github.io/transittraj/reference/filter_by_route.md)
and
[`get_shape_geometry()`](https://obrien-ben.github.io/transittraj/reference/get_shape_geometry.md).
Additionally, we can also create an interactive visualization of a GTFS
feed:

``` r

plot_interactive_gtfs(gtfs = lacmta_gtfs,
                      color_palette = "gtfs")
```

Try clicking on routes or stops to see a pop-up with more information.
This interactive map is very useful for deciding which `shape_id` and
`direction_id` you want to work with.

## Conclusion

We now know what data sources `transittraj` requires. Each function in
the package will check whether the input data meets that function’s
requirements, and if it does not, an error will be thrown describing
what’s wrong. When in doubt, try using
[`validate_tides()`](https://obrien-ben.github.io/transittraj/reference/validate_tides.md)
or
[`tidytransit::validate_gtfs()`](https://r-transit.github.io/tidytransit/reference/validate_gtfs.html)
to check your data.

In the next vignette (`vignette("article/data-workflow-la")`), we’ll
explore the AVL cleaning process.
