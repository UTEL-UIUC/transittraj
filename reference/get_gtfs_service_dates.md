# Get a dataframe of all service dates and their service IDs from a GTFS

This function returns a dataframe with each date covered by a GTFS and
the `service_id` run on that date. This data is extracted from the
`calendar.txt` and `calendar_dates.txt` files, depending on how the GTFS
is structured. See `Details` for a discussion.

## Usage

``` r
get_gtfs_service_dates(
  gtfs,
  date_min = NULL,
  date_max = NULL,
  use_calendar_table = "calendar"
)
```

## Arguments

- gtfs:

  A tidygtfs object.

- date_min:

  Optional. The starting (earliest possible) `Date` object for the
  returned dataframe. Default is `NULL`, where the earliest date in the
  GTFS will be used.

- date_max:

  Optional. The ending (latest possible) `Date` object for the returned
  dataframe. Default is `NULL`, where the latest date in the GTFS will
  be used.

- use_calendar_table:

  Optional. Should the GTFS's `calendar.txt` or `calendar_dates.txt` be
  used for the feasible date range? Must be `"calendar"` or
  `"calendar_dates"`. Default is `"calendar"`.

## Value

A dataframe with `Date` column `date` and character column `service_id`.

## Details

The GTFS standard allows for two different structurings of
`calendar.txt` and `calendar_dates.txt`:

- Standard service in `calendar.txt`, with exceptions in
  `calendar_dates.txt`. Here, `calendar.txt` will list the standard
  service ID by weekday (e.g., Monday, Tuesday, etc.), and
  `calendar_dates.txt` lists specific dates which are exceptions to
  this. In this scenario, `get_gtfs_service_dates()` will get enumerate
  all weekdays and dates in `calendar.txt`, and assign the correct
  `service_id` to it, depending on if the date is listed as an exception
  in `calendar_dates.txt`.

- All dates of service are enumerated in `calendar_dates.txt`, and
  `calendar.txt` is not used. In this scenario,
  `get_gtfs_service_dates()` will simply filter, clean, and return this
  table.

Use the input parameter `use_calendar_table` to control which method to
use. If `use_calendar_table = "calendar"`, the former method will be
used; if `use_calendar_table = "calendar_dates"`, the latter will be
used. To restrict the date enumeration to only a specific window, set
`date_min` and `date_max`.

This function is also intended for GTFS feeds with only one service ID
per day. Some GTFS providers (including `lacmta_gtfs`) have unique
`service_id`s by route, and thus service dates do not have unique
`service_id`s. Consider filtering your GTFS to a single route before
using this function (see
[`filter_by_route()`](https://obrien-ben.github.io/transittraj/reference/filter_by_route.md)).
If there are multiple service IDs on a given day, the first appearing
will be returned.

## Examples

``` r
# Set parameters
study_date <- as.Date("2026-05-27")

# Get needed input data
lineE_gtfs <- filter_by_route(gtfs = lacmta_gtfs, route_ids = "804",
                              dir_id = 0)

# Run function: get service ID by day in date range
study_service_ids <- get_gtfs_service_dates(gtfs = lineE_gtfs,
                                            date_min = study_date,
                                            date_max = study_date,
                                            use_calendar_table = "calendar")
print(study_service_ids)
#>         date              service_id
#> 1 2026-05-27 RDEC25-804-1_Weekday-90
```
