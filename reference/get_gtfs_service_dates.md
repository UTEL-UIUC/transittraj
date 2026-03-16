# Get a dataframe of all service dates and their service IDs from a GTFS.

This function returns a dataframe of each date covered by a GTFS and the
`service_id` run on this date. This data is extracted from the
`calendar.txt` and `calendar_dates.txt` files, depending on how the GTFS
is structured. See `Details` for a discussion.

## Usage

``` r
get_gtfs_service_dates(
  gtfs,
  date_min = NULL,
  date_max = NULL,
  use_table = "calendar"
)
```

## Arguments

- gtfs:

  A tidygtfs object.

- date_min:

  Optional. The starting (earliest possible) date for the returned
  dataframe. Default is NULL, where the earliest date in the GTFS will
  be used.

- date_max:

  Optional. The starting (latest possible) date for the returned
  dataframe. Default is NULL, where the latest date in the GTFS will be
  used.

- use_table:

  Optional. Should the GTFS's `calendar.txt` or `calendar_dates.txt` be
  used for the feasible date range? Must be `"calendar"` or
  `"calendar_dates"`. Default is `"calendar"`.

## Value

A dataframe with Date column `date`, and numeric column `service_id`.

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

Use the input parameter `use_table` to control which method to use. If
`use_table = "calendar"`, the former method will be used; if
`use_table = "calendar_dates"`, the latter will be used. To restrict the
date enumeration to only a specific window, set `date_min` and
`date_max`.

## Examples

``` r
# Set parameters
trb_start <- as.Date("2026-01-11")
trb_end <- as.Date("2026-01-15")

# Run function: get service ID by day in date range
trb_service_ids <- get_gtfs_service_dates(gtfs = wmata_gtfs,
                                          date_min = trb_start,
                                          date_max = trb_end,
                                          use_table = "calendar")
#> Error in get_gtfs_service_dates(gtfs = wmata_gtfs, date_min = trb_start,     date_max = trb_end, use_table = "calendar"): Unrecognized min_date data type. Please input Date class (see as.Date()).
head(trb_service_ids)
#> Error: object 'trb_service_ids' not found
```
