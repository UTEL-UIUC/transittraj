# Filter GTFS to a desired route(s) and direction(s).

This function returns a new `tidygtfs` object with only the information
relevant to your desired routes and directions. All fields included in
the input `gtfs` will be filtered. See `Details` for more information
about required files and fields

## Usage

``` r
filter_by_route(gtfs, route_ids, dir_id = NULL)
```

## Arguments

- gtfs:

  A tidygtfs object.

- route_ids:

  A numeric vector or single numeric containing the desired route ID(s).

- dir_id:

  Optional. A numeric vector or single numeric containing the desired
  direction ID(s).

## Value

A tidygtfs object containing only information relevant to the desired
route and direction.

## Details

The following files and fields are required for this function:

- `routes`: with `route_id` and `agency_id`

- `agency`: with `agency_id`

- `trips`: with `route_id`, `direction_id`, `shape_id`, `service_id`,
  and `trip_id`

- `stop_times`: with `stop_id` and `trip_id`

The following files are optional. If they are included, the must include
the listed fields:

- `stops`: with `stop_id`

- `shapes`: with `shape_id`

- `calendar`: with `service_id`

- `calendar_dates`: with `service_id`

- `transfers`: with `trip_id` and `stop_id`

- `frequencies`: with `trip_id`

- `fare_rules`: with `route_id`

- `feed_info`

For these optional files, the function will detect whether they are
present. If so, they will be filtered; if not, they will be left `NULL`
in the new GTFS. If any required file or field is missing, an error will
be thrown describing what is missing.

## Examples

``` r
# Set my parameters
my_route <- "C53"
my_dir <- 0

# Filter WMATA GTFS
filter_by_route(gtfs = wmata_gtfs,
                route_ids = my_route,
                dir_id = 0)
#> $agency
#> # A tibble: 1 × 8
#>   agency_id agency_name agency_url      agency_timezone agency_lang agency_phone
#>   <chr>     <chr>       <chr>           <chr>           <chr>       <chr>       
#> 1 1         WMATA       https://www.wm… America/New_Yo… en          (202) 637-7…
#> # ℹ 2 more variables: agency_fare_url <chr>, agency_email <chr>
#> 
#> $routes
#> # A tibble: 1 × 9
#>   route_id agency_id route_short_name route_long_name   route_desc route_type
#>   <chr>    <chr>     <chr>            <chr>             <chr>           <int>
#> 1 C53      1         C53              U ST-CONGRESS HTS NA                  3
#> # ℹ 3 more variables: route_url <chr>, route_color <chr>,
#> #   route_text_color <chr>
#> 
#> $trips
#> # A tibble: 952 × 7
#>    route_id service_id trip_id  trip_headsign     direction_id block_id shape_id
#>    <chr>    <chr>      <chr>    <chr>                    <int> <chr>    <chr>   
#>  1 C53      4          10600100 North to Adams M…            0 S002     C53:03  
#>  2 C53      4          2200100  North to Woodley…            0 S002     C53:04  
#>  3 C53      8          10600070 North to Adams M…            0 S002     C53:03  
#>  4 C53      8          2200070  North to Woodley…            0 S002     C53:04  
#>  5 C53      1          13225080 North to Adams M…            0 S009     C53:03  
#>  6 C53      1          7701080  North to Adams M…            0 S009     C53:03  
#>  7 C53      2          10857010 North to Adams M…            0 S009     C53:03  
#>  8 C53      2          25798010 North to Adams M…            0 S009     C53:03  
#>  9 C53      3          13225090 North to Adams M…            0 S009     C53:03  
#> 10 C53      3          7701090  North to Adams M…            0 S009     C53:03  
#> # ℹ 942 more rows
#> 
#> $stop_times
#> # A tibble: 53,100 × 10
#>    trip_id arrival_time departure_time stop_id stop_sequence stop_headsign
#>    <chr>   <time>       <time>         <chr>           <int> <chr>        
#>  1 182010  09:10:00     09:10:00       13111               2 NA           
#>  2 182010  09:10:51     09:10:51       13790               3 NA           
#>  3 182010  09:11:25     09:11:25       13108               4 NA           
#>  4 182010  09:12:00     09:12:00       2584                5 NA           
#>  5 182010  09:12:29     09:12:29       2609                6 NA           
#>  6 182010  09:13:03     09:13:03       13107               7 NA           
#>  7 182010  09:13:28     09:13:28       2683                8 NA           
#>  8 182010  09:14:57     09:14:57       2793                9 NA           
#>  9 182010  09:15:34     09:15:34       2811               10 NA           
#> 10 182010  09:16:24     09:16:24       2867               11 NA           
#> # ℹ 53,090 more rows
#> # ℹ 4 more variables: pickup_type <int>, drop_off_type <int>,
#> #   shape_dist_traveled <dbl>, timepoint <int>
#> 
#> $stops
#> # A tibble: 57 × 8
#>    stop_id stop_code stop_name      stop_desc stop_lat stop_lon zone_id stop_url
#>    <chr>   <chr>     <chr>          <chr>        <dbl>    <dbl> <chr>   <chr>   
#>  1 2584    1000181   Alabama Av SE… NA            38.8    -77.0 NA      NA      
#>  2 2609    1000188   Alabama Av SE… NA            38.8    -77.0 NA      NA      
#>  3 2683    1000203   Alabama Av SE… NA            38.8    -77.0 NA      NA      
#>  4 2793    1000219   Alabama Av SE… NA            38.9    -77.0 NA      NA      
#>  5 2811    1000225   Alabama Av SE… NA            38.9    -77.0 NA      NA      
#>  6 2867    1000238   Alabama Av SE… NA            38.9    -77.0 NA      NA      
#>  7 3314    1000322   Naylor Rd SE+… NA            38.9    -77.0 NA      NA      
#>  8 3421    1000343   Marion Barry … NA            38.9    -77.0 NA      NA      
#>  9 3524    1000376   Marion Barry … NA            38.9    -77.0 NA      NA      
#> 10 3577    1000388   Marion Barry … NA            38.9    -77.0 NA      NA      
#> # ℹ 47 more rows
#> 
#> $shapes
#> # A tibble: 2,613 × 5
#>    shape_id shape_pt_lat shape_pt_lon shape_pt_sequence shape_dist_traveled
#>    <chr>           <dbl>        <dbl>             <int>               <dbl>
#>  1 C53:03           38.8        -77.0             10001                  NA
#>  2 C53:03           38.8        -77.0             10002                  NA
#>  3 C53:03           38.8        -77.0             10003                  NA
#>  4 C53:03           38.8        -77.0             10004                  NA
#>  5 C53:03           38.8        -77.0             10005                  NA
#>  6 C53:03           38.8        -77.0             10006                  NA
#>  7 C53:03           38.8        -77.0             10007                  NA
#>  8 C53:03           38.8        -77.0             10008                  NA
#>  9 C53:03           38.8        -77.0             10009                  NA
#> 10 C53:03           38.8        -77.0             10010                  NA
#> # ℹ 2,603 more rows
#> 
#> $calendar
#> # A tibble: 8 × 10
#>   service_id monday tuesday wednesday thursday friday saturday sunday start_date
#>   <chr>       <int>   <int>     <int>    <int>  <int>    <int>  <int> <date>    
#> 1 1               0       0         0        0      0        0      0 2025-12-14
#> 2 2               0       0         0        0      0        0      1 2025-12-14
#> 3 3               0       0         0        0      0        0      0 2025-12-14
#> 4 4               0       0         0        0      0        0      0 2025-12-14
#> 5 6               0       0         0        0      1        0      0 2025-12-14
#> 6 8               0       0         0        0      0        1      0 2025-12-14
#> 7 9               1       1         0        1      0        0      0 2025-12-14
#> 8 10              0       0         1        0      0        0      0 2025-12-14
#> # ℹ 1 more variable: end_date <date>
#> 
#> $calendar_dates
#> # A tibble: 46 × 3
#>    service_id date       exception_type
#>    <chr>      <date>              <int>
#>  1 1          2025-12-22              1
#>  2 9          2025-12-22              2
#>  3 1          2025-12-23              1
#>  4 9          2025-12-23              2
#>  5 1          2025-12-24              1
#>  6 10         2025-12-24              2
#>  7 2          2025-12-25              1
#>  8 9          2025-12-25              2
#>  9 3          2025-12-26              1
#> 10 6          2025-12-26              2
#> # ℹ 36 more rows
#> 
#> $.
#> $.$dates_services
#> # A tibble: 182 × 2
#>    date       service_id
#>    <date>     <chr>     
#>  1 2025-12-14 2         
#>  2 2025-12-15 9         
#>  3 2025-12-16 9         
#>  4 2025-12-17 10        
#>  5 2025-12-18 9         
#>  6 2025-12-19 6         
#>  7 2025-12-20 8         
#>  8 2025-12-21 2         
#>  9 2025-12-27 8         
#> 10 2025-12-28 2         
#> # ℹ 172 more rows
#> 
#> 
```
