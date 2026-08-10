# Apply median filters to detect large jumps (i.e., outliers) in trajectories.

Noise in GPS trajectories can manifest itself as one or more points
lying far away from points recorded at a similar time. This function
identifies these points using median filters. By default, outliers are
removed. See `Details` for a discussion of removal methodologies.

## Usage

``` r
clean_jumps(
  distance_df,
  neighborhood_width = 7,
  t_cutoff = 3,
  min_median_deviation = -Inf,
  max_median_deviation = Inf,
  evaluate_tails = FALSE,
  evaluate_implosions = FALSE,
  replace_outliers = FALSE,
  return_removals = FALSE
)
```

## Arguments

- distance_df:

  A dataframe of linearized AVL data. Must include `trip_id_performed`,
  `event_timestamp`, and `distance`.

- neighborhood_width:

  Optional. An integer representing the total sliding window width
  around each observation. Default is 7 (3 on either side).

- t_cutoff:

  Optional. For Hampel filters, number of standardized MADs away to
  consider an outlier. Default is 3.

- min_median_deviation:

  Optional. A numeric, the minimum allowed deviation of an observation
  from its window median, in units of distance. Default is `-Inf`.

- max_median_deviation:

  Optional. A numeric, the maximum allowed deviation of an observation
  from its window median, in units of distance. Default is `Inf`.

- evaluate_tails:

  Optional. A boolean, should the beginning and ending observations,
  before a complete window can be formed, be evaluated? Default is
  `FALSE`.

- evaluate_implosions:

  Optional. A boolean, should points in implosion sequences be
  evaluated? "Implosions" occur when more than half of a window is
  constant. Default is `FALSE`.

- replace_outliers:

  Optional. A boolean, should points identified as outliers be replaced
  by their window median? Default is `FALSE`.

- return_removals:

  Optional. A boolean, should the function return a dataframe of points
  removed and why? Default is `FALSE`.

## Value

The input `distance_df` with violating points removed. If
`return_removals = TRUE`, a dataframe with observations removed and why.

## Details

There are many different types of median filters. In general, these
filters create a sliding window around each point (here, controlled by
`neighborhood_width`) and treats a point based on its deviation from the
median of that window. This function supports two main ways of
classifying outliers based on their deviation:

- Raw deviation: `min_median_deviation` and `max_median_deviation` set
  bounds for an acceptable deviation between a point and the median of
  the window around it, in units of the input `distance` column.

- Hampel filter: Uses the median absolute deviation (MAD), the median of
  deviations from the median. With a conversion factor (`s = 1.48`),
  this is analogous to a standard error. The `t_cutoff`, then, is
  analogous to an acceptable window of `t` values.

Both of these can be used at the same time. If multiple criteria are
set, a point will be removed if it violates any criterion.

A Hampel filter is generally considered highly robust, and is the
recommended approach. There are, however, two main limitations to be
aware of:

- It can struggle at the beginnings and ends of trips, before a complete
  window can be formed. Use `evaluate_tails` to skip these.

- When more than hald of a window has the same value, the MAD is 0 and
  an observation is guaranteed to be flagged as an outlier. This is
  known as an "implosion". As we expect noise in GPS data, even when a
  vehicle is standing still, this is unlikely. It can occur, however,
  near trip terminals, where many GPS points snap to the exact same
  point on the route alignment. Use `evaluate_implosions` to identify
  and skip points in an implosion.

Once a point has been identified as an outlier, there are two possible
treatments, controlled by `replace_outliers`:

- Replacement with the window median. This is the most common approach
  to median filters, but is likely not appropriate for AVL data.
  Replacement may introduce non-monotonicities.

- Removal of the point. This is a less common approach, but may be a
  more sensible for this application, given that interpolating curves
  will be fit later in the cleaning process.

## References

Pearson, Ronald K., Yrjö Neuvo, Jaakko Astola, and Moncef Gabbouj. 2016.
“Generalized Hampel Filters.” EURASIP Journal on Advances in Signal
Processing 2016 (1): 87. https://doi.org/10.1186/s13634-016-0383-6.

## Examples

``` r
# Set my parameters
my_cutoff = 2.5
my_neighborhood = 9

# Get input data
lineE_no_overlaps <- new_transittraj_data("clean_overlapping_subtrips")
dim(lineE_no_overlaps)
#> [1] 3104   10

# Run function
lineE_no_jumps <- clean_jumps(distance_df = lineE_no_overlaps,
                              neighborhood_width = my_neighborhood,
                              t_cutoff = my_cutoff)
dim(lineE_no_jumps)
#> [1] 3089   10
head(lineE_no_jumps)
#> # A tibble: 6 × 10
#>   location_ping_id               service_date trip_id_performed speed vehicle_id
#>   <chr>                          <chr>        <chr>             <dbl> <chr>     
#> 1 4af122e0b668d6821335d641a89ad… 2026-05-27   63383915           1.74 1047-1048…
#> 2 ef3b602e52fe3556a7539491e7792… 2026-05-27   63383915           3.31 1047-1048…
#> 3 a940808be7f3a59066c981bffe3e5… 2026-05-27   63383915           2.15 1047-1048…
#> 4 6df05dfca51b44f25d403356de5a3… 2026-05-27   63383915           0    1047-1048…
#> 5 5326947f997dad696a09f510d4857… 2026-05-27   63383915           0    1047-1048…
#> 6 0eeafa189aab82fe0bff169a9dc58… 2026-05-27   63383915           0    1047-1048…
#> # ℹ 5 more variables: event_timestamp <dttm>, direction_id <int>,
#> #   shape_id <chr>, route_id <chr>, distance <dbl>
```
