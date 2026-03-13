# Validate inputs to predict trajectory methods & set up dataframes.

Checks that the input `new_times` and `new_distances` to
[`predict.avltrajectory_group()`](https://obrien-ben.github.io/transittraj/reference/predict.avltrajectory_group.md)
and
[`predict.avltrajectory_single()`](https://obrien-ben.github.io/transittraj/reference/predict.avltrajectory_group.md)
meet requirements of these functions, and sets up dataframes for
interpolation. Factoring intended to reduce code duplication.

## Usage

``` r
predict_traj_input_setup(new_times, new_distances)
```

## Arguments

- new_times:

  DF or vector of new times

- new_distances:

  DF or vector of new distances

## Value

List of `new_times_df` and `new_distances_df`
