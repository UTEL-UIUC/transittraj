# Corrects distances to be (strictly) monotonic, recursively. Should be for one single trip.

Internal function. Note intended for external use. Calls to
[`correct_dists_forward()`](https://obrien-ben.github.io/transittraj/reference/correct_dists_forward.md)
and/or
[`correct_dists_backward()`](https://obrien-ben.github.io/transittraj/reference/correct_dists_backward.md),
depending on user-input `direction`.

## Usage

``` r
correct_dists_fun(d_0, d_e, direction)
```

## Arguments

- d_0:

  A numeric vector of initial positions.

- d_e:

  A numeric distance perturbation to add.

- direction:

  A character string indicating the filter direction.

## Value

A numeric vector of strictly monotonic positions.
