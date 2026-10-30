# Corrects distances to be (strictly) monotonic, recursively. Should be for one single trip.

Internal function. Note intended for external use.

## Usage

``` r
correct_dists_fun(d_0, d_e = 0)
```

## Arguments

- d_0:

  A numeric vector of initial positions.

- d_e:

  A numeric distance perturbation to add.

## Value

A numeric vector of strictly monotonic positions.
