# Corrects distances to be (strictly) monotonic, recursively, in the backward direction (i.e., pulling down forward drifting). Should be for one single trip.

Internal function. Not intended for external use.

## Usage

``` r
correct_dists_backward(d_0, d_e)
```

## Arguments

- d_0:

  A numeric vector of initial positions.

- d_e:

  A numeric distance perturbation to add.

## Value

A numeric vector of strictly monotonic positions.
