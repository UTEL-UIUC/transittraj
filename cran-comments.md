## Submission Details

This is a minor-version package update. In this version we have:

* Resolved check error due to a missing API key in a vignette. The vignette
no longer relies on Internet access or that API.

* Rewrote some exported functions (`get_linear_distances()`,
`project_onto_route()`) and their tests. User-facing functionality has not
changed, but performance is greatly improved.

* Added import of `geos` package.

* Incremented package version to 1.1.0.

## R CMD check results

0 errors | 0 warnings | 0 note

* This is an update. There are no new errors or warnings.

## Reverse Dependencies

This package has no reverse dependencies.
