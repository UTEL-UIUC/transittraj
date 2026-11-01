## Submission Details

This is a patch package update. In this version we have:

* Rewrote an exported functions (`make_monotonic()`) and its tests, with
a new internal function (`correct_dists_fun()`) to support. User-facing
functionality has not changed, but performance is greatly improved.

* Removed import of `data.table` package; this was used only by the old version
of `make_monotonic()`.

* Reorganized `README` to list new vignettes on the package website.

* Incremented package version to 1.1.1.

A previous CRAN submission for this version failed due to an old date in
DESCRIPTION. This has been updated.

## R CMD check results

0 errors | 0 warnings | 0 note

* This is an update. There are no new errors or warnings.

## Reverse Dependencies

This package has no reverse dependencies.
