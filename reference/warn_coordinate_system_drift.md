# Warn when coordinate_system no longer matches variables_where

`coordinate_system` is derived from `variables_where` by
[`infer_coordinate_system()`](https://animovement.dev/anicore/reference/infer_coordinate_system.md),
but only at construction. Writing the source field on its own leaves the
derived one stale.

## Usage

``` r
warn_coordinate_system_drift(data)
```

## Arguments

- data:

  An aniframe object.

## Value

`TRUE`, invisibly.

## Details

Called only from
[`validate_aniframe()`](https://animovement.dev/anicore/reference/validate_aniframe.md),
after
[`ensure_is_spatial()`](https://animovement.dev/anicore/reference/ensure_is_spatial.md)
has established that `variables_where` declares at least one column.
