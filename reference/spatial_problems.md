# Spatial columns that are missing or not numeric

The shared kernel behind
[`is_spatial()`](http://animovement.dev/aniframe/reference/is_spatial.md)
and
[`ensure_is_spatial()`](http://animovement.dev/aniframe/reference/ensure_is_spatial.md).

## Usage

``` r
spatial_problems(data)
```

## Arguments

- data:

  An aniframe object.

## Value

Named list with the `declared` spatial variables and the `missing` and
`non_numeric` subsets of them.
