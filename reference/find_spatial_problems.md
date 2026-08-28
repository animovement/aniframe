# Spatial columns that are missing or not numeric

The shared kernel behind
[`is_spatial()`](https://animovement.dev/anicore/reference/is_spatial.md)
and
[`ensure_is_spatial()`](https://animovement.dev/anicore/reference/ensure_is_spatial.md).

## Usage

``` r
find_spatial_problems(data)
```

## Arguments

- data:

  An aniframe object.

## Value

Named list with the `declared` spatial variables and the `missing` and
`non_numeric` subsets of them.
