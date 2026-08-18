# Ensure the spatial columns match the metadata

Guard form of
[`is_spatial()`](http://animovement.dev/aniframe/reference/is_spatial.md),
for functions that reach coordinates by iterating `variables_where`.
Aborts naming the offending columns, so the error points at the metadata
mismatch rather than surfacing later and further away.

## Usage

``` r
ensure_is_spatial(data)
```

## Arguments

- data:

  An aniframe object.

## Value

The input `data`, invisibly.

## See also

[`is_spatial()`](http://animovement.dev/aniframe/reference/is_spatial.md),
[`validate_aniframe()`](http://animovement.dev/aniframe/reference/validate_aniframe.md).

## Examples

``` r
af <- aniframe(time = 1:5, x = 1:5, y = 1:5)
ensure_is_spatial(af)
```
