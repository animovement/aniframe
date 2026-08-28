# Reflect a spatial axis around a reference value

`reference - data[[axis]]`, which is what turning an axis over amounts
to.

## Usage

``` r
reflect_axis(data, axis, reference)
```

## Arguments

- data:

  A data frame (typically an aniframe) containing `axis`.

- axis:

  Name of the column to reflect.

- reference:

  A single finite value to reflect around.

## Value

The data with `axis` replaced by `reference - data[[axis]]`.
