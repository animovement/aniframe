# Reflect a spatial axis around a reference value

Internal helper that reflects values in a numeric column around a
reference, computed as `reference - data[[axis]]`. Currently used by
[`set_origin()`](http://animovement.dev/aniframe/reference/set_origin.md)
to flip the y-axis when changing the coordinate origin convention, but
parameterised so it can be reused for other axes (e.g. z) later.

## Usage

``` r
reflect_axis(data, axis, reference)
```

## Arguments

- data:

  A data frame (typically an aniframe) containing `axis`.

- axis:

  Character. Name of the column to reflect.

- reference:

  Numeric. A single finite value to reflect around.

## Value

The data with `axis` replaced by `reference - data[[axis]]`.
