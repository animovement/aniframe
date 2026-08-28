# The axes of a coordinate system that carry a length

The complement of the angular axes that
[`set_unit_angle()`](https://animovement.dev/anicore/reference/set_unit_angle.md)
converts. A spatial unit applies to these and to nothing else: on a
cylindrical frame `rho` and `z` are both lengths while `phi` is an
angle, so converting by column name rather than by role leaves one of
them behind (#98).

## Usage

``` r
get_system_axes(coordinate_system)
```

## Arguments

- coordinate_system:

  A `coordinate_system` metadata value.

## Value

Character vector of axis names, empty when the coordinate system is
`"unknown"` or `"none"`.
