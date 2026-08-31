# Shortest signed distance between two angles

The difference `to_angle - from_angle`, wrapped to `(-pi, pi]` so that
it is the shorter way round the circle rather than the arithmetic
difference. This is the primitive the circular summaries are built on.

## Usage

``` r
circ_difference(from_angle, to_angle)
```

## Arguments

- from_angle:

  A numeric vector of angles, in radians.

- to_angle:

  A numeric vector of angles, in radians.

## Value

Numeric vector of signed angular differences in `(-pi, pi]`, positive
anticlockwise.

## See also

Other circular statistics:
[`circ_mad()`](https://animovement.dev/anicore/reference/circ_mad.md),
[`circ_mean()`](https://animovement.dev/anicore/reference/circ_mean.md),
[`circ_median()`](https://animovement.dev/anicore/reference/circ_median.md),
[`circ_sd()`](https://animovement.dev/anicore/reference/circ_sd.md),
[`circ_successive_difference()`](https://animovement.dev/anicore/reference/circ_successive_difference.md)

## Examples

``` r
# a tenth of a turn, not nine tenths
circ_difference(0.1, 6.1)
#> [1] -0.2831853

circ_difference(c(0, pi / 2), c(pi / 2, 0))
#> [1]  1.570796 -1.570796
```
