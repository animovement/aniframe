# Circular mean

The mean direction: the angle of the vector sum of the unit vectors
pointing along each observation.

## Usage

``` r
circ_mean(x, na_rm = TRUE)
```

## Arguments

- x:

  A numeric vector of angles, in radians.

- na_rm:

  A logical value (default `TRUE`) determining whether missing values
  are removed before computing. When `FALSE`, any `NA` gives `NA`.

## Value

A single angle in `[0, 2*pi)`, or `NA_real_` when there is nothing to
summarise.

## See also

Other circular statistics:
[`circ_difference()`](https://animovement.dev/anicore/reference/circ_difference.md),
[`circ_mad()`](https://animovement.dev/anicore/reference/circ_mad.md),
[`circ_median()`](https://animovement.dev/anicore/reference/circ_median.md),
[`circ_sd()`](https://animovement.dev/anicore/reference/circ_sd.md),
[`circ_successive_difference()`](https://animovement.dev/anicore/reference/circ_successive_difference.md)

## Examples

``` r
# 10 degrees; an arithmetic mean would say 190
rad_to_deg(circ_mean(deg_to_rad(c(350, 30))))
#> [1] 10
```
