# Circular median

Fisher's circular median: the direction minimising the summed angular
distance to every observation. Where two directions tie, their mean
direction is returned.

## Usage

``` r
circ_median(x, na_rm = TRUE)
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
[`circ_mean()`](https://animovement.dev/anicore/reference/circ_mean.md),
[`circ_sd()`](https://animovement.dev/anicore/reference/circ_sd.md),
[`circ_successive_difference()`](https://animovement.dev/anicore/reference/circ_successive_difference.md)

## Examples

``` r
circ_median(c(0.1, 0.2, 6.2))
#> [1] 0.1

# unaffected by where the circle is cut
circ_median(c(0.1, 0.2, 6.2) + pi)
#> [1] 3.241593
```
