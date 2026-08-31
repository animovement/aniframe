# Differences between successive angles in a series

Applies
[`circ_difference()`](https://animovement.dev/anicore/reference/circ_difference.md)
along a vector, comparing each angle with the one `lag` positions before
it — the turn from one heading to the next, rather than the difference
between two angles you name. Unlike
[`base::diff()`](https://rdrr.io/r/base/diff.html) the result is the
same length as `x`, padded with `NA` at the start, so it can be used
inside
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

## Usage

``` r
circ_successive_difference(x, lag = 1L)
```

## Arguments

- x:

  A numeric vector of angles, in radians.

- lag:

  A positive integer (default `1L`) giving the lag to difference at.

## Value

A numeric vector the same length as `x`, in radians. The first `lag`
entries are `NA`; the rest are angular differences in `(-pi, pi]`.

## See also

Other circular statistics:
[`circ_difference()`](https://animovement.dev/anicore/reference/circ_difference.md),
[`circ_mad()`](https://animovement.dev/anicore/reference/circ_mad.md),
[`circ_mean()`](https://animovement.dev/anicore/reference/circ_mean.md),
[`circ_median()`](https://animovement.dev/anicore/reference/circ_median.md),
[`circ_sd()`](https://animovement.dev/anicore/reference/circ_sd.md)

## Examples

``` r
circ_successive_difference(c(0, pi / 2, pi, 3 * pi / 2))
#> [1]       NA 1.570796 1.570796 1.570796

# crossing zero is a small step, not a large one
circ_successive_difference(c(6.2, 0.1))
#> [1]        NA 0.1831853
```
