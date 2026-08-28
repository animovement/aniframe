# The sampling rate, in Hz

The sampling rate, in Hz

## Usage

``` r
get_sampling_rate(data)
```

## Arguments

- data:

  An aniframe or anievent object.

## Value

Numeric scalar, or `NA` when the rate is not recorded.

## See also

[`set_sampling_rate()`](https://animovement.dev/anicore/reference/set_sampling_rate.md)

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
get_sampling_rate(af)
#> [1] NA
```
