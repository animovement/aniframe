# Test whether an aniframe uses a polar coordinate system

Requires columns `rho` and `phi` and forbids `theta` or `z`.

## Usage

``` r
is_polar(data)
```

## Arguments

- data:

  An aniframe.

## Value

A logical value.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
is_polar(af)
#> [1] FALSE
```
