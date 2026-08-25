# Test whether an aniframe uses a spherical coordinate system

Requires `rho`, `phi` and `theta`; forbids `z`.

## Usage

``` r
is_spherical(data)
```

## Arguments

- data:

  An aniframe.

## Value

A logical value.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
is_spherical(af)
#> [1] FALSE
```
