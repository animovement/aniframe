# Test for a 2‑D Cartesian coordinate system

Requires columns `x` and `y`. Column `z` may be present only if it is
completely `NA`.

## Usage

``` r
is_cartesian_2d(data)
```

## Arguments

- data:

  An aniframe.

## Value

`TRUE` if the aniframe has `x` and `y` and none of `rho`, `phi` or
`theta`, otherwise `FALSE`.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
is_cartesian_2d(af)
#> [1] TRUE
```
