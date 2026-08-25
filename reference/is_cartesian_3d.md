# Test for a 3‑D Cartesian coordinate system

Requires non‑missing columns `x`, `y` and `z`.

## Usage

``` r
is_cartesian_3d(data)
```

## Arguments

- data:

  An aniframe.

## Value

`TRUE` if the aniframe has `x`, `y` and `z` and none of `rho`, `phi` or
`theta`, otherwise `FALSE`.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
is_cartesian_3d(af)
#> [1] FALSE
```
