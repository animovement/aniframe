# Test for a 1‑D Cartesian coordinate system

The aniframe must contain **exactly one** of `x`, `y` or `z` and none of
the polar columns (`rho`, `phi`, `theta`).

## Usage

``` r
is_cartesian_1d(data, stop = FALSE)
```

## Arguments

- data:

  An aniframe.

- stop:

  Unused, and kept only so the signature does not change. It has no
  effect.

## Value

`TRUE` if the aniframe has exactly one of `x`, `y` or `z` and none of
`rho`, `phi` or `theta`, otherwise `FALSE`.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
is_cartesian_1d(af)
#> [1] FALSE
```
