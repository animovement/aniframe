# Test for a 1-D Cartesian coordinate system

Test for a 1-D Cartesian coordinate system

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

A logical value.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
is_cartesian_1d(af)
#> [1] FALSE
```
