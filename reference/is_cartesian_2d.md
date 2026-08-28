# Test for a 2-D Cartesian coordinate system

Test for a 2-D Cartesian coordinate system

## Usage

``` r
is_cartesian_2d(data)
```

## Arguments

- data:

  An aniframe.

## Value

A logical value.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
is_cartesian_2d(af)
#> [1] TRUE
```
