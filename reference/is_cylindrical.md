# Test whether an aniframe uses a cylindrical coordinate system

Test whether an aniframe uses a cylindrical coordinate system

## Usage

``` r
is_cylindrical(data)
```

## Arguments

- data:

  An aniframe.

## Value

A logical value.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
is_cylindrical(af)
#> [1] FALSE
```
