# Test whether an aniframe uses a Cartesian coordinate system

Returns `TRUE` if the data frame satisfies *any* of the 1-D, 2-D or 3-D
Cartesian checks.

## Usage

``` r
is_cartesian(data)
```

## Arguments

- data:

  An aniframe.

## Value

A logical value.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
is_cartesian(af)
#> [1] TRUE
```
