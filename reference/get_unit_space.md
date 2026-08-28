# The unit the spatial coordinates are in

The unit the spatial coordinates are in

## Usage

``` r
get_unit_space(data)
```

## Arguments

- data:

  An aniframe or anievent object.

## Value

Length-one character vector.

## See also

[`set_unit_space()`](https://animovement.dev/anicore/reference/set_unit_space.md)

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
get_unit_space(af)
#> [1] "px"
```
