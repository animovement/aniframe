# The unit the angular axes are in

The unit the angular axes are in

## Usage

``` r
get_unit_angle(data)
```

## Arguments

- data:

  An aniframe or anievent object.

## Value

Length-one character vector.

## See also

[`set_unit_angle()`](https://animovement.dev/anicore/reference/set_unit_angle.md)

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
get_unit_angle(af)
#> [1] "rad"
```
