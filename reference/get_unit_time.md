# The unit the index or bout boundaries are in

The unit the index or bout boundaries are in

## Usage

``` r
get_unit_time(data)
```

## Arguments

- data:

  An aniframe or anievent object.

## Value

Length-one character vector.

## See also

[`set_unit_time()`](https://animovement.dev/anicore/reference/set_unit_time.md)

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
get_unit_time(af)
#> [1] "frame"
```
