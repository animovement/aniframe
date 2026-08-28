# Get how far each axis runs

Get how far each axis runs

## Usage

``` r
get_axis_extents(data)
```

## Arguments

- data:

  An aniframe or anievent object.

## Value

Named numeric vector, axis role to extent. Empty when the frame declares
none.

## See also

[`set_axis_extents()`](https://animovement.dev/anicore/reference/set_axis_extents.md),
[`get_axis_directions()`](https://animovement.dev/anicore/reference/get_axis_directions.md)

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
get_axis_extents(af)
#> named numeric(0)
```
