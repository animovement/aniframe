# Say how far each axis runs

Records the extent of one or more axes, keyed by axis role — the height
of the video frame for `y`, its width for `x`. Roles not named keep the
extent they had.

The extent is what
[`set_axis_directions()`](https://animovement.dev/anicore/reference/set_axis_directions.md)
reflects around when an axis is turned over: `new = extent - old`.

## Usage

``` r
set_axis_extents(data, extents)
```

## Arguments

- data:

  An aniframe object.

- extents:

  Named numeric vector, axis role to extent. Each must be positive and
  finite; `NA` clears an axis.

## Value

The aniframe with updated `axis_extents` metadata.

## See also

[`get_axis_extents()`](https://animovement.dev/anicore/reference/get_axis_extents.md),
[`set_axis_directions()`](https://animovement.dev/anicore/reference/set_axis_directions.md)

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
af <- set_axis_extents(af, c(x = 1920, y = 1080))
get_axis_extents(af)
#>    x    y 
#> 1920 1080 
```
