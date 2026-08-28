# Say which way an axis points

Records the direction of one or more axes, keyed by axis role. Roles not
named keep the direction they had, so flipping one axis leaves the rest
alone.

Turning an axis to its opposite reflects that column around the axis
extent, so the data ends up expressed in the direction being declared.
Any other change is a re-description and leaves the values untouched.

## Usage

``` r
set_axis_directions(data, directions)
```

## Arguments

- data:

  An aniframe object.

- directions:

  Named character vector, axis role to direction — one of `right`,
  `left`, `up`, `down`, `back` or `forward`. `NA` clears an axis.

## Value

The aniframe, with reflected coordinates for any axis turned to its
opposite and the new directions recorded.

## Details

Directions are read from where the recording was made: `right`/`left`
across the view, `up`/`down` within it, `back`/`forward` toward and away
from the viewer. No two axes may point along the same pair.

An axis runs from zero to its extent, so turning it over gives
`new = extent - old`. An axis with no declared extent is centred on its
origin instead, and turning it over negates it. Declare one with
[`set_axis_extents()`](https://animovement.dev/anicore/reference/set_axis_extents.md)
for data that is measured from a corner, such as video.

On a frame that stores angles there is no column to reflect, but `phi`
and `theta` are measured from the axes and are recomputed instead.

## See also

[`get_axis_directions()`](https://animovement.dev/anicore/reference/get_axis_directions.md),
[`set_axis_extents()`](https://animovement.dev/anicore/reference/set_axis_extents.md),
[`get_angle_direction()`](https://animovement.dev/anicore/reference/get_angle_direction.md)

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
af <- set_axis_extents(af, c(y = 1080))
af <- set_axis_directions(af, c(x = "right", y = "down"))

# Turning y over reflects it
af <- set_axis_directions(af, c(y = "up"))
get_axis_directions(af)
#>       x       y 
#> "right"    "up" 
```
