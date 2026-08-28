# Say which way angles run

With one of the two axes declared the other follows, and is recorded.
With both declared, turning the sense over reverses the **vertical**
axis and reflects that column — the image-plane flip, stated as what it
does to the angles rather than to a corner.

## Usage

``` r
set_angle_direction(data, angle_direction)
```

## Arguments

- data:

  An aniframe object.

- angle_direction:

  Either `"clockwise"` or `"counter_clockwise"`.

## Value

The aniframe, with the axis directions that give this sense and the
vertical axis reflected if it had to turn over.

## See also

[`get_angle_direction()`](https://animovement.dev/anicore/reference/get_angle_direction.md),
[`set_axis_directions()`](https://animovement.dev/anicore/reference/set_axis_directions.md)

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
af <- set_axis_directions(af, c(x = "right"))

# y follows from the sense of rotation
af <- set_angle_direction(af, "counter_clockwise")
get_axis_directions(af)
#>       x       y 
#> "right"    "up" 
```
