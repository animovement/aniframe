# Say whether the frame is right- or left-handed

Handedness is what 3D data is usually described by, so this says it
directly rather than through three separate axis directions.

With two axes declared the third follows, and is recorded. With all
three declared, turning the handedness over reverses the **depth** axis
— the one pointing `back` or `forward` — and reflects that column, which
is the conventional way to convert between the two.

Right-handed is the convention across the suite, so
`set_handedness(data)` completes a frame the standard way. It is not
assumed of a frame that has not been asked: which side a recording was
made from is a fact about the recording, and
[`get_handedness()`](https://animovement.dev/anicore/reference/get_handedness.md)
reports `"unknown"` until it is told.

## Usage

``` r
set_handedness(data, handedness = "right")
```

## Arguments

- data:

  An aniframe object.

- handedness:

  Either `"right"` or `"left"`. Right-handed is the convention across
  the suite and the default here; a frame is only left-handed if it is
  told to be.

## Value

The aniframe, with the axis directions that give this handedness and the
depth axis reflected if it had to turn over.

## Details

Two axes cannot fix a handedness, so at least two must already be
declared for the third to follow from this one. Declare them with
[`set_axis_directions()`](https://animovement.dev/anicore/reference/set_axis_directions.md).

## See also

[`get_handedness()`](https://animovement.dev/anicore/reference/get_handedness.md),
[`set_axis_directions()`](https://animovement.dev/anicore/reference/set_axis_directions.md)

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
af <- set_axis_directions(af, c(x = "right", y = "up"))

# z follows from the handedness, which defaults to right
af <- set_handedness(af)
get_axis_directions(af)
#>       x       y       z 
#> "right"    "up"  "back" 
```
