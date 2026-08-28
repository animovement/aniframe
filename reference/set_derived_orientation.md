# Declare axis directions by the answer they should give

The shared half of
[`set_handedness()`](https://animovement.dev/anicore/reference/set_handedness.md)
and
[`set_angle_direction()`](https://animovement.dev/anicore/reference/set_angle_direction.md).
Both invert the same one-way derivation, which is under-determined on
its own: the axes already declared supply the rest of the answer, and
when they supply all of it one axis has to turn over.

## Usage

``` r
set_derived_orientation(
  data,
  wanted,
  derive,
  roles,
  turning,
  what,
  required = TRUE
)
```

## Arguments

- data:

  An aniframe object.

- wanted:

  The value the derivation should give.

- derive:

  The derivation to invert.

- roles:

  The axis roles it reads.

- turning:

  Which opposed pair to reverse when every role is declared.

- what:

  Name of the quantity, for messages.

- required:

  Whether too few declared axes is an error. `FALSE` for a quantity that
  can be stated on its own.

## Value

`data`, with directions declared through
[`set_axis_directions()`](https://animovement.dev/anicore/reference/set_axis_directions.md).
