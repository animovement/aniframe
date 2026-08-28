# Reflect a vector of angles

Reflect a vector of angles

## Usage

``` r
reflect_angle(x, about, unit, wrap = TRUE, signed = FALSE)
```

## Arguments

- x:

  Numeric vector of angles.

- about:

  `"zero"` to negate, `"half_turn"` to take the supplement.

- unit:

  The frame's `unit_angle`.

- wrap:

  Whether the result is a bearing, and so has to come back onto a full
  turn.

- signed:

  Whether that range is the signed one rather than `[0, 2pi)`.

## Value

The reflected angles, in the same unit and range.
