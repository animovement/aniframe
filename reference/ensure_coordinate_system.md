# Ensure the frame is in one of the coordinate systems a caller needs

The shared guard behind
[`ensure_is_polar()`](https://animovement.dev/anicore/reference/ensure_is_polar.md)
and its siblings. It does the check itself rather than being handed the
answer, so it keeps the rule the rest of the package follows: `is_*()`
returns a logical, `ensure_*()` errors.

## Usage

``` r
ensure_coordinate_system(data, permitted, wanted)
```

## Arguments

- data:

  An aniframe object.

- permitted:

  Coordinate systems that satisfy the caller.

- wanted:

  Human-readable name of the required coordinate system.

## Value

`TRUE`, invisibly.

## Details

Reports what the frame *is* in, and points at the two ways out: saying
what the columns mean, or converting the coordinates.
