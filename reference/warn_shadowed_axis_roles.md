# Warn when an axis role is carried by one column while another has its name

`get_axes(af)[["x"]]` may be `"u"` while the frame also has a column
literally called `x`. The frame is not malformed and the mapping is
right, but `.data$x` then returns a real column of real numbers that is
not the x axis — plausible wrong answers rather than an error, and the
habit axis roles exist to replace (#119).

## Usage

``` r
warn_shadowed_axis_roles(axes, columns)
```

## Arguments

- axes:

  A normalised role-to-column mapping.

- columns:

  The frame's column names.

## Value

`TRUE`, invisibly.

## Details

A warning, not an error: the state is legal, and a column named `x` may
honestly mean something else. Silence it for a whole loop with
`options(aniframe.quiet = TRUE)`.
