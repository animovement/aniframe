# Reject roles that no coordinate system defines

Named by the offending role, at the point of declaration — as opposed to
silently degrading the frame to `"unknown"` and failing later in
whichever spatial function the user reaches for first.

## Usage

``` r
ensure_valid_axis_roles(axes)
```

## Arguments

- axes:

  A normalised role-to-column mapping.

## Value

`TRUE`, invisibly.
