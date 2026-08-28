# Work out handedness from three axis directions

The sign of the determinant of the three direction vectors: positive is
the right-handed orientation, the one `right`, `up` and `back` are in.

## Usage

``` r
derive_handedness(directions)
```

## Arguments

- directions:

  Named character vector of axis directions.

## Value

One of `"right"`, `"left"` or `"unknown"`.
