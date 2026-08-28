# Ensure the index column is present and numeric

Which column that is comes from the frame's own declaration; `time` is
its default, not a requirement (#109).

## Usage

``` r
ensure_has_index(data)
```

## Arguments

- data:

  An aniframe object.

## Value

`TRUE`, invisibly.
