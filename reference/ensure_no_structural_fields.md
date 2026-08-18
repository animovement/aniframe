# Refuse the structural metadata fields

[`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
writes the metadata list and nothing else, which is what makes it safe
to use everywhere. The structural fields need more than that — the frame
has to be retyped, reordered, regrouped and its derived fields refreshed
— so they are reachable only through their own setters.

## Usage

``` r
ensure_no_structural_fields(user_md)
```

## Arguments

- user_md:

  The metadata the caller supplied.

## Value

`TRUE`, invisibly.
