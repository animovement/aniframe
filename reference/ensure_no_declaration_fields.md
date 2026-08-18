# Refuse the metadata fields that have their own setters

[`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
writes the metadata list and nothing else, which is what makes it safe
to use everywhere. The `variables_*` fields need more than that: they
name columns, so the names have to be checked against the frame, and for
the three structural roles the frame has to be retyped, reordered,
regrouped and its derived fields refreshed. They are therefore reachable
only through their own setters.

## Usage

``` r
ensure_no_declaration_fields(user_md)
```

## Arguments

- user_md:

  The metadata the caller supplied.

## Value

`TRUE`, invisibly.
