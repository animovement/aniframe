# Refuse the metadata fields that have their own setters

[`set_metadata()`](https://animovement.dev/aniframe/reference/set_metadata.md)
writes the metadata list and nothing else, which is what makes it safe
to use everywhere. The `variables_*` fields need more than that: they
name columns, so the names have to be checked against the frame, and for
the three structural roles the frame has to be retyped, reordered,
regrouped and its derived fields refreshed. Writing one of them as a
*field* is therefore refused, and the dedicated setters do the job
instead.

## Usage

``` r
ensure_no_declaration_fields(user_md)
```

## Arguments

- user_md:

  The metadata the caller supplied.

## Value

`TRUE`, invisibly.

## Details

Restoring a **complete** metadata object is a different operation, and
is allowed. Rebuilding a frame and putting its metadata back is the
round-trip the class-preserving methods perform internally, and
downstream packages do it too — `animetric::summarise_keypoints()`
recomputes a frame and restores the metadata it captured beforehand.
Refusing that left them no way to carry metadata across a rebuild at
all.
