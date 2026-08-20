# Validate a complete metadata list and attach it

The write path shared by
[`set_metadata()`](https://animovement.dev/aniframe/reference/set_metadata.md)
and the internal callers that legitimately write structural fields — the
constructors and the variable setters. Unlike
[`set_metadata()`](https://animovement.dev/aniframe/reference/set_metadata.md)
it applies no field-level policy: the caller has already decided what
the metadata should be.

## Usage

``` r
write_metadata(data, metadata)
```

## Arguments

- data:

  An aniframe or anievent object.

- metadata:

  A complete metadata list.

## Value

`data`, with `metadata` attached.
