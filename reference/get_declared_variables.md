# Columns declared by the metadata, keyed by role

`variables_event` is a named list of `state` / `point` columns rather
than a flat vector, so it is flattened here to give every role the same
shape. `NA` entries mean "unset" and are dropped.

## Usage

``` r
get_declared_variables(md)
```

## Arguments

- md:

  An aniframe metadata list.

## Value

Named list of character vectors, one per declaration field.

## Details

`variables_index` is read through
[`resolve_index()`](https://animovement.dev/anicore/reference/resolve_index.md)
rather than directly, so a frame serialised before the field existed
reports the `time` column it was built with rather than nothing at all.
