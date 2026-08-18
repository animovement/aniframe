# Columns declared by the metadata, keyed by role

`variables_event` is a named list of `state` / `point` columns rather
than a flat vector, so it is flattened here to give every role the same
shape. `NA` entries mean "unset" and are dropped.

## Usage

``` r
declared_variables(md)
```

## Arguments

- md:

  An aniframe metadata list.

## Value

Named list of character vectors, one per `variables_*` field.
