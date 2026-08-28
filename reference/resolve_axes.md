# Resolve the axis mapping from a metadata list

Objects serialised before the field existed have no `axes`, but their
`variables_where` was matched against the role names to infer a
coordinate system, so the column name *was* the role. Reading it that
way here keeps those frames working untouched.

## Usage

``` r
resolve_axes(md)
```

## Arguments

- md:

  A metadata list.

## Value

Named character vector, empty when no role set applies.
