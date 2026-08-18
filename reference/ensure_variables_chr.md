# Ensure a declaration is a character vector

Guards the `add_` / `remove_` paths in particular, where
[`union()`](https://rdrr.io/r/base/sets.html) and
[`setdiff()`](https://rdrr.io/r/base/sets.html) would otherwise silently
coerce.

## Usage

``` r
ensure_variables_chr(variables)
```

## Arguments

- variables:

  Value supplied by the caller.

## Value

`TRUE`, invisibly.
