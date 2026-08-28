# The spatial declaration, as a role mapping where there is one

[`get_variables()`](https://animovement.dev/anicore/reference/get_variables.md)
strips names, which for `where` throws the axis roles away. Every path
that re-declares the spatial columns has to start from the mapping
instead, or [`union()`](https://rdrr.io/r/base/sets.html) and
[`setdiff()`](https://rdrr.io/r/base/sets.html) silently reduce a
renamed frame to `unknown` (#109).

## Usage

``` r
get_declared_where(data)
```

## Arguments

- data:

  An aniframe or anievent object.

## Value

Named character vector, or a bare one when no roles are known.
