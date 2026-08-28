# Restructure an aniframe

The tail of
[`as_aniframe()`](https://animovement.dev/anicore/reference/as_aniframe.md),
factored out so that construction and re-declaration cannot drift apart:
validate the declared columns exist, standardise their types, relocate,
arrange, regroup, and refresh the derived `coordinate_system`.

## Usage

``` r
restructure_aniframe(
  data,
  variables_what,
  variables_when,
  variables_where,
  strict = TRUE
)
```

## Arguments

- data:

  An aniframe object.

- variables_what, variables_when, variables_where:

  The declaration to apply.

## Value

`data`, restructured, with the declaration recorded.
