# Ensure declared columns are present

Shared by construction
([`ensure_has_aniframe_cols()`](https://animovement.dev/anicore/reference/ensure_has_aniframe_cols.md))
and re-declaration, so a column that isn't there is reported the same
way whichever route the caller took.

## Usage

``` r
ensure_has_declared_cols(data, cols, role)
```

## Arguments

- data:

  A data frame.

- cols:

  Character vector of declared column names.

- role:

  One of `"what"`, `"when"`, `"where"`.

## Value

`TRUE`, invisibly.
