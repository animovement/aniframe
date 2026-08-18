# Ensure declared columns are present

Shared by construction
([`ensure_aniframe_cols()`](http://animovement.dev/aniframe/reference/ensure_aniframe_cols.md))
and re-declaration, so a column that isn't there is reported the same
way whichever route the caller took.

## Usage

``` r
ensure_declared_cols_exist(data, cols, role)
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
