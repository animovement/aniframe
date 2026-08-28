# Normalise a `variables_where` declaration into a role-to-column mapping

An unnamed vector is the historical form, where the column name *is* the
role; it is read that way, which is what keeps every existing frame and
every reader's output working untouched.

## Usage

``` r
normalise_axes(variables_where)
```

## Arguments

- variables_where:

  Character vector, optionally named by axis role.

## Value

Named character vector: names are roles, values are columns.
