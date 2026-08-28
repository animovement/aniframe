# Was this declaration written as an explicit role mapping?

Explicit roles are validated strictly and an unrecognised one aborts. A
bare vector of column names keeps the older, lenient behaviour of
warning and falling back to `"unknown"`, because that is what readers
and existing frames rely on.

## Usage

``` r
has_axis_roles(variables_where)
```

## Arguments

- variables_where:

  The declaration as supplied.

## Value

`TRUE` when every element carries a role name.
