# Custom tibble summary for anievent

Builds the print header rows shown above an `anievent`. Shows identity
columns (`variables_what`), the unique event channels carried by the
`channel` column, and the standard sampling-rate row inherited from the
metadata substrate.

## Usage

``` r
# S3 method for class 'anievent'
tbl_sum(x, ...)
```

## Arguments

- x:

  An anievent object.

- ...:

  Additional arguments (unused).

## Value

Named character vector with summary information.
