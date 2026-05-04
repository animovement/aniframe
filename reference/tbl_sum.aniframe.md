# Custom tibble summary for aniframe

Builds the print header rows shown above an aniframe. The set of rows is
driven by the metadata: one row per column listed in `variables_what`
and one row per column in `variables_when` (excluding `time`). This
means custom identity/temporal variables (e.g. `track`, `model`,
`session`) appear automatically, and rows are omitted entirely when
their column is absent.

## Usage

``` r
# S3 method for class 'aniframe'
tbl_sum(x, ...)
```

## Arguments

- x:

  An aniframe object

- ...:

  Additional arguments (unused)

## Value

Named character vector with summary information
