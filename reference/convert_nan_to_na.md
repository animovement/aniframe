# Convert NaN to NA in numeric columns

Replaces all `NaN` values with `NA` in numeric columns of a data frame.

## Usage

``` r
convert_nan_to_na(data)
```

## Arguments

- data:

  A data frame.

## Value

A data frame with `NaN` values replaced by `NA` in numeric columns.

## Examples

``` r
df <- data.frame(x = c(1, NaN, 3))
convert_nan_to_na(df)
#>    x
#> 1  1
#> 2 NA
#> 3  3
```
