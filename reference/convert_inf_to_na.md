# Convert Inf to NA in numeric columns

Replaces all `Inf` and `-Inf` values with `NA` in numeric columns of a
data frame. The sibling of
[`convert_nan_to_na()`](https://animovement.dev/anicore/reference/convert_nan_to_na.md),
for sources that mark a missing observation with an infinity rather than
a `NaN` — TRex is one, and its own documentation masks `np.inf` out
before plotting.

## Usage

``` r
convert_inf_to_na(data)
```

## Arguments

- data:

  A data frame.

## Value

A data frame with `Inf` and `-Inf` replaced by `NA` in numeric columns.

## Details

Worth doing at read time rather than later: an `Inf` propagates through
arithmetic silently, so a single untracked frame turns a mean, a speed
or a bounding box into `Inf` rather than into a missing value.

## Examples

``` r
df <- data.frame(x = c(1, Inf, -Inf, 3))
convert_inf_to_na(df)
#>    x
#> 1  1
#> 2 NA
#> 3 NA
#> 4  3
```
