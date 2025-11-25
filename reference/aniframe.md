# aniframe package

An R package providing core data structures for movement data.

Creates a specialized data frame for animal tracking data with required
columns for positional data (x/y/z) and time, plus optional columns for
individual, keypoint, trial, and session identifiers.

## Usage

``` r
aniframe(
  ...,
  metadata = list(),
  .rows = NULL,
  .name_repair = c("check_unique", "unique", "universal", "minimal")
)
```

## Arguments

- ...:

  Name-value pairs to create columns in the data frame

- metadata:

  Optional list of metadata

- .rows:

  Number of rows (passed to tibble)

- .name_repair:

  How to repair column names (passed to tibble)

## Value

An aniframe object (tibble with aniframe class)

## See also

Useful links:

- <http://animovement.dev/aniframe/>

- <https://github.com/animovement/aniframe/>

- Report bugs at <https://github.com/animovement/aniframe/issues>

## Author

**Maintainer**: Mikkel Roald-Arbøl <animovement.84w1m@passmail.com>
([ORCID](https://orcid.org/0000-0002-9998-0058))

## Examples

``` r
aniframe(
  individual = rep(1:2, each = 25),
  time = rep(1:10, 5),
  x = rnorm(50),
  y = rnorm(50),
  trial = 1
)
#> # Individuals: 1, 2
#> # Keypoints:   NA
#> # Trials:      1
#>    trial individual keypoint  time        x       y confidence
#>    <int> <fct>      <fct>    <int>    <dbl>   <dbl>      <dbl>
#>  1     1 1          NA           1 -2.44    -0.206          NA
#>  2     1 1          NA           1  2.07     1.11           NA
#>  3     1 1          NA           1 -1.30     1.34           NA
#>  4     1 1          NA           2 -0.00557  0.0192         NA
#>  5     1 1          NA           2 -1.63    -0.246          NA
#>  6     1 1          NA           2  0.738    0.237          NA
#>  7     1 1          NA           3  0.622    0.0296         NA
#>  8     1 1          NA           3  0.512   -1.18           NA
#>  9     1 1          NA           3  1.89     1.32           NA
#> 10     1 1          NA           4  1.15     0.550          NA
#> # ℹ 40 more rows
```
