# aniframe package

An R package providing core data structures for movement data.

Creates a specialized data frame for movement data with columns defining
entity identity, timepoints, and spatial position.

## Usage

``` r
aniframe(
  ...,
  metadata = list(),
  variables_what = NULL,
  variables_when = NULL,
  variables_where = NULL,
  .rows = NULL,
  .name_repair = c("check_unique", "unique", "universal", "minimal")
)
```

## Arguments

- ...:

  Name-value pairs to create columns in the data frame.

- metadata:

  Optional list of metadata.

- variables_what:

  Character vector of identity columns that together define a unique
  entity. Defaults to `c("individual", "keypoint")`.

- variables_when:

  Character vector of temporal columns that together define a unique
  timepoint. Defaults to `"time"`.

- variables_where:

  Character vector of spatial columns that together define position.
  Defaults to `c("x", "y")`.

- .rows:

  Number of rows (passed to tibble).

- .name_repair:

  How to repair column names (passed to tibble).

## Value

An aniframe object (tibble with aniframe class).

## See also

Useful links:

- <http://animovement.dev/aniframe/>

- <https://github.com/animovement/aniframe/>

- Report bugs at <https://github.com/animovement/aniframe/issues>

## Author

**Maintainer**: Mikkel Roald-Arbøl <animovement.84w1m@passmail.com>
([ORCID](https://orcid.org/0000-0002-9998-0058))

Authors:

- Mikkel Roald-Arbøl <animovement.84w1m@passmail.com>
  ([ORCID](https://orcid.org/0000-0002-9998-0058))

## Examples

``` r
aniframe(
  individual = rep(1:2, each = 25),
  time = rep(1:10, 5),
  x = rnorm(50),
  y = rnorm(50)
)
#> # Individuals: 1, 2
#> # Keypoints:   centroid
#>    individual keypoint  time      x      y
#>         <int> <fct>    <int>  <dbl>  <dbl>
#>  1          1 centroid     1 -0.387  0.429
#>  2          1 centroid     1 -0.209 -1.90 
#>  3          1 centroid     1  2.04  -0.103
#>  4          1 centroid     2 -0.785  0.122
#>  5          1 centroid     2 -1.40   0.936
#>  6          1 centroid     2  0.449 -0.974
#>  7          1 centroid     3 -1.06  -1.14 
#>  8          1 centroid     3  0.259 -0.309
#>  9          1 centroid     3  1.39   1.27 
#> 10          1 centroid     4 -0.796 -0.558
#> # ℹ 40 more rows

# Custom variables
aniframe(
  track = rep(1:3, each = 10),
  trial = 1,
  time = rep(1:10, 3),
  x = rnorm(30),
  y = rnorm(30),
  variables_what = "track",
  variables_when = c("trial", "time")
)
#> # Tracks: 1, 2, 3
#> # Trials: 1
#>    track trial  time       x      y
#>    <int> <int> <int>   <dbl>  <dbl>
#>  1     1     1     1  0.150  -0.174
#>  2     1     1     2 -1.43   -0.222
#>  3     1     1     3 -0.0103 -1.01 
#>  4     1     1     4 -0.212   0.481
#>  5     1     1     5 -0.906   1.60 
#>  6     1     1     6 -2.10   -1.52 
#>  7     1     1     7  1.89   -1.42 
#>  8     1     1     8 -0.968   0.877
#>  9     1     1     9 -0.103   0.624
#> 10     1     1    10  0.240   2.11 
#> # ℹ 20 more rows
```
