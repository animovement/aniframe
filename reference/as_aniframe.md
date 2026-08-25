# Convert a data frame to aniframe

Convert a data frame to aniframe

## Usage

``` r
as_aniframe(
  data,
  metadata = list(),
  variables_what = NULL,
  variables_when = NULL,
  variables_where = NULL
)
```

## Arguments

- data:

  A data frame with movement data.

- metadata:

  A list of metadata to attach to the aniframe.

- variables_what:

  Character vector of identity columns that together define a unique
  entity, and which the frame is grouped by. If `NULL` (the default),
  detected from the data: whichever of `model`, `individual`, `subject`,
  `track` and `keypoint` are present, in that order (coarse to fine). An
  aniframe needs at least one identity variable, so if none of them is
  found, a `keypoint` column is added with the value `"centroid"`. Pass
  `character(0)` to declare no identity variables at all — a deliberate
  opt-out, which leaves the frame ungrouped. Every column named here
  must exist in `data`.

- variables_when:

  Character vector of temporal columns that together define a unique
  timepoint. If `NULL` (the default), detected from the data: whichever
  of `observation`, `session`, `trial` and `time` are present. `time` is
  always required.

- variables_where:

  Character vector of spatial columns that together define position. If
  `NULL` (the default), detected from the data.

## Value

An aniframe object

## Examples

``` r
df <- data.frame(
  time = 1:3, individual = 'a', keypoint = 'centroid',
  x = c(0, 1, 2), y = c(0, 1, 0)
)
as_aniframe(df)
#> # Individuals: a
#> # Keypoints:   centroid
#>   individual keypoint  time     x     y
#>   <fct>      <fct>    <int> <dbl> <dbl>
#> 1 a          centroid     1     0     0
#> 2 a          centroid     2     1     1
#> 3 a          centroid     3     2     0
```
