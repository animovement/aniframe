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
  entity. Defaults to `c("individual", "keypoint")`.

- variables_when:

  Character vector of temporal columns that together define a unique
  timepoint. Defaults to `"time"`.

- variables_where:

  Character vector of spatial columns that together define position. If
  NULL, detected from data.

## Value

An aniframe object
