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
