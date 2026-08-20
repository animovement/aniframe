# Cast a data frame to an anievent

Strict cast — the input must already be in canonical anievent shape: one
row per bout or instant, with the columns `channel`, `type`, `label`,
`start`, and `stop`. Identity columns travel via `variables_what`. An
optional `modifiers` list-column may carry per-event modifier values
(each cell a character vector, matching the BORIS export format).

## Usage

``` r
as_anievent(
  data,
  metadata = list(),
  variables_what = NULL,
  variables_when = NULL
)

# S3 method for class 'anievent'
as_anievent(
  data,
  metadata = list(),
  variables_what = NULL,
  variables_when = NULL
)

# S3 method for class 'aniframe'
as_anievent(
  data,
  metadata = list(),
  variables_what = NULL,
  variables_when = NULL
)

# S3 method for class 'data.frame'
as_anievent(
  data,
  metadata = list(),
  variables_what = NULL,
  variables_when = NULL
)
```

## Arguments

- data:

  A data frame with the required columns.

- metadata:

  Optional list of metadata.

- variables_what:

  Character vector of identity columns. When `NULL` (default),
  auto-detected from a known list (`model`, `individual`, `subject`,
  `track`, `keypoint`) — only those present in `data` are used. Pass an
  explicit value to use any other column name(s) as identity. An
  anievent with no identity column is permitted (e.g. a single-subject
  experiment).

- variables_when:

  Character vector of temporal columns. When `NULL` (default),
  auto-detected from a known grouping list (`observation`, `session`,
  `trial`) and concatenated with the required temporal endpoints
  `c("start", "stop")`. Pass explicitly to use other names for the
  grouping context.

## Value

An anievent object.

## Details

To *encode* per-frame data (factor / logical / character columns) into
the bout shape, use
[`to_anievent()`](https://animovement.dev/aniframe/reference/to_anievent.md)
instead.
