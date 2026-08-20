# Detect the identity / grouping scope of one event column

Returns the minimal subset of `candidate_cols` that the value of
`event_col` varies across (given `time_col`). Used by
[`to_anievent.aniframe()`](https://animovement.dev/aniframe/reference/to_anievent.md)
to drop redundant identity columns — e.g. a `behaviour` column that is
constant across `keypoint` for each `(individual, time)` drops
`keypoint` from the resulting anievent's grouping.

## Usage

``` r
detect_event_scope(data, event_col, candidate_cols, time_col = "time")
```
