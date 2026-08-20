# String-keyed kernel shared by `to_anievent` methods

Walks `state_cols` and `point_cols`, encodes each via the run-length /
point-pick helpers, binds the bouts together, and casts the result via
[`as_anievent()`](https://animovement.dev/aniframe/reference/as_anievent.md).

## Usage

``` r
to_anievent_from_columns(
  data,
  time_col,
  state_cols,
  point_cols,
  what_cols,
  when_cols,
  metadata
)
```
