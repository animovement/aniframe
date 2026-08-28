# Encode one state event column into bouts

Within each `group_cols` partition, emit one row per maximal run of
identical non-`NA` (normalised) values in `col`. `NA` rows break runs
(so a value sequence like `c("REM", NA, "REM")` becomes two bouts, not
one). `start` is the `time_col` value at the first frame in the run;
`stop` is the value at the last frame.

## Usage

``` r
encode_state_bouts(data, col, time_col, group_cols)
```
