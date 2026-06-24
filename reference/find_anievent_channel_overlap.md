# Find the first overlapping bout pair within any (identity + temporal-grouping + channel) group of an anievent.

Returns `NULL` when no overlap exists; otherwise a small named list
identifying the channel and offending row.

## Usage

``` r
find_anievent_channel_overlap(data)
```
