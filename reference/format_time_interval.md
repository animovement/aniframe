# Build the "Time" interval row for the aniframe print summary

Returns `NULL` when the interval cannot be expressed in seconds (e.g.
`unit_time = "frame"` with no `sampling_rate`, or
`unit_time = "unknown"`). When `start_datetime` is set in metadata,
formats absolute datetimes; otherwise formats elapsed time as
`HH:MM:SS`. Switches to millisecond precision (`HH:MM:SS.fff`) when the
recording is shorter than one second.

## Usage

``` r
format_time_interval(x, md)
```
