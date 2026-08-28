# Multiplier from a metadata `unit_time` value to seconds

Returns `NA_real_` when conversion is not possible (e.g. `"frame"`
without a `sampling_rate`, or `"unknown"`).

## Usage

``` r
compute_seconds_per_time_unit(unit, sampling_rate)
```
