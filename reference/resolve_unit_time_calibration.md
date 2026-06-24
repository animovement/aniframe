# Resolve the multiplicative factor for a unit_time conversion

Shared between
[`set_unit_time.aniframe()`](http://animovement.dev/aniframe/reference/set_unit_time.md)
and
[`set_unit_time.anievent()`](http://animovement.dev/aniframe/reference/set_unit_time.md).
Validates `to_unit`, reads the current `unit_time` from metadata, and
returns the calibration factor to apply to the temporal columns. Emits
an informational message and returns 1 (no-op on data values) when the
source unit is `"frame"` / `"unknown"` and no calibration factor was
supplied.

## Usage

``` r
resolve_unit_time_calibration(data, to_unit, calibration_factor)
```
