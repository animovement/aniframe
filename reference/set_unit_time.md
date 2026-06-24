# Set the temporal unit of an aniframe or anievent

Converts the temporal columns of an `aniframe` (the `time` column) or
`anievent` (the `start` and `stop` columns) to a different unit of
measurement. Handles automatic conversion between standard SI time units
and custom calibration from frame or arbitrary units.

## Usage

``` r
set_unit_time(data, to_unit, calibration_factor = 1)

# S3 method for class 'aniframe'
set_unit_time(data, to_unit, calibration_factor = 1)

# S3 method for class 'anievent'
set_unit_time(data, to_unit, calibration_factor = 1)
```

## Arguments

- data:

  An
  [`aniframe()`](http://animovement.dev/aniframe/reference/aniframe.md)
  or
  [`anievent()`](http://animovement.dev/aniframe/reference/anievent.md)
  object.

- to_unit:

  Character string specifying the target time unit. Must be one of the
  permitted units defined in `default_metadata()$unit_time` (typically
  `"ms"`, `"s"`, `"m"`, `"h"`).

- calibration_factor:

  Numeric value for scaling time values. Default is 1. When converting
  from standard time units (`ms`, `s`, `m`, `h`), this is ignored and
  the appropriate conversion factor is calculated automatically. When
  converting from `"frame"` or `"unknown"` units, you must provide a
  calibration factor to define the relationship between the current and
  target units.

## Value

The input object with temporal columns converted to `to_unit` and
`unit_time` metadata updated accordingly.

## Details

For an `aniframe` the `time` column is multiplied by the calibration
factor; for an `anievent` both `start` and `stop` are. In either case:

- the function validates `to_unit` against the permitted levels;

- if converting from a standard unit (`ms`, `s`, `m`, `h`) to another
  standard unit, the calibration factor is auto-computed;

- if converting from `"frame"` or `"unknown"` with
  `calibration_factor = 1`, an informational message is emitted and the
  data values are left unchanged (the metadata still flips to
  `to_unit`);

- the object's `unit_time` metadata is updated.

## Examples

``` r
if (FALSE) { # \dontrun{
# aniframe: convert milliseconds to seconds (automatic)
data_s <- set_unit_time(data, to_unit = "s")

# aniframe: convert frames to seconds at 30 fps
data_s <- set_unit_time(data, to_unit = "s", calibration_factor = 1 / 30)

# anievent: same call shape; mutates start/stop instead of time
ae_s <- set_unit_time(ae, to_unit = "s", calibration_factor = 1 / 30)
} # }
```
