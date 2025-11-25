# Set the temporal unit of an aniframe object

Converts time values in an aniframe object to a different unit of
measurement. The function handles both automatic unit conversion between
standard time units and custom calibration from frame or arbitrary
units.

## Usage

``` r
set_unit_time(data, to_unit, calibration_factor = 1)
```

## Arguments

- data:

  An aniframe object containing time data.

- to_unit:

  Character string specifying the target time unit. Must be one of the
  permitted units defined in `default_metadata()$unit_time` (typically
  "ms", "s", "m", "h" for milliseconds, seconds, minutes, hours).

- calibration_factor:

  Numeric value for scaling time values. Default is 1. When converting
  from standard time units (ms, s, m, h), this is ignored and the
  appropriate conversion factor is calculated automatically. When
  converting from "frame" or "unknown" units, you must provide a
  calibration factor to define the relationship between the current
  units and the target unit.

## Value

An aniframe object with time values converted to the specified unit and
updated metadata reflecting the new unit_time.

## Details

The function performs the following operations:

- Validates that `to_unit` is a permitted time unit

- Determines the current time unit from the object's metadata

- If converting from standard time units (ms, s, m, h) to another
  standard unit, automatically calculates the conversion factor

- If converting from "frame" or "unknown" units with
  `calibration_factor = 1`, issues an informational message and returns
  data unchanged

- Applies the calibration factor to the time column

- Updates the object's unit_time metadata

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert from milliseconds to seconds (automatic conversion)
data_s <- set_unit_time(data, to_unit = "s")

# Convert from frames to seconds with custom calibration
# (e.g., 30 frames per second means 1 frame = 1/30 seconds)
data_s <- set_unit_time(data, to_unit = "s", calibration_factor = 1/30)

# Convert from hours to minutes (automatic conversion)
data_m <- set_unit_time(data, to_unit = "m")
} # }
```
