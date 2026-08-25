# Set the spatial unit of an aniframe object

Converts the spatial coordinates of an aniframe object to a different
unit of measurement. The function handles both automatic unit conversion
between standard units and custom calibration from pixel or arbitrary
units.

The columns converted are the axes of the frame's `coordinate_system`
that carry a **length** — `x`, `y` and `z` on a Cartesian frame, but
`rho` on a polar or spherical one and `rho` and `z` on a cylindrical
one. The angular axes (`phi`, `theta`) are left alone; they are
[`set_unit_angle()`](https://animovement.dev/aniframe/reference/set_unit_angle.md)'s
to convert.

## Usage

``` r
set_unit_space(data, to_unit, calibration_factor = 1)
```

## Arguments

- data:

  An aniframe object containing spatial coordinate data.

- to_unit:

  Character string specifying the target spatial unit. Must be one of
  the permitted units defined in `default_metadata()$unit_space`.

- calibration_factor:

  Numeric value for scaling spatial coordinates. Default is 1. When
  converting from standard units (mm, cm, m), this is ignored and the
  appropriate conversion factor is calculated automatically. When
  converting from "px" or "unknown", you must provide a calibration
  factor to define the relationship between the current units and the
  target unit.

## Value

An aniframe object with spatial coordinates converted to the specified
unit and updated metadata reflecting the new unit_space.

## Details

The function performs the following operations:

- Validates that `to_unit` is a permitted spatial unit

- Determines the current spatial unit from the object's metadata

- If converting from standard units (mm, cm, m) to another standard
  unit, automatically calculates the conversion factor

- If converting from "px" or "unknown" units with
  `calibration_factor = 1`, issues an informational message and returns
  data unchanged

- Applies the calibration factor to the length axes of the frame's
  coordinate system

- Updates the object's unit_space metadata

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert from millimeters to centimeters (automatic conversion)
data_cm <- set_unit_space(data, to_unit = "cm")

# Convert from pixels to millimeters with custom calibration
# (e.g., 1 pixel = 0.5 mm)
data_mm <- set_unit_space(data, to_unit = "mm", calibration_factor = 0.5)
} # }
```
