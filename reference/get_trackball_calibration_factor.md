# Calculate trackball calibration factor

Computes a calibration factor for converting optical flow sensor units
to real-world distance units. The function compares the known physical
distance traveled by a trackball (based on its diameter and number of
rotations) to the measured distance in sensor units.

## Usage

``` r
get_trackball_calibration_factor(data, ball_diameter, ball_rotations)
```

## Arguments

- data:

  An aniframe object containing x and y coordinates from the optical
  flow sensor.

- ball_diameter:

  Numeric. The diameter of the trackball in real-world units (e.g., mm
  or cm).

- ball_rotations:

  Numeric. The number of complete rotations the ball was rotated during
  calibration.

## Value

A numeric calibration factor (real distance / measured distance).
Multiply sensor readings by this factor to convert to real-world units.

## Details

The returned calibration factor can be used in `set_unit_space`.

The function:

- Calculates the real distance traveled as circumference × rotations

- Measures the distance in sensor units using the axis with maximum
  travel

- Returns the ratio of real distance to measured distance

## Examples

``` r
if (FALSE) { # \dontrun{
# Calibrate with a 50mm diameter ball rotated 10 times
cal_factor <- get_trackball_calibration_factor(
  data = sensor_data,
  ball_diameter = 50,
  ball_rotations = 10
)
} # }
```
