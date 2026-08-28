# Work out the sense of rotation from the axis directions

The turn from x to y reads counter-clockwise from the side the depth
axis points to. Which side that is matters: the same scene filmed from
above and from below gives images whose x and y are declared identically
but whose rotations run opposite ways, and only `z` tells them apart.

## Usage

``` r
derive_angle_direction(directions, handedness = "unknown")
```

## Arguments

- directions:

  Named character vector of axis directions.

- handedness:

  A stated handedness, used when no `z` is declared.

## Value

One of `"clockwise"`, `"counter_clockwise"` or `"unknown"`.

## Details

With no `z` declared the sense is the one the recording shows, measured
from where it was taken.
