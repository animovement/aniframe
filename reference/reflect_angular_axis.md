# Turn an axis over on a frame that stores angles

No column carries the role, so there is nothing to reflect – but the
angles are measured from it, and a frame left claiming a direction its
angles do not agree with is the failure this is here to prevent.

## Usage

``` r
reflect_angular_axis(data, role)
```

## Arguments

- data:

  An aniframe object.

- role:

  An axis role.

## Value

`data`, with the angles it stores measured the other way.

## Details

Turning `x` over reflects `phi` about the vertical, turning `y` over
reflects it about the horizontal, and turning `z` over reflects `theta`
about the equator. Anything else leaves the data alone: the direction is
then a fact about the space rather than about the columns.
