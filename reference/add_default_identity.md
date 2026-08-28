# Add a default identity variable when the data has none

An aniframe needs **at least one identity (`what`) variable** — the
columns that together say which entity a row belongs to, and which the
frame is grouped by. When auto-detection finds none of the recognised
names in the data, one is added so that rule holds.

## Usage

``` r
add_default_identity(data)
```

## Arguments

- data:

  Data frame to complete.

## Value

`data`, with an identity column added if it had none.

## Details

The column added is `keypoint = "centroid"`. It is not a claim about the
data: it does not mean the frame holds pose or skeleton data, only that
it has a single unnamed entity. A more neutral default
(`individual = "all"`) was considered and rejected in \#77 — the name
stays as it is.

This applies only to the auto-detection path. An explicit
`variables_what = character(0)` is a deliberate declaration of "no
identity variables" and is left alone.
