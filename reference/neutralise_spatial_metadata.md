# Fill the spatial metadata fields with their "not applicable" values

An anievent shares the metadata substrate with
[`aniframe()`](https://animovement.dev/anicore/reference/aniframe.md)
but has no spatial component: a stream of behavioural events has no
axes, no reference frame and no angular unit. Inheriting the movement
defaults made it claim otherwise — a BORIS export read into an anievent
announced a coordinate system it had no coordinates for (#73).

## Usage

``` r
neutralise_spatial_metadata(metadata)
```

## Arguments

- metadata:

  Metadata supplied by the caller.

## Value

`metadata`, with the untouched spatial fields set to their neutral
values.

## Details

Values the caller supplied are left alone, so a reader that knows better
can still say so.
