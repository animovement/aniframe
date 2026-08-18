# Fill the spatial metadata fields with their "not applicable" values

An anievent shares the metadata substrate with
[`aniframe()`](http://animovement.dev/aniframe/reference/aniframe.md)
but has no spatial component: a stream of behavioural events has no
coordinate origin, no reference frame and no angular unit. Inheriting
the movement defaults made it claim otherwise — a BORIS export read into
an anievent announced `origin: bottom_left` (#73).

## Usage

``` r
neutral_spatial_metadata(metadata)
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
