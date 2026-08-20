# Set the sampling rate of an aniframe or anievent

Sets the sampling rate (in Hz) on an
[`aniframe()`](https://animovement.dev/aniframe/reference/aniframe.md)
or
[`anievent()`](https://animovement.dev/aniframe/reference/anievent.md)
and, if the object's `unit_time` is currently `"frame"` or `"unknown"`,
converts the temporal columns from frames to seconds using
`1 / sampling_rate`. If `unit_time` is already an SI unit, only the
metadata is updated.

## Usage

``` r
set_sampling_rate(data, sampling_rate)

# S3 method for class 'aniframe'
set_sampling_rate(data, sampling_rate)

# S3 method for class 'anievent'
set_sampling_rate(data, sampling_rate)
```

## Arguments

- data:

  An aniframe or anievent.

- sampling_rate:

  Numeric value in Hz (samples per second).

## Value

The input object with `sampling_rate` metadata updated and, where
applicable, temporal columns converted to seconds.

## Examples

``` r
if (FALSE) { # \dontrun{
# aniframe in frames -> seconds at 30 fps
data_s <- set_sampling_rate(data, sampling_rate = 30)

# anievent: same call shape
ae_s <- set_sampling_rate(ae, sampling_rate = 30)
} # }
```
