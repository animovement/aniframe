# Warn when a declared sampling rate disagrees with the index

A frame declaring 50 Hz whose timestamps say otherwise is worth knowing
about: it is the same shape as \#98, where the metadata claimed a unit
the data was not in. Only checkable when the index is in a real time
unit – on a frame-indexed recording the rate is the conversion rather
than a claim the gaps can contradict.

## Usage

``` r
warn_sampling_rate_mismatch(data)
```

## Arguments

- data:

  An aniframe object.

## Value

`TRUE`, invisibly.
