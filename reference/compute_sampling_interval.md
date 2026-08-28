# Derive the sampling interval from the index

The median gap, which is unmoved by a few dropped frames in a way the
mean is not.

## Usage

``` r
compute_sampling_interval(data)
```

## Arguments

- data:

  An aniframe object.

## Value

Numeric scalar, or `NA` when the frame has no gaps to measure.
