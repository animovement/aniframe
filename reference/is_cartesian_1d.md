# Test for a 1‑D Cartesian coordinate system

The data frame must contain **exactly one** of `x`, `y` or `z` and none
of the polar columns (`rho`, `phi`, `theta`).

## Usage

``` r
is_cartesian_1d(data, stop = FALSE)
```

## Arguments

- data:

  A data frame.

- stop:

  Unused placeholder kept for API compatibility.

## Value

Logical scalar (invisible).
