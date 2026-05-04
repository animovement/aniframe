# Detect spatial variables from data

Polar-family detection runs first so that cylindrical data (`rho`,
`phi`, `z`) and spherical data (`rho`, `phi`, `theta`) are not
mis-classified as Cartesian on account of their `z` column. The `rho` +
`phi` pair is the signature of a polar-family system; `z` then
distinguishes cylindrical from polar, and `theta` distinguishes
spherical.

## Usage

``` r
detect_variables_where(data)
```

## Arguments

- data:

  Data frame to check.

## Value

Character vector of detected spatial variable names, or NULL if none
found.
