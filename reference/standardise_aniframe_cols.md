# Standardize column types for aniframe

Converts character identity and temporal variables to factors. Spatial
variables are converted to numeric.

## Usage

``` r
standardise_aniframe_cols(
  data,
  variables_what,
  variables_when,
  variables_where
)
```

## Arguments

- data:

  Data frame to standardise.

- variables_what:

  Identity variable names.

- variables_when:

  Temporal variable names.

- variables_where:

  Spatial variable names.

## Value

Data frame with standardised column types.
