# Standardize column types for aniframe

Converts character identity and temporal variables to factors. Converts
numeric identity and temporal variables (except the index) to integers.
Spatial variables are converted to numeric.

## Usage

``` r
standardise_aniframe_cols(
  data,
  variables_what,
  variables_when,
  variables_where,
  index = "time"
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

- index:

  The index column, which stays numeric. The temporal context variables
  are made categorical.

## Value

Data frame with standardised column types.
