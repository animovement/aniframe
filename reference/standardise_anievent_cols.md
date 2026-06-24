# Standardise column types for an anievent

Coerces identity and temporal-grouping columns to factor/integer
(mirroring the aniframe convention), `channel` to character, `label` to
factor, and `start`/`stop` to numeric.

## Usage

``` r
standardise_anievent_cols(data, variables_what, variables_when)
```

## Arguments

- data:

  Data frame to standardise.

- variables_what:

  Identity variable names.

- variables_when:

  Temporal variable names — grouping columns (everything except
  `start`/`stop`) are coerced like identity columns; `start` and `stop`
  are forced numeric.

## Value

Data frame with standardised column types.
