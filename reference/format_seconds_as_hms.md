# Format seconds as HH:MM:SS (or HH:MM:SS.fff)

Format seconds as HH:MM:SS (or HH:MM:SS.fff)

## Usage

``` r
format_seconds_as_hms(s, fractional = FALSE)
```

## Arguments

- s:

  Numeric seconds.

- fractional:

  If `TRUE`, format with millisecond precision (`HH:MM:SS.fff`).
  Defaults to `FALSE` (integer seconds, rounded).
