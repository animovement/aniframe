# Set the angular unit of an aniframe object

Converts angular columns in an aniframe between degrees (`"deg"`) and
radians (`"rad"`), and updates the `unit_angle` metadata to match.

Spatial angular columns (`phi`, `theta`) are converted automatically
whenever they are present in the data, so polar/cylindrical/spherical
coordinates always stay consistent with the declared unit. Additional
angular columns (e.g. heading or orientation columns named outside the
polar family) can be supplied via `cols`.

## Usage

``` r
set_unit_angle(data, to_unit, cols = NULL)
```

## Arguments

- data:

  An aniframe object containing angular data.

- to_unit:

  Character string specifying the target angular unit. Must be one of
  `c("rad", "deg")` (the levels of
  `list_default_metadata()$unit_angle`).

- cols:

  Optional character vector of additional angular column names to
  convert. The spatial angular columns `phi` and `theta` are detected
  automatically and need not be listed; pass `cols` only for non-spatial
  angular columns (e.g. `"heading"`). All listed columns must be present
  and numeric.

## Value

An aniframe object with the relevant angular columns converted to the
specified unit and `unit_angle` metadata updated accordingly.

## Details

If the current `unit_angle` already matches `to_unit`, an informational
message is shown and the data are returned unchanged (apart from the
metadata round-trip).

## Examples

``` r
if (FALSE) { # \dontrun{
# Polar data: phi is converted automatically
df <- data.frame(time = 1:3, rho = 1:3, phi = c(0, pi / 2, pi))
anif <- as_aniframe(df)
anif_deg <- set_unit_angle(anif, to_unit = "deg")

# Custom angular columns alongside the spatial ones
anif2 <- set_unit_angle(anif, to_unit = "deg", cols = "heading")
} # }
```
