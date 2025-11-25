# Set the angular unit of an aniframe object

Converts angular measurements (e.g., headings, orientations) stored in
an aniframe object from one unit to another. The function supports
conversion between degrees (`"deg"`) and radians (`"rad"`). It validates
the requested target unit, checks that the supplied columns exist and
contain numeric data, performs the conversion, and updates the object's
metadata.

## Usage

``` r
set_unit_angle(data, cols, to_unit)
```

## Arguments

- data:

  An aniframe object containing angular data.

- cols:

  Character vector of column names that hold the angular values to be
  converted. All listed columns must be present in `data` and be
  numeric.

- to_unit:

  Character string specifying the target angular unit. Must be one of
  the permitted units defined in `default_metadata()$unit_angle`
  (typically `"deg"` or `"rad"`).

## Value

An aniframe object with the selected angular columns converted to the
specified unit and with its `unit_angle` metadata updated accordingly.

## Details

The function proceeds through the following steps:  
• Verifies that `to_unit` is a permitted angular unit.  
• Confirms that each name in `cols` exists in `data` and that the
corresponding columns are numeric.  
• Retrieves the current angular unit from the object's metadata.  
• If the current unit already matches `to_unit`, an informational
message is displayed and the data are returned unchanged.  
• Otherwise, the appropriate conversion function is applied to each
column:

- [`rad_to_deg()`](http://animovement.dev/aniframe/reference/rad_to_deg.md)
  when converting from radians to degrees.  

- [`deg_to_rad()`](http://animovement.dev/aniframe/reference/deg_to_rad.md)
  when converting from degrees to radians.  
  • Finally, the object's metadata are updated with the new
  `unit_angle`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assume `df` is an aniframe with heading angles stored in columns
# "head_left" and "head_right" measured in radians
df_deg <- set_unit_angle(df,
                          cols = c("head_left", "head_right"),
                          to_unit = "deg")

# Convert back to radians
df_rad <- set_unit_angle(df_deg,
                          cols = c("head_left", "head_right"),
                          to_unit = "rad")
} # }
```
