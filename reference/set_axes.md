# Declare which column carries which axis role

The mapping decides the `coordinate_system` and is what spatial
transformations index by, so — like the `variables_*` declarations — it
is not reachable through
[`set_metadata()`](https://animovement.dev/anicore/reference/set_metadata.md)
and has its own setter, which restructures the frame too.

## Usage

``` r
set_axes(data, axes)
```

## Arguments

- data:

  An aniframe object.

- axes:

  Named character vector: names are axis roles, values are the columns
  carrying them. The roles must form a coordinate system, and every
  column must exist in `data`.

## Value

`data`, re-declared and restructured.

## Details

The direction is role to column, the same way round as
[`get_axes()`](https://animovement.dev/anicore/reference/get_axes.md)
returns it and as
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)
reads, so `set_axes(af, get_axes(af))` does nothing.

## See also

[`get_axes()`](https://animovement.dev/anicore/reference/get_axes.md)

## Examples

``` r
df <- data.frame(time = 1:3, individual = "a", u = c(1, 2, 3), v = c(0, 1, 0))
af <- as_aniframe(df, variables_where = c("u", "v"))
#> Warning: Could not infer coordinate system from spatial variables: "u" and "v".
#> ℹ Setting coordinate system to "unknown".
#> ℹ To keep the coordinate system, say which axis each column carries with
#>   `set_axes()`.
get_metadata(af, "coordinate_system")
#> [1] unknown
#> 7 Levels: unknown cartesian_1d cartesian_2d cartesian_3d polar ... spherical

af <- set_axes(af, c(x = "u", y = "v"))
get_axes(af)
#>   x   y 
#> "u" "v" 
get_metadata(af, "coordinate_system")
#> [1] cartesian_2d
#> 7 Levels: unknown cartesian_1d cartesian_2d cartesian_3d polar ... spherical
```
