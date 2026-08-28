# Declare which columns carry identity, time and position

`variables_what`, `variables_when` and `variables_where` name the
columns that carry, respectively, entity identity, temporal position and
spatial position. They are the frame's structure rather than a
description of it:
[`as_aniframe()`](https://animovement.dev/anicore/reference/as_aniframe.md)
uses them to coerce column types, order columns and rows, group the
frame, and derive `coordinate_system`.

These functions declare them *and* restructure the frame to match, so
the two cannot drift apart.
[`set_metadata()`](https://animovement.dev/anicore/reference/set_metadata.md)
refuses these three fields for that reason.

- `set_variables_*()` replaces the declaration.

- `add_variables_*()` appends to it — the common case, and one that
  avoids the footgun of having to restate the existing variables.

- `remove_variables_*()` drops from it.

- `get_variables_*()` reads it.

The column must exist before it can be declared, so the order is always
create-then-declare:

    data |>
      dplyr::mutate(id = "hi") |>
      add_variables_what("id")

## Usage

``` r
get_variables_what(data)

get_variables_when(data)

get_variables_where(data)

set_variables_what(data, variables)

set_variables_when(data, variables)

set_variables_where(data, variables)

add_variables_what(data, variables)

add_variables_when(data, variables)

add_variables_where(data, variables)

remove_variables_what(data, variables)

remove_variables_when(data, variables)

remove_variables_where(data, variables)
```

## Arguments

- data:

  An aniframe or anievent object.

- variables:

  Character vector of column names.

## Value

For the setters, `data` restructured and re-declared. For the getters, a
character vector of column names.

## See also

[`validate_aniframe()`](https://animovement.dev/anicore/reference/validate_aniframe.md),
which reports a frame whose metadata has drifted out of sync by some
other route.

## Examples

``` r
af <- aniframe(time = 1:5, x = 1:5, y = 1:5)

# Declaring an identity column groups the frame by it
af |>
  dplyr::mutate(id = "a") |>
  add_variables_what("id") |>
  dplyr::group_vars()
#> [1] "keypoint" "id"      

# Declaring a third spatial column refreshes coordinate_system
af |>
  dplyr::mutate(z = 0) |>
  add_variables_where("z") |>
  get_metadata("coordinate_system")
#> [1] cartesian_3d
#> 7 Levels: unknown cartesian_1d cartesian_2d cartesian_3d polar ... spherical
```
