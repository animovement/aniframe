# Default metadata structure

Returns a list containing the default metadata fields and their initial
values. The same metadata substrate is shared by both
[`aniframe()`](https://animovement.dev/anicore/reference/aniframe.md)
and
[`anievent()`](https://animovement.dev/anicore/reference/anievent.md)
objects; per-class data contracts are versioned via `spec_version`. Most
fields are initialized as `NA` and should be set appropriately for your
data.

## Usage

``` r
list_default_metadata()
```

## Value

A named list with the following fields:

- `source`: Data source identifier (character, NA)

- `source_version`: Version of the data source (character, NA)

- `filename`: Original filename(s) (character vector, NA). Accepts a
  vector of length 1 or more — readers that load from multiple files
  (e.g. `aniread::read_trackball()`) populate this with all source
  paths.

- `sampling_rate`: Sampling rate in Hz (numeric, NA). Declared, not
  derived — set it with
  [`set_sampling_rate()`](https://animovement.dev/anicore/reference/set_sampling_rate.md).

- `sampling_interval`: The interval between consecutive observations
  (numeric, NA), in the unit the index is in. Derived from the index at
  construction and refreshed on every re-declaration, so it describes
  the data rather than a claim about it. Read it with
  [`get_sampling_interval()`](https://animovement.dev/anicore/reference/get_sampling_interval.md);
  ask whether the spacing is even with
  [`is_sampling_regular()`](https://animovement.dev/anicore/reference/is_sampling_regular.md),
  which is computed on demand because dropping rows changes the answer.

- `start_datetime`: Start date and time of recording (POSIXct, NA)

- `variables_index`: The single column the frame is indexed by
  (character, `"time"`). Exactly one column, and it may be called
  anything — the constructor requires *that* column rather than a column
  literally named `time`. It is never one of the `variables_when`, which
  holds the surrounding temporal context and is what the frame is
  grouped by; the index positions each row *within* its context and so
  is never a grouping variable. Read it with
  [`get_index()`](https://animovement.dev/anicore/reference/get_index.md),
  change it with
  [`set_index()`](https://animovement.dev/anicore/reference/set_index.md).
  Absent from objects serialised before the field existed, where it
  reads back as `"time"` — the value they were built with.

- `variables_what`, `variables_when`, `variables_where`: The columns
  that carry, respectively, entity identity, temporal context and
  spatial position. These are the structural fields —
  [`as_aniframe()`](https://animovement.dev/anicore/reference/as_aniframe.md)
  uses them to coerce column types, order columns and rows, group the
  frame, and derive `coordinate_system`. The values here are a
  placeholder skeleton for an object with no data attached; every
  constructor overwrites them from the data or from its arguments. In
  particular `variables_what` is **not** a requirement that a frame
  carry `individual` and `keypoint` columns — the rule is that a frame
  has at least one identity variable, whichever it happens to be. Nor
  does the order of `variables_what` assert a hierarchy: identity
  variables need not nest, and a position in the vector does not mean a
  level. See
  [`list_recognised_variables_what()`](https://animovement.dev/anicore/reference/list_recognised_variables_what.md).

- `axes`: Which column carries which axis role (named character,
  `c(x = "x", y = "y")`). Names are roles — `x`, `y`, `z`, `rho`, `phi`,
  `theta` — and values are the columns carrying them, so a frame whose
  coordinates are called something else still has a usable
  `coordinate_system`. The role set is closed, which is what keeps
  transformations between coordinate systems well defined; the column
  names are free. Empty when the roles are unknown. Read it with
  [`get_axes()`](https://animovement.dev/anicore/reference/get_axes.md),
  change it with
  [`set_axes()`](https://animovement.dev/anicore/reference/set_axes.md).
  `variables_where` names the same columns without their roles, and is
  derived from this.

- `reference_frame`: Reference frame (factor, "allocentric"). Permitted
  values: "allocentric", "egocentric", "none".

- `coordinate_system`: Coordinate system (factor, "cartesian_2d")

- `axis_directions`: Which way each axis points, keyed by axis role
  (named character, empty). One of "right", "left", "up", "down", "back"
  or "forward", read from where the recording was made. Read it with
  [`get_axis_directions()`](https://animovement.dev/anicore/reference/get_axis_directions.md),
  change it with
  [`set_axis_directions()`](https://animovement.dev/anicore/reference/set_axis_directions.md),
  which reflects an axis turned over.
  [`get_angle_direction()`](https://animovement.dev/anicore/reference/get_angle_direction.md)
  and
  [`get_handedness()`](https://animovement.dev/anicore/reference/get_handedness.md)
  follow from it.

- `axis_extents`: How far each axis runs, keyed by axis role (named
  numeric, empty) — the video frame height for `y`. Read it with
  [`get_axis_extents()`](https://animovement.dev/anicore/reference/get_axis_extents.md),
  change it with
  [`set_axis_extents()`](https://animovement.dev/anicore/reference/set_axis_extents.md).
  It is what an axis is reflected around.

- `handedness`: Whether the frame is right- or left-handed (factor,
  "unknown"). Three declared axis directions determine it and are read
  in preference; the field carries the convention on its own for a frame
  that states one without spelling the axes out. Read it with
  [`get_handedness()`](https://animovement.dev/anicore/reference/get_handedness.md),
  change it with
  [`set_handedness()`](https://animovement.dev/anicore/reference/set_handedness.md).

The spatial fields all have a way of saying "not applicable", because
the metadata substrate is shared with
[`anievent()`](https://animovement.dev/anicore/reference/anievent.md),
which has no spatial component at all: `unit_space`, `unit_angle` and
`reference_frame` take "none", `coordinate_system` takes "unknown", and
`axes`, `axis_directions` and `axis_extents` are empty. An anievent is
constructed with those values rather than inheriting movement defaults
it cannot honour (#73).

- `connections`: Named list of connection tables, one per identity or
  temporal variable (typically `keypoint` for skeletons; could also be
  `individual` for social networks). Each entry is a 2-column tibble of
  `from`/`to` pairs. Default is an empty list. Manage via
  [`set_connections()`](https://animovement.dev/anicore/reference/set_connections.md),
  [`get_connections()`](https://animovement.dev/anicore/reference/get_connections.md),
  [`add_connections()`](https://animovement.dev/anicore/reference/add_connections.md)
  and
  [`remove_connections()`](https://animovement.dev/anicore/reference/remove_connections.md).

- `variables_event`: Named list with two entries, `state` and `point`,
  each a character vector naming columns that carry per-frame
  categorical event labels. `state` columns are interval-valued
  (durative behaviours, listed coarse to fine where they nest); `point`
  columns are instantaneous (zero-duration events). Foundation for the
  `anievent` class and downstream event-handling utilities. Default is
  an empty list for each.

- `spec_version`: Named list of semantic version strings, one per class
  in the animovement ecosystem (currently `aniframe` and `anievent`).
  Versions the full data contract of each class (mandatory columns,
  validator, and the metadata fields the class uses), independently of
  the package version. Objects serialised before this field existed are
  tolerated by the metadata validator; new objects always get it.

## See also

[`set_metadata()`](https://animovement.dev/anicore/reference/set_metadata.md),
[`get_metadata()`](https://animovement.dev/anicore/reference/get_metadata.md)

## Examples

``` r
names(list_default_metadata())
#>  [1] "source"            "source_version"    "filename"         
#>  [4] "sampling_rate"     "sampling_interval" "start_datetime"   
#>  [7] "variables_index"   "variables_what"    "variables_when"   
#> [10] "variables_where"   "variables_event"   "axes"             
#> [13] "unit_space"        "unit_angle"        "unit_time"        
#> [16] "reference_frame"   "coordinate_system" "axis_directions"  
#> [19] "axis_extents"      "handedness"        "connections"      
#> [22] "spec_version"     
```
