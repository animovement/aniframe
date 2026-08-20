# Default metadata structure

Returns a list containing the default metadata fields and their initial
values. The same metadata substrate is shared by both
[`aniframe()`](https://animovement.dev/aniframe/reference/aniframe.md)
and
[`anievent()`](https://animovement.dev/aniframe/reference/anievent.md)
objects; per-class data contracts are versioned via `spec_version`. Most
fields are initialized as `NA` and should be set appropriately for your
data.

## Usage

``` r
default_metadata()
```

## Value

A named list with the following fields:

- `source`: Data source identifier (character, NA)

- `source_version`: Version of the data source (character, NA)

- `filename`: Original filename(s) (character vector, NA). Accepts a
  vector of length 1 or more — readers that load from multiple files
  (e.g. `aniread::read_trackball()`) populate this with all source
  paths.

- `sampling_rate`: Sampling rate in Hz (numeric, NA)

- `start_datetime`: Start date and time of recording (POSIXct, NA)

- `variables_what`, `variables_when`, `variables_where`: The columns
  that carry, respectively, entity identity, temporal position and
  spatial position. These are the structural fields —
  [`as_aniframe()`](https://animovement.dev/aniframe/reference/as_aniframe.md)
  uses them to coerce column types, order columns and rows, group the
  frame, and derive `coordinate_system`. The values here are a
  placeholder skeleton for an object with no data attached; every
  constructor overwrites them from the data or from its arguments. In
  particular `variables_what` is **not** a requirement that a frame
  carry `individual` and `keypoint` columns — the rule is that a frame
  has at least one identity variable, whichever it happens to be.

- `reference_frame`: Reference frame (factor, "allocentric"). Permitted
  values: "allocentric", "egocentric", "none".

- `coordinate_system`: Coordinate system (factor, "cartesian_2d")

- `origin`: Location of the (0,0) coordinate relative to the recording
  frame (factor, "bottom_left"). Permitted values: "bottom_left",
  "top_left", "none".

The spatial fields all have a way of saying "not applicable", because
the metadata substrate is shared with
[`anievent()`](https://animovement.dev/aniframe/reference/anievent.md),
which has no spatial component at all: `unit_space`, `unit_angle`,
`reference_frame` and `origin` take "none", `coordinate_system` takes
"unknown", and `y_height` takes `NA`. An anievent is constructed with
those values rather than inheriting movement defaults it cannot honour
(#73).

- `y_height`: Height of the recording frame in y-axis units (numeric,
  NA). Used by
  [`set_origin()`](https://animovement.dev/aniframe/reference/set_origin.md)
  to reflect y coordinates when switching origin conventions.

- `connections`: Named list of connection tables, one per identity or
  temporal variable (typically `keypoint` for skeletons; could also be
  `individual` for social networks). Each entry is a 2-column tibble of
  `from`/`to` pairs. Default is an empty list. Manage via
  [`set_connections()`](https://animovement.dev/aniframe/reference/set_connections.md),
  [`get_connections()`](https://animovement.dev/aniframe/reference/get_connections.md),
  [`add_connections()`](https://animovement.dev/aniframe/reference/add_connections.md)
  and
  [`remove_connections()`](https://animovement.dev/aniframe/reference/remove_connections.md).

- `variables_event`: Named list with two entries, `state` and `point`,
  each a character vector naming columns that carry per-frame
  categorical event labels. `state` columns are interval-valued
  (durative behaviours, ordered coarse to fine to encode nesting);
  `point` columns are instantaneous (zero-duration events). Foundation
  for the `anievent` class and downstream event-handling utilities.
  Default is an empty list for each.

- `spec_version`: Named list of semantic version strings, one per class
  in the animovement ecosystem (currently `aniframe` and `anievent`).
  Versions the full data contract of each class (mandatory columns,
  validator, and the metadata fields the class uses), independently of
  the package version. Objects serialised before this field existed are
  tolerated by the metadata validator; new objects always get it.

## See also

[`set_metadata()`](https://animovement.dev/aniframe/reference/set_metadata.md),
[`get_metadata()`](https://animovement.dev/aniframe/reference/get_metadata.md)
