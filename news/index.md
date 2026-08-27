# Changelog

## aniframe (development version)

### Added

- Every exported function now has a runnable example
  ([\#106](https://github.com/animovement/aniframe/issues/106)).

### Fixed

- [`set_unit_space()`](https://animovement.dev/aniframe/reference/set_unit_space.md)
  converts the length axes of the frame’s coordinate system rather than
  whichever of `x`, `y` and `z` are present
  ([\#98](https://github.com/animovement/aniframe/issues/98)). `rho` is
  a length on polar, cylindrical and spherical frames and was never
  converted, while the metadata was updated to claim the new unit.
  Angular axes remain
  [`set_unit_angle()`](https://animovement.dev/aniframe/reference/set_unit_angle.md)’s
  to convert. Where the coordinate system is `unknown` a length cannot
  be told from an angle, and the function now warns rather than silently
  converting nothing.

- [`set_unit_space()`](https://animovement.dev/aniframe/reference/set_unit_space.md),
  [`set_unit_angle()`](https://animovement.dev/aniframe/reference/set_unit_angle.md),
  [`set_unit_time()`](https://animovement.dev/aniframe/reference/set_unit_time.md)
  and
  [`set_sampling_rate()`](https://animovement.dev/aniframe/reference/set_sampling_rate.md)
  no longer re-inject a `keypoint` column and overwrite `variables_what`
  with it ([\#96](https://github.com/animovement/aniframe/issues/96)). A
  frame given a custom identity such as `id` was silently regrouped on a
  constant column.

- [`as_aniframe()`](https://animovement.dev/aniframe/reference/as_aniframe.md)
  keeps the roles a frame already declares rather than re-deriving them,
  so casting an aniframe is no longer destructive
  ([\#96](https://github.com/animovement/aniframe/issues/96)). A
  declaration whose columns have since been dropped still falls through
  to detection, so a cast continues to repair a drifted frame.

## aniframe 0.7.0 (2026-08-18)

### Added

- [`set_variables_what()`](https://animovement.dev/aniframe/reference/variables.md),
  [`set_variables_when()`](https://animovement.dev/aniframe/reference/variables.md),
  [`set_variables_where()`](https://animovement.dev/aniframe/reference/variables.md)
  and
  [`set_variables_event()`](https://animovement.dev/aniframe/reference/variables_event.md)
  declare the variable roles, each with `get_`, `add_` and `remove_`
  verbs ([\#82](https://github.com/animovement/aniframe/issues/82)).
  They declare the role *and* restructure the frame to match, so the
  metadata and the frame cannot drift apart. `add_variables_*()`
  appends, so adding one identity column no longer means restating the
  others.

- [`validate_aniframe()`](https://animovement.dev/aniframe/reference/validate_aniframe.md)
  re-checks that the metadata still describes the frame: every declared
  column present, `time` and the spatial columns numeric
  ([\#79](https://github.com/animovement/aniframe/issues/79)).
  Counterpart to
  [`validate_anievent()`](https://animovement.dev/aniframe/reference/validate_anievent.md).

- [`is_spatial()`](https://animovement.dev/aniframe/reference/is_spatial.md)
  and
  [`ensure_is_spatial()`](https://animovement.dev/aniframe/reference/ensure_is_spatial.md)
  test the columns named in `variables_where`, which the
  `is_cartesian*()` family does not — those look at column names only
  ([\#79](https://github.com/animovement/aniframe/issues/79)).

### Changed

- [`set_metadata()`](https://animovement.dev/aniframe/reference/set_metadata.md)
  no longer writes the `variables_*` fields; use their dedicated setters
  ([\#82](https://github.com/animovement/aniframe/issues/82)). Writing
  them as plain metadata left the frame typed, ordered and grouped as
  before, so operations silently integrated across identities. A
  complete metadata object can still be restored wholesale.

- [`as_aniframe()`](https://animovement.dev/aniframe/reference/as_aniframe.md)
  errors when `variables_what` names a column that is not in the data,
  as it already did for `variables_when` and `variables_where`
  ([\#77](https://github.com/animovement/aniframe/issues/77)).

- `aniframe` and `anievent` recognise the same identity variables —
  `model`, `individual`, `subject`, `track`, `keypoint` — ordered coarse
  to fine ([\#77](https://github.com/animovement/aniframe/issues/77)).

- `spec_version` moves to `aniframe = "1.1.0"` and `anievent = "0.2.0"`.

### Removed

- [`as_aniframe()`](https://animovement.dev/aniframe/reference/as_aniframe.md)
  no longer adds a `keypoint = "centroid"` column to data that already
  has an identity column
  ([\#77](https://github.com/animovement/aniframe/issues/77)). Results
  are unaffected — the column was constant — but it no longer appears in
  the frame or the print header.

### Fixed

- Downstream subclasses survive the class-preserving methods
  ([\#81](https://github.com/animovement/aniframe/issues/81)).
  `animetric`’s `aniframe_kin` was dropped by the first
  [`filter()`](https://rdrr.io/r/stats/filter.html), `mutate()` or `[`.
  Verbs that were never covered — `distinct()`, joins, `bind_rows()` —
  still drop it.

- An `anievent` no longer claims spatial properties it cannot have, such
  as a BORIS export announcing `origin: bottom_left`
  ([\#73](https://github.com/animovement/aniframe/issues/73)).
  `unit_angle`, `origin` and `reference_frame` gain a `"none"` level.

## aniframe 0.6.0 (2026-06-24)

### Added

- `anievent`, a class for behavioural events in long format — one row
  per bout (state event) or instant (point event)
  ([\#67](https://github.com/animovement/aniframe/issues/67)). A sibling
  of `aniframe`: it shares the metadata substrate but does not inherit
  from it. Required columns are `channel`, `type`, `label`, `start` and
  `stop`, with identity columns travelling via `variables_what` and an
  optional `modifiers` list-column. `type` is derived per
  `(channel, label)` group at construction — a group is `"point"` only
  when all its bouts are instantaneous — and can be set explicitly where
  that misclassifies.

- [`anievent()`](https://animovement.dev/aniframe/reference/anievent.md)
  and
  [`as_anievent()`](https://animovement.dev/aniframe/reference/as_anievent.md)
  construct the class,
  [`is_anievent()`](https://animovement.dev/aniframe/reference/is_anievent.md)
  and
  [`ensure_is_anievent()`](https://animovement.dev/aniframe/reference/ensure_is_anievent.md)
  test it, and
  [`validate_anievent()`](https://animovement.dev/aniframe/reference/validate_anievent.md)
  re-checks its invariants on demand
  ([\#68](https://github.com/animovement/aniframe/issues/68)).
  Class-preserving dplyr and base-R methods keep the class through
  tidyverse pipelines.

- [`to_anievent()`](https://animovement.dev/aniframe/reference/to_anievent.md)
  run-length-encodes per-frame data into bouts, as distinct from
  [`as_anievent()`](https://animovement.dev/aniframe/reference/as_anievent.md),
  which casts data that is already bout-shaped. Methods for `data.frame`
  and `aniframe`; the latter auto-detects each channel’s identity scope,
  so a label constant across keypoints does not produce a duplicate bout
  per keypoint.

- A `variables_event` metadata field — a named list `list(state, point)`
  declaring which columns hold per-frame event labels
  ([\#66](https://github.com/animovement/aniframe/issues/66)). State
  columns are interval-valued, point columns instantaneous; both appear
  in the print header when populated.

- A `spec_version` metadata field, keyed by class, so each class’s data
  contract can evolve independently of the package version
  ([\#65](https://github.com/animovement/aniframe/issues/65)). Older
  serialised objects without it continue to validate.

- [`set_unit_time()`](https://animovement.dev/aniframe/reference/set_unit_time.md)
  and
  [`set_sampling_rate()`](https://animovement.dev/aniframe/reference/set_sampling_rate.md)
  are S3 generics with `aniframe` and `anievent` methods. On an anievent
  the calibration factor applies to `start` and `stop` rather than
  `time`.

- [`as_aniframe()`](https://animovement.dev/aniframe/reference/as_aniframe.md)
  auto-detects `observation` as a temporal grouping column, alongside
  `session` and `trial`.

- New article, “The anievent data structure”, covering channels, state
  and point events, modifiers, validation and multi-observation data
  ([\#70](https://github.com/animovement/aniframe/issues/70)).

### Changed

- [`validate_anievent()`](https://animovement.dev/aniframe/reference/validate_anievent.md)
  warns when two bouts of the same `channel` overlap within a group. A
  warning rather than an error: overlap is normal BORIS output and the
  long format handles it natively.

- [`set_metadata()`](https://animovement.dev/aniframe/reference/set_metadata.md)
  accepts partial `variables_event` input — supplying only `state` or
  only `point` is fine, and `NA` or empty entries read as “none” rather
  than erroring
  ([\#76](https://github.com/animovement/aniframe/issues/76)).

- The metadata print heading reads “animovement metadata”, since the
  substrate is shared by both classes
  ([\#69](https://github.com/animovement/aniframe/issues/69)). The S3
  class name is unchanged.

## aniframe 0.5.0 (2026-05-04)

### Added

- [`set_origin()`](https://animovement.dev/aniframe/reference/set_origin.md)
  converts between the `bottom_left` and `top_left` origin conventions,
  reflecting `y` around the recorded frame height
  ([\#52](https://github.com/animovement/aniframe/issues/52)).

- [`set_y_height()`](https://animovement.dev/aniframe/reference/set_y_height.md)
  sets the y-axis frame height that
  [`set_origin()`](https://animovement.dev/aniframe/reference/set_origin.md)
  uses, validated against the data range, and a `y_height` metadata
  field to hold it. Readers populate it from the source;
  [`as_aniframe()`](https://animovement.dev/aniframe/reference/as_aniframe.md)
  falls back to `max(y)` when missing, and never overwrites an existing
  value.

- A `connections` metadata field for skeletons and other variable-level
  networks ([\#6](https://github.com/animovement/aniframe/issues/6)).
  Stored as a named list keyed by the relevant identity or temporal
  variable — typically `keypoint`, but `individual` for social networks
  — with each entry a 2-column `from`/`to` tibble whose order is
  preserved. Manage it with
  [`set_connections()`](https://animovement.dev/aniframe/reference/set_connections.md),
  [`get_connections()`](https://animovement.dev/aniframe/reference/get_connections.md),
  [`add_connections()`](https://animovement.dev/aniframe/reference/add_connections.md)
  and
  [`remove_connections()`](https://animovement.dev/aniframe/reference/remove_connections.md).
  Endpoints missing from the corresponding column warn but are kept.

- A “Time” row in the print summary showing the tracked interval as
  `HH:MM:SS to HH:MM:SS`, or as absolute datetimes when `start_datetime`
  is set ([\#50](https://github.com/animovement/aniframe/issues/50)).
  Sub-second runs use millisecond precision.

- New articles introducing the data structure: “The aniframe data
  structure”, “Metadata on an aniframe” and “Connections”.

### Changed

- [`set_unit_angle()`](https://animovement.dev/aniframe/reference/set_unit_angle.md)
  converts the spatial angular columns `phi` and `theta` whenever they
  are present, so polar, cylindrical and spherical coordinates stay
  consistent with the declared `unit_angle`
  ([\#21](https://github.com/animovement/aniframe/issues/21)). These
  were previously assumed to be radians and left alone. The signature
  becomes `set_unit_angle(data, to_unit, cols = NULL)`, matching
  [`set_unit_time()`](https://animovement.dev/aniframe/reference/set_unit_time.md);
  pass `cols` only for additional non-spatial angular columns.
  Positional callers need to swap their arguments.

- The print summary is driven by `variables_what` and `variables_when`
  rather than hard-coded column names
  ([\#51](https://github.com/animovement/aniframe/issues/51)). Custom
  identity and temporal variables such as `track` and `model` now
  appear, rows are omitted when their column is absent, and single-track
  readers no longer emit “Unknown or uninitialised column:
  `individual`”.

- The metadata print renders as a single block with field names and
  values aligned in fixed-width columns
  ([\#48](https://github.com/animovement/aniframe/issues/48)).

- The `filename` metadata field accepts a character vector of length one
  or more, for readers that load from multiple source files
  ([\#34](https://github.com/animovement/aniframe/issues/34)).

- Renamed the `point_of_reference` metadata field to `origin`, with
  permitted values `"bottom_left"` and `"top_left"`.

### Deprecated

- `point_of_reference` as a metadata field name.
  [`set_metadata()`](https://animovement.dev/aniframe/reference/set_metadata.md)
  still accepts it, with a warning.

### Fixed

- [`as_aniframe()`](https://animovement.dev/aniframe/reference/as_aniframe.md)
  no longer mis-classifies cylindrical (`rho`, `phi`, `z`) and spherical
  (`rho`, `phi`, `theta`) data as Cartesian
  ([\#44](https://github.com/animovement/aniframe/issues/44)). Detection
  recognises the `rho` + `phi` signature first, so cylindrical data is
  no longer reduced to `cartesian_1d` by its `z` column. Cylindrical
  spatial columns are now ordered `rho`, `phi`, `z`
  ([\#43](https://github.com/animovement/aniframe/issues/43)).

## aniframe 0.4.1

### Fixed

- Corrected metadata written by
  [`as_aniframe()`](https://animovement.dev/aniframe/reference/as_aniframe.md).

## aniframe 0.4.0

### Added

- `variables_what`, `variables_when` and `variables_where` arguments to
  [`as_aniframe()`](https://animovement.dev/aniframe/reference/as_aniframe.md)
  and
  [`example_aniframe()`](https://animovement.dev/aniframe/reference/example_aniframe.md),
  written into the frame’s metadata. These declare which columns carry
  identity, temporal position and spatial position, and are the basis
  for how the frame is typed, ordered and grouped.

### Changed

- Identity and temporal variables are coerced to integer, with the
  exception of `time`, which stays numeric.
- `time` is required. A frame without it is no longer a valid aniframe.
- An unrecognised set of spatial columns is accepted, with
  `coordinate_system` recorded as `"unknown"`, rather than refused.

## aniframe 0.3.5

### Added

- A `NEWS.md` file, to track changes to the package.
- Smaller units: `ns` (nanosecond), `us` (microsecond), `nm` (nanometre)
  and `um` (micrometre).

### Removed

- `get_trackball_calibration_factor()`, following the move of trackball
  handling to aniread.

## aniframe 0.3.4

### Removed

- Trackball calibration. It reads hardware output rather than describing
  a frame, and belongs with the readers in aniread.

## aniframe 0.3.3

### Fixed

- `NA` and `NaN` handling in metadata and coercion.
- An `NA` datetime is no longer given a class, which had made empty
  `start_datetime` values print oddly.

## aniframe 0.3.2

### Changed

- `"cartesian"` is no longer a permitted `coordinate_system` value; the
  dimensioned forms `cartesian_1d`, `cartesian_2d` and `cartesian_3d`
  replace it.
- Metadata printing and the `start_datetime` class were tidied.

## aniframe 0.3.1

### Removed

- `add_centroid()`. Deriving a centroid is a metric rather than a
  property of the frame, and belongs in animetric.

## aniframe 0.3.0

Spatial transformations leave aniframe for
[anispace](https://github.com/animovement/anispace). aniframe keeps the
coordinate *system* — what a frame is in, and how to test it — while
converting between systems becomes anispace’s job.

### Added

- [`ensure_is_cartesian()`](https://animovement.dev/aniframe/reference/ensure_is_cartesian.md),
  with `_1d()`, `_2d()` and `_3d()` variants, and
  [`ensure_is_polar()`](https://animovement.dev/aniframe/reference/ensure_is_polar.md),
  [`ensure_is_cylindrical()`](https://animovement.dev/aniframe/reference/ensure_is_cylindrical.md)
  and
  [`ensure_is_spherical()`](https://animovement.dev/aniframe/reference/ensure_is_spherical.md)
  — guards to sit at the top of a function that requires a particular
  coordinate system.
- [`convert_nan_to_na()`](https://animovement.dev/aniframe/reference/convert_nan_to_na.md)
  is exported.

### Removed

- The coordinate transformations `map_to_cartesian()`, `map_to_polar()`,
  `map_to_cylindrical()` and `map_to_spherical()`, the component
  converters `cartesian_to_rho()`, `cartesian_to_phi()`,
  `cartesian_to_theta()`, `polar_to_x()`, `polar_to_y()` and
  `spherical_to_z()`, the rigid transforms `rotate_coords()`,
  `translate_coords()` and `transform_to_egocentric()`, and the angle
  helpers `wrap_angle()`, `unwrap_angle()`, `diff_angle()` and
  `calculate_angular_difference()`. All are available from anispace.

## aniframe 0.2.5

### Fixed

- `map_to_cartesian()` no longer adds a `z` column when converting from
  polar coordinates.

## aniframe 0.2.4

### Changed

- [`set_metadata()`](https://animovement.dev/aniframe/reference/set_metadata.md)
  accepts a partial metadata list, rather than requiring every field.

## aniframe 0.2.3

### Added

- [`is_cartesian()`](https://animovement.dev/aniframe/reference/is_cartesian.md),
  with `_1d()`, `_2d()` and `_3d()` variants, and
  [`is_polar()`](https://animovement.dev/aniframe/reference/is_polar.md),
  [`is_cylindrical()`](https://animovement.dev/aniframe/reference/is_cylindrical.md)
  and
  [`is_spherical()`](https://animovement.dev/aniframe/reference/is_spherical.md)
  to test a frame’s coordinate system.

### Changed

- `model` is recognised as an identity column, alongside `individual`
  and `keypoint`.

## aniframe 0.2.2

### Added

- `unwrap_angle()`, the counterpart to `wrap_angle()`.

### Changed

- `constrain_angles_radians()` is renamed `wrap_angle()`.

## aniframe 0.2.1

### Added

- Unit handling:
  [`set_unit_space()`](https://animovement.dev/aniframe/reference/set_unit_space.md),
  [`set_unit_angle()`](https://animovement.dev/aniframe/reference/set_unit_angle.md),
  [`set_unit_time()`](https://animovement.dev/aniframe/reference/set_unit_time.md)
  and
  [`set_sampling_rate()`](https://animovement.dev/aniframe/reference/set_sampling_rate.md).
- Coordinate transformations: `map_to_cartesian()`, `map_to_polar()`,
  `map_to_cylindrical()` and `map_to_spherical()`, with the component
  converters `cartesian_to_rho()`, `cartesian_to_phi()`,
  `cartesian_to_theta()`, `polar_to_x()`, `polar_to_y()` and
  `spherical_to_z()`.
- Rigid transformations: `rotate_coords()`, `translate_coords()` and
  `transform_to_egocentric()`.
- Angle handling:
  [`deg_to_rad()`](https://animovement.dev/aniframe/reference/deg_to_rad.md),
  [`rad_to_deg()`](https://animovement.dev/aniframe/reference/rad_to_deg.md),
  `constrain_angles_radians()`, `calculate_angular_difference()` and
  `diff_angle()`.
- [`ensure_is_aniframe()`](https://animovement.dev/aniframe/reference/ensure_is_aniframe.md),
  a guard for functions that require an aniframe.
- Trackball calibration, via `get_trackball_calibration_factor()`.

## aniframe 0.2.0 (2025-10-23)

### Changed

- [`tbl_sum.aniframe()`](https://animovement.dev/aniframe/reference/tbl_sum.aniframe.md)
  is registered as an S3 method rather than exported.

## aniframe 0.1.0 (2025-10-13)

First release. aniframe provides the core data structure for the
animovement suite: a tibble subclass carrying metadata that says which
columns hold identity, time and position.

### Added

- [`aniframe()`](https://animovement.dev/aniframe/reference/aniframe.md)
  and
  [`as_aniframe()`](https://animovement.dev/aniframe/reference/as_aniframe.md)
  to construct a frame,
  [`is_aniframe()`](https://animovement.dev/aniframe/reference/is_aniframe.md)
  to test one, and
  [`example_aniframe()`](https://animovement.dev/aniframe/reference/example_aniframe.md)
  to generate one.
- [`get_metadata()`](https://animovement.dev/aniframe/reference/get_metadata.md),
  [`set_metadata()`](https://animovement.dev/aniframe/reference/set_metadata.md)
  and
  [`default_metadata()`](https://animovement.dev/aniframe/reference/default_metadata.md)
  to read and write the metadata a frame carries.
