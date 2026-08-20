# Changelog

## aniframe (development version)

### Bug fixes

- [`set_unit_space()`](http://animovement.dev/aniframe/reference/set_unit_space.md),
  [`set_unit_angle()`](http://animovement.dev/aniframe/reference/set_unit_angle.md),
  [`set_unit_time()`](http://animovement.dev/aniframe/reference/set_unit_time.md)
  and
  [`set_sampling_rate()`](http://animovement.dev/aniframe/reference/set_sampling_rate.md)
  no longer re-inject a `keypoint` column and overwrite `variables_what`
  with it ([\#96](https://github.com/animovement/aniframe/issues/96)).
  Each ended by casting with
  [`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md),
  which re-ran auto-detection: a frame given a custom identity such as
  `id` had no recognised identity name, so one was injected and the
  declaration replaced — silently regrouping the frame on a constant
  column. The cast was also redundant, since `mutate()` has preserved
  class and metadata since
  [\#81](https://github.com/animovement/aniframe/issues/81).

- [`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md)
  keeps the roles a frame already declares rather than re-deriving them,
  so casting an object that is already an aniframe is no longer
  destructive
  ([\#96](https://github.com/animovement/aniframe/issues/96)). A
  declaration whose columns have since been dropped still falls through
  to detection, so a cast continues to repair a drifted frame.

## aniframe 0.7.0 (2026-08-18)

### Breaking changes

- [`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
  no longer writes the `variables_*` fields — they have dedicated
  setters now
  ([\#82](https://github.com/animovement/aniframe/issues/82)). Writing
  them as plain metadata left the frame typed, ordered and grouped as it
  was before, so anything relying on the grouping silently integrated
  across identities. A complete metadata object can still be restored
  wholesale.
- [`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md)
  no longer adds a `keypoint = "centroid"` column to data that already
  has an identity column
  ([\#77](https://github.com/animovement/aniframe/issues/77)). Results
  are unaffected — the column was constant — but it no longer appears in
  the frame or the print header.
- [`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md)
  errors when `variables_what` names a column that is not in the data,
  as it already did for `variables_when` and `variables_where`
  ([\#77](https://github.com/animovement/aniframe/issues/77)).

### New features

- Dedicated setters for the variable roles:
  [`set_variables_what()`](http://animovement.dev/aniframe/reference/variables.md),
  `_when()`, `_where()` and `_event()`, each with `get_`, `add_` and
  `remove_` verbs
  ([\#82](https://github.com/animovement/aniframe/issues/82)). They
  declare the role *and* restructure the frame to match, so the metadata
  and the frame cannot drift apart. `add_variables_*()` appends, so
  adding one identity column no longer means restating the others.
- [`validate_aniframe()`](http://animovement.dev/aniframe/reference/validate_aniframe.md)
  re-checks that the metadata still describes the frame: every declared
  column present, `time` and the spatial columns numeric
  ([\#79](https://github.com/animovement/aniframe/issues/79)).
  Counterpart to
  [`validate_anievent()`](http://animovement.dev/aniframe/reference/validate_anievent.md).
- [`is_spatial()`](http://animovement.dev/aniframe/reference/is_spatial.md)
  and
  [`ensure_is_spatial()`](http://animovement.dev/aniframe/reference/ensure_is_spatial.md)
  check the columns named in `variables_where`, which the
  `is_cartesian*()` family never did — those look at column names only
  ([\#79](https://github.com/animovement/aniframe/issues/79)).

### Improvements

- `aniframe` and `anievent` now recognise the same identity variables:
  `model`, `individual`, `subject`, `track`, `keypoint`, ordered coarse
  to fine ([\#77](https://github.com/animovement/aniframe/issues/77)).
- The rule that an aniframe needs at least one identity variable is now
  stated where it is enforced, and the documentation no longer describes
  `c("individual", "keypoint")` as *the* default — identity columns are
  detected from the data
  ([\#77](https://github.com/animovement/aniframe/issues/77)).

### Bug fixes

- Downstream subclasses survive the class-preserving methods
  ([\#81](https://github.com/animovement/aniframe/issues/81)).
  `animetric`’s `aniframe_kin` was dropped by the first
  [`filter()`](https://rdrr.io/r/stats/filter.html), `mutate()` or `[`,
  because the methods rebuilt a fixed class instead of restoring the
  incoming one. Verbs that were never covered (`distinct()`, joins,
  `bind_rows()`) still drop it.
- An `anievent` no longer claims spatial properties it cannot have — a
  BORIS export announced `origin: bottom_left`
  ([\#73](https://github.com/animovement/aniframe/issues/73)).
  `unit_angle`, `origin` and `reference_frame` gain a `"none"` level,
  and an anievent is built with the neutral value for each.

### Internal

- `spec_version` moves to `aniframe = "1.1.0"` and `anievent = "0.2.0"`.
- The tail of
  [`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md)
  and
  [`as_anievent()`](http://animovement.dev/aniframe/reference/as_anievent.md)
  is factored into
  [`restructure_aniframe()`](http://animovement.dev/aniframe/reference/restructure_aniframe.md)
  /
  [`restructure_anievent()`](http://animovement.dev/aniframe/reference/restructure_anievent.md),
  shared with the new setters so construction and re-declaration cannot
  diverge.

## aniframe 0.6.0 (2026-06-24)

### New features

- Added the `anievent` class for behavioural events in long format — one
  row per bout (state event) or instant (point event). A sibling of
  `aniframe`: it shares the metadata substrate but does not inherit from
  it. Required columns are `channel`, `type`, `label`, `start` and
  `stop`, with identity columns travelling via `variables_what` and an
  optional `modifiers` list-column. `type` is derived per
  `(channel, label)` group at construction — a group is `"point"` only
  when all its bouts are instantaneous — and can be set explicitly where
  that misclassifies
  ([\#67](https://github.com/animovement/aniframe/issues/67)).
- New API around the class:
  [`anievent()`](http://animovement.dev/aniframe/reference/anievent.md)
  and
  [`as_anievent()`](http://animovement.dev/aniframe/reference/as_anievent.md)
  to construct,
  [`is_anievent()`](http://animovement.dev/aniframe/reference/is_anievent.md)
  /
  [`ensure_is_anievent()`](http://animovement.dev/aniframe/reference/ensure_is_anievent.md)
  to test, and
  [`validate_anievent()`](http://animovement.dev/aniframe/reference/validate_anievent.md)
  to re-check structural invariants on demand. Class-preserving dplyr
  and base-R methods are registered so the class survives tidyverse
  pipelines ([\#68](https://github.com/animovement/aniframe/issues/68)).
- Added
  [`to_anievent()`](http://animovement.dev/aniframe/reference/to_anievent.md),
  which run-length-encodes per-frame data into bouts — as distinct from
  [`as_anievent()`](http://animovement.dev/aniframe/reference/as_anievent.md),
  which casts data that is already bout-shaped. Methods for `data.frame`
  (tidyselect the event columns) and `aniframe` (read them from
  `variables_event`); the latter auto-detects each channel’s identity
  scope, so a label constant across keypoints doesn’t produce a
  duplicate bout per keypoint.
- Added a `variables_event` metadata field — a named list
  `list(state, point)` declaring which `aniframe` columns hold per-frame
  event labels. State columns are interval-valued, point columns
  instantaneous; both surface in the print header when populated
  ([\#66](https://github.com/animovement/aniframe/issues/66)).
- Added a `spec_version` metadata field, keyed by class, so each class’s
  data contract can evolve independently of the package version. Older
  serialised objects without it continue to validate
  ([\#65](https://github.com/animovement/aniframe/issues/65)).
- [`set_unit_time()`](http://animovement.dev/aniframe/reference/set_unit_time.md)
  and
  [`set_sampling_rate()`](http://animovement.dev/aniframe/reference/set_sampling_rate.md)
  are now S3 generics with `aniframe` and `anievent` methods. On an
  anievent the calibration factor applies to `start` and `stop` rather
  than `time`; the rest of the contract is identical.
- [`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md)
  now auto-detects `observation` as a temporal grouping column,
  alongside `session` and `trial` — groundwork for BORIS data, where
  each observation has its own time origin.
- [`validate_anievent()`](http://animovement.dev/aniframe/reference/validate_anievent.md)
  now warns when two bouts of the same `channel` overlap within a group.
  A warning rather than an error: overlap is normal BORIS output and the
  long format handles it natively.

### Improvements

- The
  [`print.aniframe_metadata()`](http://animovement.dev/aniframe/reference/print.aniframe_metadata.md)
  heading now reads “animovement metadata”, since the substrate is
  shared by both classes. The S3 class name is unchanged, for backwards
  compatibility with serialised objects
  ([\#69](https://github.com/animovement/aniframe/issues/69)).
- [`get_metadata()`](http://animovement.dev/aniframe/reference/get_metadata.md),
  [`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
  and
  [`default_metadata()`](http://animovement.dev/aniframe/reference/default_metadata.md)
  documentation generalised to cover both `aniframe` and `anievent`
  ([\#69](https://github.com/animovement/aniframe/issues/69)).
- [`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
  now accepts partial `variables_event` input — supplying only `state`
  or only `point` is fine, and `NA` or empty entries read as “none”
  rather than erroring
  ([\#76](https://github.com/animovement/aniframe/issues/76)).

### Documentation

- New pkgdown article “The anievent data structure”, covering channels,
  state vs point events, modifiers, validation and multi-observation
  data ([\#70](https://github.com/animovement/aniframe/issues/70)).
- New pkgdown reference section indexing the user-facing anievent API.
  The class-preserving S3 methods are marked `@keywords internal` —
  still exported and dispatched — so they don’t clutter the index,
  matching the tibble convention.

### Internal

- Factored the strip-class /
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) / rebuild /
  re-attach pattern shared by `aniframe_methods.R` and
  `anievent_methods.R` into
  [`preserve_animovement_class()`](http://animovement.dev/aniframe/reference/preserve_animovement_class.md).
- [`resolve_unit_time_calibration()`](http://animovement.dev/aniframe/reference/resolve_unit_time_calibration.md)
  factors out the shared logic between the two
  [`set_unit_time()`](http://animovement.dev/aniframe/reference/set_unit_time.md)
  methods.
- Test coverage at 100% (876 tests).

## aniframe 0.5.0 (2026-05-04)

### New features

- Added
  [`set_origin()`](http://animovement.dev/aniframe/reference/set_origin.md)
  to convert between `bottom_left` and `top_left` coordinate origin
  conventions, reflecting `y` around the recorded frame height
  ([\#52](https://github.com/animovement/aniframe/issues/52)).
- Added
  [`set_y_height()`](http://animovement.dev/aniframe/reference/set_y_height.md)
  for setting the y-axis frame height used by
  [`set_origin()`](http://animovement.dev/aniframe/reference/set_origin.md),
  with validation against the data range.
- Added a `y_height` metadata field. Reader functions (in `aniread`)
  populate it from the source;
  [`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md)
  falls back to `max(y)` when missing. Existing values are never
  overwritten — use
  [`set_y_height()`](http://animovement.dev/aniframe/reference/set_y_height.md)
  to change them.
- Added a `connections` metadata field for skeletons and other
  variable-level networks
  ([\#6](https://github.com/animovement/aniframe/issues/6)). Stored as a
  named list keyed by the relevant identity or temporal variable
  (typically `keypoint`, but also `individual` for social networks).
  Each entry is a 2-column `from`/`to` tibble; the order is preserved so
  downstream code can treat the table as either directed or undirected.
  Manage with the new exported functions
  [`set_connections()`](http://animovement.dev/aniframe/reference/set_connections.md),
  [`get_connections()`](http://animovement.dev/aniframe/reference/get_connections.md),
  [`add_connections()`](http://animovement.dev/aniframe/reference/add_connections.md),
  [`remove_connections()`](http://animovement.dev/aniframe/reference/remove_connections.md).
  Endpoints not found in the corresponding column emit a warning
  (typo-catcher) but are still kept.
- Added a “Time” row to the print summary showing the tracked interval
  as `HH:MM:SS to HH:MM:SS`, or as absolute datetimes when
  `start_datetime` is set in metadata. Sub-second runs use millisecond
  precision (`HH:MM:SS.fff`). The row is omitted when the interval
  cannot be expressed in seconds
  ([\#50](https://github.com/animovement/aniframe/issues/50)).

### Improvements

- [`set_unit_angle()`](http://animovement.dev/aniframe/reference/set_unit_angle.md)
  now automatically converts the spatial angular columns `phi` and
  `theta` whenever they are present, so polar / cylindrical / spherical
  coordinates stay consistent with the declared `unit_angle`. Previously
  these columns were assumed to be in radians and were not affected by
  [`set_unit_angle()`](http://animovement.dev/aniframe/reference/set_unit_angle.md).
  The argument order is also rearranged to
  `set_unit_angle(data, to_unit, cols = NULL)` (matching
  [`set_unit_time()`](http://animovement.dev/aniframe/reference/set_unit_time.md)),
  and `cols` is now optional — pass it only for additional non-spatial
  angular columns
  ([\#21](https://github.com/animovement/aniframe/issues/21)).
- [`tbl_sum.aniframe()`](http://animovement.dev/aniframe/reference/tbl_sum.aniframe.md)
  (the print summary) is now driven by the `variables_what` and
  `variables_when` metadata fields rather than hard-coding `individual`
  / `keypoint` / `session` / `trial`. Custom identity and temporal
  variables (e.g. `track`, `model`) appear automatically, and rows are
  omitted when their column is absent — fixing the “Unknown or
  uninitialised column: `individual`” warning emitted by single-track
  readers ([\#51](https://github.com/animovement/aniframe/issues/51)).
- [`print.aniframe_metadata()`](http://animovement.dev/aniframe/reference/print.aniframe_metadata.md)
  renders as a single block (no leading newline, no blank lines between
  entries), and field names and types are now padded to fixed widths so
  values line up vertically (similar to
  [`str()`](https://rdrr.io/r/utils/str.html)). The `[levels: ...]` line
  for factor fields is indented to match the value column
  ([\#48](https://github.com/animovement/aniframe/issues/48)).
- The `filename` metadata field now explicitly supports a character
  vector of length \>= 1, for readers that load from multiple source
  files (e.g. `aniread::read_trackball()`)
  ([\#34](https://github.com/animovement/aniframe/issues/34)).

### Bug fixes

- Fixed
  [`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md)
  mis-classifying cylindrical (`rho`, `phi`, `z`) and spherical (`rho`,
  `phi`, `theta`) data as Cartesian
  ([\#44](https://github.com/animovement/aniframe/issues/44)). The
  auto-detection now recognises the `rho` + `phi` signature first, so
  cylindrical data is no longer reduced to `cartesian_1d` because of its
  `z` column. As a side effect, cylindrical spatial columns are now
  ordered `rho, phi, z` rather than `z` ending up before `rho` / `phi`
  ([\#43](https://github.com/animovement/aniframe/issues/43)).

### Breaking changes

- Renamed the `point_of_reference` metadata field to `origin` and locked
  its permitted values to `c("bottom_left", "top_left")`. The old name
  is still accepted by
  [`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
  for backwards compatibility, with a deprecation warning.
- [`set_unit_angle()`](http://animovement.dev/aniframe/reference/set_unit_angle.md)
  argument order changed from `(data, cols, to_unit)` to
  `(data, to_unit, cols = NULL)` — non-breaking for callers using named
  arguments (which all existing examples do); positional callers will
  need to swap.

### Documentation

- New pkgdown articles introducing the `aniframe` data structure: “The
  aniframe data structure”, “Metadata on an aniframe”, and
  “Connections”, available under **Articles** on the package website.
- [`set_origin()`](http://animovement.dev/aniframe/reference/set_origin.md)
  and
  [`set_y_height()`](http://animovement.dev/aniframe/reference/set_y_height.md)
  added to the pkgdown reference index.

### Internal

- [`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
  now replaces list-valued fields top-level rather than letting
  [`utils::modifyList()`](https://rdrr.io/r/utils/modifyList.html)
  recurse into them, so list-of-data-frames fields like `connections`
  round-trip correctly without attempting to merge tibbles row-wise.
- Renamed validators to follow the codebase’s `check_/ensure_` and
  `is_/ensure_is_` conventions: `validate_metadata` →
  `ensure_valid_metadata`, `validate_aniframe_cols` →
  `ensure_aniframe_cols`, `check_is_list` → `is_list`. All three are
  internal — no user-facing change.
- Added `covr`, `pkgdown`, and `quarto` to CI workflow dependencies.
- Test coverage at 100% (586 tests).

## aniframe 0.4.0

- Adopt tidy movement data logic, using what, when and where variables.
  This adds `variables_what`, `variables_when` and `variables_where`
  arguments to `as_aniframe` and `example_aniframe`. These are written
  into the *aniframe*’s metadata.

## aniframe 0.3.5

- Added a `NEWS.md` file to track changes to the package.
- Added smaller units `ns` (nanosecond), `us` (microsecond), `nm`
  (nanometer) and `um` (micrometer/micron).
