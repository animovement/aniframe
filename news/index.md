# Changelog

## aniframe (development version)

### New features

- Added dedicated setters for the variable roles:
  [`set_variables_what()`](http://animovement.dev/aniframe/reference/variables.md)
  / `when` / `where`, with matching `get_`, `add_` and `remove_` verbs,
  following the `connections` family
  ([\#82](https://github.com/animovement/aniframe/issues/82)). These
  declare the role *and* restructure the frame to match — validating
  that the columns exist, coercing their types, relocating, reordering,
  regrouping, and refreshing `coordinate_system` — so the frame and its
  own description cannot drift apart. `add_variables_*()` appends to the
  declaration, so adding one identity column no longer means restating
  the others;
  [`add_variables_when()`](http://animovement.dev/aniframe/reference/variables.md)
  keeps `time` last, since rows sort by the coarser temporal context
  first. They work on both `aniframe` and `anievent`, except
  [`set_variables_where()`](http://animovement.dev/aniframe/reference/variables.md),
  which an anievent refuses.

- Added
  [`validate_aniframe()`](http://animovement.dev/aniframe/reference/validate_aniframe.md),
  which re-checks that an aniframe’s metadata still describes the frame
  it is attached to
  ([\#79](https://github.com/animovement/aniframe/issues/79)). Every
  column named in `variables_what`, `variables_when`, `variables_where`
  and `variables_event` must be present, `time` must be present and
  numeric, and the `variables_where` columns must be numeric — all hard
  errors. A `coordinate_system` that no longer matches `variables_where`
  is a warning, since the field is derived rather than declared.
  Counterpart to the existing
  [`validate_anievent()`](http://animovement.dev/aniframe/reference/validate_anievent.md).

- Added
  [`is_spatial()`](http://animovement.dev/aniframe/reference/is_spatial.md)
  and
  [`ensure_is_spatial()`](http://animovement.dev/aniframe/reference/ensure_is_spatial.md),
  the spatial subset of those checks: the columns named in
  `variables_where` must be present and numeric
  ([\#79](https://github.com/animovement/aniframe/issues/79)). This is a
  different question from the one
  [`is_cartesian()`](http://animovement.dev/aniframe/reference/is_cartesian.md)
  and its siblings answer — those test for the presence of column
  *names* and never consult the metadata or the column types, so a frame
  that has lost its `x` column still passes
  [`is_cartesian_1d()`](http://animovement.dev/aniframe/reference/is_cartesian_1d.md)
  on the strength of `y` alone. Downstream packages that reach
  coordinates by iterating `variables_where` (`aniprocess` carries a
  local copy for its filters) can use these instead of writing their
  own.

### Improvements

- `aniframe` and `anievent` now recognise the same identity variables:
  `model`, `individual`, `subject`, `track`, `keypoint`, ordered coarse
  to fine ([\#77](https://github.com/animovement/aniframe/issues/77)).
  The two lists had diverged — `aniframe` knew `keypoint` but not
  `subject`, `anievent` the reverse — so identity meant something
  slightly different on each side. `subject` and `individual` name the
  same kind of thing; both are recognised because behavioural-coding
  tools speak of subjects where tracking tools speak of individuals.
- The identity rule is now stated where it is enforced
  ([\#77](https://github.com/animovement/aniframe/issues/77)). An
  aniframe needs **at least one identity (`what`) variable**;
  [`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md)
  guarantees that through a new internal
  [`ensure_identity()`](http://animovement.dev/aniframe/reference/ensure_identity.md),
  and the recognised identity names live in one place
  ([`recognised_variables_what()`](http://animovement.dev/aniframe/reference/recognised_variables_what.md))
  rather than inline. The documentation of
  [`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md),
  [`aniframe()`](http://animovement.dev/aniframe/reference/aniframe.md)
  and
  [`default_metadata()`](http://animovement.dev/aniframe/reference/default_metadata.md)
  no longer describes `c("individual", "keypoint")` as *the default* —
  identity columns are detected from the data, and `variables_what` in
  [`default_metadata()`](http://animovement.dev/aniframe/reference/default_metadata.md)
  is a placeholder that every constructor overwrites. Declaring
  `variables_what = character(0)` remains a supported opt-out for data
  with no identity at all.

### Bug fixes

- Subclasses of `aniframe` and `anievent` now survive the
  class-preserving methods
  ([\#81](https://github.com/animovement/aniframe/issues/81)). dplyr
  strips the whole animovement family before
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) returns, and
  the methods previously rebuilt a fixed `aniframe` / `anievent` — so a
  downstream subclass such as `animetric`’s `aniframe_kin` was dropped
  by the first [`filter()`](https://rdrr.io/r/stats/filter.html),
  `mutate()` or `[`. The incoming class vector is now captured before
  dispatch and restored afterwards, in its original order, so subclasses
  keep dispatch priority over their parent. Downstream packages need not
  register methods of their own. Verbs that aren’t enumerated here
  (`distinct()`, `rowwise()`, the `*_join()` family, `bind_rows()`)
  still drop the class, as they did before.

### Breaking changes

- [`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
  now refuses `variables_what`, `variables_when` and `variables_where`,
  pointing at the setters above
  ([\#82](https://github.com/animovement/aniframe/issues/82)). Writing
  them through
  [`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
  updated the metadata and nothing else: the print header changed, so it
  looked like it had worked, while the frame kept its old column types,
  order and — most consequentially — its old grouping. With several
  trials bound together, anything relying on that grouping integrated
  straight across the boundary between individuals, producing a spurious
  jump at each join with no warning.
  [`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
  now means “write metadata, change nothing else”, which is what makes
  it safe to use everywhere else.
- [`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md)
  no longer adds a `keypoint = "centroid"` column to data that already
  has an identity column
  ([\#77](https://github.com/animovement/aniframe/issues/77)). It
  previously added one whenever `keypoint` was absent, so a frame
  carrying `individual` gained a constant `keypoint` alongside it.
  Results are unaffected — grouping by a constant column changes nothing
  — but the column no longer appears in the frame, the print header, or
  the output of
  [`to_anievent()`](http://animovement.dev/aniframe/reference/to_anievent.md).
  Data with no recognised identity column at all still gets the injected
  `keypoint`, as before.
- [`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md)
  now errors when `variables_what` names a column that is not in the
  data, matching the existing behaviour for `variables_when` and
  `variables_where`. Metadata that names absent columns was previously
  accepted and stored, leaving the frame described by columns it did not
  have.

### Internal

- The tail of
  [`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md)
  and
  [`as_anievent()`](http://animovement.dev/aniframe/reference/as_anievent.md)
  — validate, standardise types, relocate, arrange, regroup, refresh
  derived fields — is factored into
  [`restructure_aniframe()`](http://animovement.dev/aniframe/reference/restructure_aniframe.md)
  /
  [`restructure_anievent()`](http://animovement.dev/aniframe/reference/restructure_anievent.md),
  shared with the new setters, so construction and re-declaration cannot
  diverge.
- [`write_metadata()`](http://animovement.dev/aniframe/reference/write_metadata.md)
  is the internal write path that validates a complete metadata list and
  attaches it, without the field-level policy
  [`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
  applies. The constructors, the variable setters and the
  class-preserving methods use it, since all three legitimately write
  structural fields.
- [`preserve_animovement_class()`](http://animovement.dev/aniframe/reference/preserve_animovement_class.md)
  takes the input’s class vector rather than a constructor, and restores
  only the animovement layer — the `grouped_df` / tibble tail is left to
  dplyr, which has already set it correctly on the result.
- Documentation regenerated with roxygen2 8.1.0, which restyles the
  `importFrom` block in `NAMESPACE`.

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
