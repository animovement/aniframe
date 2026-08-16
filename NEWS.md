# aniframe (development version)

# aniframe 0.6.0 (2026-06-24)

## New features

* Added the `anievent` class for behavioural events in long format — one row per bout (state event) or instant (point event). A sibling of `aniframe`: it shares the metadata substrate but does not inherit from it. Required columns are `channel`, `type`, `label`, `start` and `stop`, with identity columns travelling via `variables_what` and an optional `modifiers` list-column. `type` is derived per `(channel, label)` group at construction — a group is `"point"` only when all its bouts are instantaneous — and can be set explicitly where that misclassifies (#67).
* New API around the class: `anievent()` and `as_anievent()` to construct, `is_anievent()` / `ensure_is_anievent()` to test, and `validate_anievent()` to re-check structural invariants on demand. Class-preserving dplyr and base-R methods are registered so the class survives tidyverse pipelines (#68).
* Added `to_anievent()`, which run-length-encodes per-frame data into bouts — as distinct from `as_anievent()`, which casts data that is already bout-shaped. Methods for `data.frame` (tidyselect the event columns) and `aniframe` (read them from `variables_event`); the latter auto-detects each channel's identity scope, so a label constant across keypoints doesn't produce a duplicate bout per keypoint.
* Added a `variables_event` metadata field — a named list `list(state, point)` declaring which `aniframe` columns hold per-frame event labels. State columns are interval-valued, point columns instantaneous; both surface in the print header when populated (#66).
* Added a `spec_version` metadata field, keyed by class, so each class's data contract can evolve independently of the package version. Older serialised objects without it continue to validate (#65).
* `set_unit_time()` and `set_sampling_rate()` are now S3 generics with `aniframe` and `anievent` methods. On an anievent the calibration factor applies to `start` and `stop` rather than `time`; the rest of the contract is identical.
* `as_aniframe()` now auto-detects `observation` as a temporal grouping column, alongside `session` and `trial` — groundwork for BORIS data, where each observation has its own time origin.
* `validate_anievent()` now warns when two bouts of the same `channel` overlap within a group. A warning rather than an error: overlap is normal BORIS output and the long format handles it natively.

## Improvements

* The `print.aniframe_metadata()` heading now reads "animovement metadata", since the substrate is shared by both classes. The S3 class name is unchanged, for backwards compatibility with serialised objects (#69).
* `get_metadata()`, `set_metadata()` and `default_metadata()` documentation generalised to cover both `aniframe` and `anievent` (#69).
* `set_metadata()` now accepts partial `variables_event` input — supplying only `state` or only `point` is fine, and `NA` or empty entries read as "none" rather than erroring (#76).

## Documentation

* New pkgdown article "The anievent data structure", covering channels, state vs point events, modifiers, validation and multi-observation data (#70).
* New pkgdown reference section indexing the user-facing anievent API. The class-preserving S3 methods are marked `@keywords internal` — still exported and dispatched — so they don't clutter the index, matching the tibble convention.

## Internal

* Factored the strip-class / `NextMethod()` / rebuild / re-attach pattern shared by `aniframe_methods.R` and `anievent_methods.R` into `preserve_animovement_class()`.
* `resolve_unit_time_calibration()` factors out the shared logic between the two `set_unit_time()` methods.
* Test coverage at 100% (876 tests).

# aniframe 0.5.0 (2026-05-04)

## New features

* Added `set_origin()` to convert between `bottom_left` and `top_left` coordinate origin conventions, reflecting `y` around the recorded frame height (#52).
* Added `set_y_height()` for setting the y-axis frame height used by `set_origin()`, with validation against the data range.
* Added a `y_height` metadata field. Reader functions (in `aniread`) populate it from the source; `as_aniframe()` falls back to `max(y)` when missing. Existing values are never overwritten — use `set_y_height()` to change them.
* Added a `connections` metadata field for skeletons and other variable-level networks (#6). Stored as a named list keyed by the relevant identity or temporal variable (typically `keypoint`, but also `individual` for social networks). Each entry is a 2-column `from`/`to` tibble; the order is preserved so downstream code can treat the table as either directed or undirected. Manage with the new exported functions `set_connections()`, `get_connections()`, `add_connections()`, `remove_connections()`. Endpoints not found in the corresponding column emit a warning (typo-catcher) but are still kept.
* Added a "Time" row to the print summary showing the tracked interval as `HH:MM:SS to HH:MM:SS`, or as absolute datetimes when `start_datetime` is set in metadata. Sub-second runs use millisecond precision (`HH:MM:SS.fff`). The row is omitted when the interval cannot be expressed in seconds (#50).

## Improvements

* `set_unit_angle()` now automatically converts the spatial angular columns `phi` and `theta` whenever they are present, so polar / cylindrical / spherical coordinates stay consistent with the declared `unit_angle`. Previously these columns were assumed to be in radians and were not affected by `set_unit_angle()`. The argument order is also rearranged to `set_unit_angle(data, to_unit, cols = NULL)` (matching `set_unit_time()`), and `cols` is now optional — pass it only for additional non-spatial angular columns (#21).
* `tbl_sum.aniframe()` (the print summary) is now driven by the `variables_what` and `variables_when` metadata fields rather than hard-coding `individual` / `keypoint` / `session` / `trial`. Custom identity and temporal variables (e.g. `track`, `model`) appear automatically, and rows are omitted when their column is absent — fixing the "Unknown or uninitialised column: `individual`" warning emitted by single-track readers (#51).
* `print.aniframe_metadata()` renders as a single block (no leading newline, no blank lines between entries), and field names and types are now padded to fixed widths so values line up vertically (similar to `str()`). The `[levels: ...]` line for factor fields is indented to match the value column (#48).
* The `filename` metadata field now explicitly supports a character vector of length >= 1, for readers that load from multiple source files (e.g. `aniread::read_trackball()`) (#34).

## Bug fixes

* Fixed `as_aniframe()` mis-classifying cylindrical (`rho`, `phi`, `z`) and spherical (`rho`, `phi`, `theta`) data as Cartesian (#44). The auto-detection now recognises the `rho` + `phi` signature first, so cylindrical data is no longer reduced to `cartesian_1d` because of its `z` column. As a side effect, cylindrical spatial columns are now ordered `rho, phi, z` rather than `z` ending up before `rho` / `phi` (#43).

## Breaking changes

* Renamed the `point_of_reference` metadata field to `origin` and locked its permitted values to `c("bottom_left", "top_left")`. The old name is still accepted by `set_metadata()` for backwards compatibility, with a deprecation warning.
* `set_unit_angle()` argument order changed from `(data, cols, to_unit)` to `(data, to_unit, cols = NULL)` — non-breaking for callers using named arguments (which all existing examples do); positional callers will need to swap.

## Documentation

* New pkgdown articles introducing the `aniframe` data structure: "The aniframe data structure", "Metadata on an aniframe", and "Connections", available under **Articles** on the package website.
* `set_origin()` and `set_y_height()` added to the pkgdown reference index.

## Internal

* `set_metadata()` now replaces list-valued fields top-level rather than letting `utils::modifyList()` recurse into them, so list-of-data-frames fields like `connections` round-trip correctly without attempting to merge tibbles row-wise.
* Renamed validators to follow the codebase's `check_/ensure_` and `is_/ensure_is_` conventions: `validate_metadata` → `ensure_valid_metadata`, `validate_aniframe_cols` → `ensure_aniframe_cols`, `check_is_list` → `is_list`. All three are internal — no user-facing change.
* Added `covr`, `pkgdown`, and `quarto` to CI workflow dependencies.
* Test coverage at 100% (586 tests).

# aniframe 0.4.0

* Adopt tidy movement data logic, using what, when and where variables. This adds `variables_what`, `variables_when` and `variables_where` arguments to `as_aniframe` and `example_aniframe`. These are written into the *aniframe*'s metadata.

# aniframe 0.3.5

* Added a `NEWS.md` file to track changes to the package.
* Added smaller units `ns` (nanosecond), `us` (microsecond), `nm` (nanometer) and `um` (micrometer/micron).
