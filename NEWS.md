# aniframe 0.6.0 (development version)

## New features

* Added the `anievent` class for behavioural events in long format — one row per bout (state event) or instant (point event). Sibling of `aniframe`: shares the metadata substrate but does not inherit from it. Required columns: `channel`, `value`, `start`, `stop`; identity columns travel via `variables_what`; an optional `modifiers` list-column carries per-event modifier values as flat character vectors (matching the BORIS export format). The class is intentionally type-agnostic — the state-vs-point distinction is only load-bearing at conversion / plotting / metric time and is left to companion packages (#67).
* New exported API around the class: `anievent()` and `as_anievent()` for construction, `is_anievent()` / `ensure_is_anievent()` for predicates, and `validate_anievent()` for re-checking structural invariants on demand. Class-preserving dplyr verbs and base-R extraction / assignment methods are registered (`mutate.anievent`, `filter.anievent`, `[.anievent`, etc.) so the class round-trips through tidyverse pipelines (#68).
* Added a `variables_event` metadata field — a named list `list(state, point)` declaring which `aniframe` columns hold per-frame categorical event labels. State columns are interval-valued (ordered coarse to fine for nesting); point columns are instantaneous. Foundation for downstream conversions; the `aniframe` print header surfaces "State event variables" / "Point event variables" rows when populated (#66).
* Added a `spec_version` metadata field — a named list keyed by class, e.g. `list(aniframe = "1.0.0", anievent = "0.1.0")` — so the data contract of each class can evolve independently of the package version. Older serialised objects missing the field continue to validate (#65).
* `as_aniframe()` now auto-detects `observation` as a temporal grouping column, alongside the existing `session` and `trial`. Lays the groundwork for importing behavioural-event data from BORIS where each observation has its own time origin.
* `set_unit_time()` and `set_sampling_rate()` are now S3 generics with methods for both `aniframe` and `anievent`. On an anievent the calibration factor is applied to `start` and `stop` (instead of `time` on an aniframe); the rest of the contract is identical. Lets anievent data round-trip between frame, millisecond, and SI units the same way aniframe does.
* Added `as_anievent.aniframe()` — converts an `aniframe` with declared `variables_event` columns into an `anievent`. For each column in `variables_event$state`, contiguous runs of the same label are run-length-encoded into bouts within each `(individual, observation, …)` group; each `variables_event$point` column becomes one row per non-`NA` frame with `start == stop`. `unit_time` and `sampling_rate` are inherited from the host.
* Added `add_events()` — host-first verb that joins an `anievent` onto an `aniframe`'s per-frame grid, adding one factor-valued column per channel. State vs point is auto-detected from the bouts (a channel is `point` iff every bout has `start == stop`). Unit reconciliation is automatic for SI ↔ SI; crossing the `"frame"`/SI boundary needs a `sampling_rate` on either side. Channel-name collisions with existing host columns error out; frames outside any bout get `NA`.
* Modifiers round-trip across the two verbs. `add_events()` broadcasts each bout's `modifiers` cell across the bout's frames as a parallel `<channel>_modifiers` list-column on the host (only when a channel has at least one non-empty modifier vector). `as_anievent.aniframe()` reverses the operation: a `<channel>_modifiers` column on the source aniframe is gathered back into the resulting anievent's `modifiers` cells.
* `as_anievent.aniframe()` auto-detects each event channel's "scope" — the minimal subset of `variables_what` the value varies across — so a `behaviour` column constant across `keypoint` no longer produces duplicate bouts per keypoint. Singleton identity columns (a single unique value) are preserved for traceability rather than collapsed away. Temporal-grouping columns (`observation`, `session`, `trial`) are always carried through unconditionally — they represent distinct contexts and must not be merged. Channels with disagreeing scopes (e.g. an individual-level and a keypoint-level channel together) error with a message pointing to `variables_what` as the explicit override.
* `validate_anievent()` now also checks that two bouts of the same `channel` never overlap within the same `(identity + temporal-grouping)` group — that's the structural property defining a channel.

## Improvements

* The `print.aniframe_metadata()` heading now reads "animovement metadata" to reflect that the metadata substrate is shared by both `aniframe` and `anievent`. The S3 class name `aniframe_metadata` is unchanged for backwards compatibility with previously serialised objects (#69).
* `get_metadata()`, `set_metadata()`, and `default_metadata()` documentation generalised — these operate on either `aniframe` or `anievent` objects via the shared metadata substrate (#69).

## Documentation

* New pkgdown article "The anievent data structure" walks through the class, the channel concept (one mutually-exclusive categorical track of behaviour), state vs point events, modifiers, validation, and multi-observation handling (#70).
* New pkgdown reference section "Creating and converting anievent objects" indexes the user-facing anievent API; the class-preserving S3 methods are marked `@keywords internal` (still exported and dispatched) so they don't clutter the reference index, matching the tibble subsetting-family convention.

## Internal

* Factored the strip-class / `NextMethod` / rebuild / re-attach pattern shared by `aniframe_methods.R` and `anievent_methods.R` into `preserve_animovement_class()` in `utils.R`.
* `resolve_unit_time_calibration()` factors out the shared unit-validation and conversion-factor logic between `set_unit_time.aniframe` and `set_unit_time.anievent`.
* Test coverage at 100% (829 tests).

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
