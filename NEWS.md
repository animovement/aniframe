# aniframe (development version)

* Added a `connections` metadata field for skeletons and other variable-level networks. Stored as a named list keyed by the relevant identity or temporal variable (typically `keypoint`, but also `individual` for social networks etc.), with each entry being a 2-column `from`/`to` tibble. Order is preserved so downstream code can interpret the table as either directed or undirected. Manage with the new exported functions `set_connections()`, `get_connections()`, `add_connections()` and `remove_connections()`. Endpoints not found in the corresponding column emit a warning (typo-catcher) but are still kept (#6).
* Internal: `set_metadata()` now replaces list-valued fields top-level rather than letting `utils::modifyList()` recurse into them. This makes list-of-data-frames fields like `connections` round-trip correctly through `set_metadata()` without attempting to merge tibbles row-wise.

* `print.aniframe_metadata()` now renders as a single block — captured via `cli::cli_format_method()` and emitted with `cat()` — so there is no leading newline and no blank lines between entries. This makes the output render cleanly in HTML contexts such as Quarto / R Markdown documents (#48).

* `set_unit_angle()` now automatically converts the spatial angular columns `phi` and `theta` whenever they are present in the data, so polar / cylindrical / spherical coordinates stay consistent with the declared `unit_angle`. Previously these columns were assumed to be in radians and were not affected by `set_unit_angle()`. The argument order is also rearranged to `set_unit_angle(data, to_unit, cols = NULL)` (matching `set_unit_time()`), and `cols` is now optional — pass it only for additional non-spatial angular columns (#21).
* The `filename` metadata field now explicitly supports a character vector of length >= 1, for readers that load from multiple source files (e.g. `aniread::read_trackball()`). The behaviour was already permitted by the metadata validator; this clarifies the contract in documentation and adds a regression test (#34).

* Fixed `as_aniframe()` mis-classifying cylindrical (`rho`, `phi`, `z`) and spherical (`rho`, `phi`, `theta`) data as Cartesian (#44). The auto-detection now recognises the `rho` + `phi` signature first, so cylindrical data is no longer reduced to `cartesian_1d` because of its `z` column. As a side effect, cylindrical spatial columns are now ordered `rho, phi, z` rather than `z` ending up before `rho`/`phi` (#43).
* `tbl_sum.aniframe()` (the print summary) is now driven by the `variables_what` and `variables_when` metadata fields rather than hard-coding `individual`/`keypoint`/`session`/`trial`. Custom identity and temporal variables (e.g. `track`, `model`) appear automatically, and rows are omitted when their column is absent — fixing the "Unknown or uninitialised column: `individual`" warning emitted by single-track readers (#51).
* Added a "Time" row to the print summary showing the tracked interval as `HH:MM:SS to HH:MM:SS`. When `start_datetime` metadata is set, absolute datetimes are shown instead. Sub-second runs are formatted with millisecond precision (`HH:MM:SS.fff`). The row is omitted when the interval cannot be expressed in seconds (e.g. `unit_time = "frame"` without `sampling_rate`, or `unit_time = "unknown"`) (#50).
* Internal: rename validators to follow the codebase's `check_/ensure_` and `is_/ensure_is_` conventions: `validate_metadata` → `ensure_valid_metadata`, `validate_aniframe_cols` → `ensure_aniframe_cols`, `check_is_list` → `is_list`. All three are internal — no user-facing change.
* Added `set_origin()` to convert between `bottom_left` and `top_left` coordinate origin conventions, reflecting y coordinates around the recorded frame height (#52).
* Added `set_y_height()` to set the y-axis frame height used by `set_origin()`, with validation against the data range.
* Added `y_height` metadata field. Reader functions (in `aniread`) populate it from the source; `as_aniframe()` falls back to `max(y)` when not supplied. Existing values are never overwritten — use `set_y_height()` to change them.
* Renamed metadata field `point_of_reference` to `origin` and locked its permitted values to `c("bottom_left", "top_left")`. The old name is still accepted by `set_metadata()` for backwards compatibility, with a deprecation warning.

# aniframe 0.4.0

* Adopt tidy movement data logic, using what, when and where variables. This adds `variables_what`, `variables_when` and `variables_where` arguments to `as_aniframe` and `example_aniframe`. These are written into the *aniframe*'s metadata.

# aniframe 0.3.5

* Added a `NEWS.md` file to track changes to the package.
* Added smaller units `ns` (nanosecond), `us` (microsecond), `nm` (nanometer) and `um` (micrometer/micron).
