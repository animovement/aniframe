# aniframe (development version)

* Added `set_origin()` to convert between `bottom_left` and `top_left` coordinate origin conventions, reflecting y coordinates around the recorded frame height (#52).
* Added `set_y_height()` to set the y-axis frame height used by `set_origin()`, with validation against the data range.
* Added `y_height` metadata field. Reader functions (in `aniread`) populate it from the source; `as_aniframe()` falls back to `max(y)` when not supplied. Existing values are never overwritten — use `set_y_height()` to change them.
* Renamed metadata field `point_of_reference` to `origin` and locked its permitted values to `c("bottom_left", "top_left")`. The old name is still accepted by `set_metadata()` for backwards compatibility, with a deprecation warning.

# aniframe 0.4.0

* Adopt tidy movement data logic, using what, when and where variables. This adds `variables_what`, `variables_when` and `variables_where` arguments to `as_aniframe` and `example_aniframe`. These are written into the *aniframe*'s metadata.

# aniframe 0.3.5

* Added a `NEWS.md` file to track changes to the package.
* Added smaller units `ns` (nanosecond), `us` (microsecond), `nm` (nanometer) and `um` (micrometer/micron).
