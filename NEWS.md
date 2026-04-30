# aniframe (development version)

* Internal: rename validators to follow the codebase's `check_/ensure_` and `is_/ensure_is_` conventions: `validate_metadata` → `ensure_valid_metadata`, `validate_aniframe_cols` → `ensure_aniframe_cols`, `check_is_list` → `is_list`. All three are internal — no user-facing change.

# aniframe 0.4.0

* Adopt tidy movement data logic, using what, when and where variables. This adds `variables_what`, `variables_when` and `variables_where` arguments to `as_aniframe` and `example_aniframe`. These are written into the *aniframe*'s metadata.

# aniframe 0.3.5

* Added a `NEWS.md` file to track changes to the package.
* Added smaller units `ns` (nanosecond), `us` (microsecond), `nm` (nanometer) and `um` (micrometer/micron).
