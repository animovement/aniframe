# aniframe (development version)

* `tbl_sum.aniframe()` (the print summary) is now driven by the `variables_what` and `variables_when` metadata fields rather than hard-coding `individual`/`keypoint`/`session`/`trial`. Custom identity and temporal variables (e.g. `track`, `model`) appear automatically, and rows are omitted when their column is absent — fixing the "Unknown or uninitialised column: `individual`" warning emitted by single-track readers (#51).

# aniframe 0.4.0

* Adopt tidy movement data logic, using what, when and where variables. This adds `variables_what`, `variables_when` and `variables_where` arguments to `as_aniframe` and `example_aniframe`. These are written into the *aniframe*'s metadata.

# aniframe 0.3.5

* Added a `NEWS.md` file to track changes to the package.
* Added smaller units `ns` (nanosecond), `us` (microsecond), `nm` (nanometer) and `um` (micrometer/micron).
