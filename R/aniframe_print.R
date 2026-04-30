#' Custom tibble summary for aniframe
#'
#' @description
#' Builds the print header rows shown above an aniframe. The set of rows is
#' driven by the metadata: one row per column listed in `variables_what` and
#' one row per column in `variables_when` (excluding `time`). This means custom
#' identity/temporal variables (e.g. `track`, `model`, `session`) appear
#' automatically, and rows are omitted entirely when their column is absent.
#'
#' @param x An aniframe object
#' @param ... Additional arguments (unused)
#' @return Named character vector with summary information
#' @importFrom pillar tbl_sum
#' @export
tbl_sum.aniframe <- function(x, ...) {
  default_header <- NextMethod()

  md <- get_metadata(x)
  new_header <- character()

  identity_vars <- intersect(md$variables_what, names(x))
  for (col in identity_vars) {
    new_header <- c(
      new_header,
      stats::setNames(
        paste(unique(x[[col]]), collapse = ", "),
        title_case_pluralised(col)
      )
    )
  }

  temporal_vars <- intersect(setdiff(md$variables_when, "time"), names(x))
  for (col in temporal_vars) {
    new_header <- c(
      new_header,
      stats::setNames(
        paste(unique(x[[col]]), collapse = ", "),
        title_case_pluralised(col)
      )
    )
  }

  sampling_rate <- md$sampling_rate
  if (!is.null(sampling_rate) && !is.na(sampling_rate)) {
    new_header <- c(new_header, "Sampling rate" = paste(sampling_rate, "Hz"))
  }

  interval_row <- format_time_interval(x, md)
  if (!is.null(interval_row)) {
    new_header <- c(new_header, interval_row)
  }

  new_header
}
