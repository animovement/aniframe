#' Custom tibble summary for anievent
#'
#' Builds the print header rows shown above an `anievent`. Shows identity
#' columns (`variables_what`), the unique event channels carried by the
#' `channel` column, and the standard sampling-rate row inherited from
#' the metadata substrate.
#'
#' @param x An anievent object.
#' @param ... Additional arguments (unused).
#' @return Named character vector with summary information.
#' @importFrom pillar tbl_sum
#' @keywords internal
#' @export
tbl_sum.anievent <- function(x, ...) {
  default_header <- NextMethod()
  names(default_header)[1] <- "anievent"

  md <- get_metadata(x)
  new_header <- default_header

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

  if ("channel" %in% names(x) && nrow(x) > 0) {
    channels <- unique(x[["channel"]])
    new_header <- c(
      new_header,
      "Event channels" = paste(channels, collapse = ", ")
    )
  }

  sampling_rate <- md$sampling_rate
  if (!is.null(sampling_rate) && !is.na(sampling_rate)) {
    new_header <- c(new_header, "Sampling rate" = paste(sampling_rate, "Hz"))
  }

  new_header
}
