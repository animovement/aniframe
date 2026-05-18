#' Validate an anievent
#'
#' Re-runs the structural invariants of an `anievent` object on demand:
#' * required columns (`variable`, `value`, `start`, `stop`) are present
#'   with the expected types; identity columns travel via
#'   `variables_what` and are not part of the required set;
#' * `stop >= start` for every row;
#' * `modifiers`, if present, is a list-column of named lists (or empty
#'   lists).
#'
#' Type-linked invariants (which `variable` values are state vs point
#' events; the zero-duration requirement on point events) belong with
#' the aniframe ↔ anievent conversion code and are checked there. The
#' `anievent` class itself stays type-agnostic — `variable` and `value`
#' carry the channel information the class needs.
#'
#' Errors are raised through `cli::cli_abort()`; on success the object
#' is returned invisibly.
#'
#' @param data An anievent object.
#'
#' @return The input `data`, invisibly.
#' @export
validate_anievent <- function(data) {
  ensure_is_anievent(data)
  ensure_anievent_cols(data)
  ensure_anievent_col_types(data)
  ensure_anievent_intervals_nonnegative(data)
  ensure_anievent_modifiers_shape(data)
  invisible(data)
}


#' @keywords internal
ensure_anievent_col_types <- function(data) {
  if (!is.character(data[["variable"]])) {
    cli::cli_abort("{.field variable} must be character.")
  }
  if (!is.factor(data[["value"]])) {
    cli::cli_abort("{.field value} must be a factor.")
  }
  if (!is.numeric(data[["start"]])) {
    cli::cli_abort("{.field start} must be numeric.")
  }
  if (!is.numeric(data[["stop"]])) {
    cli::cli_abort("{.field stop} must be numeric.")
  }
  invisible(TRUE)
}


#' @keywords internal
ensure_anievent_intervals_nonnegative <- function(data) {
  diffs <- data[["stop"]] - data[["start"]]
  bad <- which(!is.na(diffs) & diffs < 0)
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "{.field stop} must be greater than or equal to {.field start} for every row.",
      "x" = "Row{?s} violating the invariant: {.val {bad}}."
    ))
  }
  invisible(TRUE)
}


#' @keywords internal
ensure_anievent_modifiers_shape <- function(data) {
  if (!"modifiers" %in% names(data)) {
    return(invisible(TRUE))
  }
  mods <- data[["modifiers"]]
  if (!is.list(mods)) {
    cli::cli_abort("{.field modifiers} must be a list-column.")
  }
  for (i in seq_along(mods)) {
    cell <- mods[[i]]
    if (!is.list(cell)) {
      cli::cli_abort(c(
        "Every cell of {.field modifiers} must be a list.",
        "x" = "Row {.val {i}} is of type {.cls {class(cell)}}."
      ))
    }
    if (length(cell) > 0) {
      nm <- names(cell)
      if (is.null(nm) || any(nm == "" | is.na(nm))) {
        cli::cli_abort(c(
          "Non-empty {.field modifiers} cells must be named lists.",
          "x" = "Row {.val {i}} has unnamed entries."
        ))
      }
    }
  }
  invisible(TRUE)
}


