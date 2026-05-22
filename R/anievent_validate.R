#' Validate an anievent
#'
#' Re-runs the structural invariants of an `anievent` object on demand:
#' * required columns (`channel`, `type`, `label`, `start`,
#'   `stop`) are present with the expected types; identity columns
#'   travel via `variables_what` and are not part of the required set;
#' * `type` is a factor with levels `c("state", "point")`,
#'   classifying each row as either a state event (durative) or a
#'   point event (instant);
#' * `stop >= start` for every row;
#' * `modifiers`, if present, is a list-column whose cells are
#'   character vectors (the values picked from BORIS modifier sets at
#'   coding time; an empty vector when the event has no modifiers).
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
  if (!is.character(data[["channel"]])) {
    cli::cli_abort("{.field channel} must be character.")
  }
  if (!is.factor(data[["type"]])) {
    cli::cli_abort(
      "{.field type} must be a factor with levels {.val state} and {.val point}."
    )
  }
  if (!identical(levels(data[["type"]]), c("state", "point"))) {
    cli::cli_abort(
      "{.field type} must have levels exactly {.val state} and {.val point}."
    )
  }
  if (!is.factor(data[["label"]])) {
    cli::cli_abort("{.field label} must be a factor.")
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
    if (!(is.character(cell) || (is.atomic(cell) && length(cell) == 0))) {
      cli::cli_abort(c(
        "Every cell of {.field modifiers} must be a character vector.",
        "x" = "Row {.val {i}} is of type {.cls {class(cell)}}."
      ))
    }
  }
  invisible(TRUE)
}
