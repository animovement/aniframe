#' Validate an anievent
#'
#' Re-runs the structural invariants of an `anievent` object on demand:
#' * required columns (`channel`, `value`, `start`, `stop`) are present
#'   with the expected types; identity columns travel via
#'   `variables_what` and are not part of the required set;
#' * `stop >= start` for every row;
#' * `modifiers`, if present, is a list-column whose cells are
#'   character vectors (the values picked from BORIS modifier sets at
#'   coding time; an empty vector when the event has no modifiers);
#' * within each `(identity + temporal-grouping)` group, two bouts of
#'   the same `channel` never overlap — channels are mutually
#'   exclusive by definition.
#'
#' Type-linked invariants (which channels are state vs point events;
#' the zero-duration requirement on point events) belong with the
#' aniframe ↔ anievent conversion code and are checked there. The
#' `anievent` class itself stays type-agnostic — `channel` and `value`
#' carry all the information the class needs.
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
  ensure_anievent_channels_disjoint(data)
  invisible(data)
}


#' @keywords internal
ensure_anievent_col_types <- function(data) {
  if (!is.character(data[["channel"]])) {
    cli::cli_abort("{.field channel} must be character.")
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
ensure_anievent_channels_disjoint <- function(data) {
  if (nrow(data) < 2) {
    return(invisible(TRUE))
  }

  md <- get_metadata(data)
  group_cols <- intersect(
    c(md$variables_what, setdiff(md$variables_when, c("start", "stop"))),
    names(data)
  )

  # Work on a bare tibble so dplyr verbs don't trigger the
  # `ungroup.anievent` "use with care" warning during validation.
  bare <- dplyr::as_tibble(data)
  groups <- dplyr::group_by(
    bare,
    dplyr::across(dplyr::all_of(c(group_cols, "channel")))
  )
  for (sub in dplyr::group_split(groups)) {
    if (nrow(sub) < 2) next
    sub <- sub[order(sub$start), ]
    overlaps <- sub$start[-1] < sub$stop[-nrow(sub)]
    if (any(overlaps)) {
      bad_row <- which(overlaps)[1] + 1
      ch <- sub$channel[bad_row]
      cli::cli_abort(c(
        "Two bouts of the same channel overlap for the same subject.",
        "x" = "Channel {.val {ch}} has overlapping bouts at row {.val {bad_row}}.",
        "i" = "Channels are mutually exclusive: only one value can be active at a time per subject."
      ))
    }
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
