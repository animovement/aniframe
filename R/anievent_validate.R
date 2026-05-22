#' Validate an anievent
#'
#' Re-runs the structural invariants of an `anievent` object on demand:
#' * required columns (`channel`, `value`, `start`, `stop`) are present
#'   with the expected types — these are hard errors;
#' * `stop >= start` for every row — hard error;
#' * `modifiers`, if present, is a list-column whose cells are
#'   character vectors — hard error;
#' * within each `(identity + temporal-grouping)` group, two bouts of
#'   the same `channel` should not overlap — **warning** only.
#'   Overlapping bouts within a channel are permitted on the
#'   `anievent` side; this is the form the data takes from coding
#'   tools that allow non-mutually-exclusive coding (e.g. BORIS
#'   without strict ethogram). `add_events()` splits them into
#'   numbered sub-columns at the boundary into `aniframe`, where
#'   mutual exclusion is structurally enforced.
#'
#' @param data An anievent object.
#'
#' @return The input `data`, invisibly.
#' @export
validate_anievent <- function(data) {
  ensure_is_anievent(data)
  ensure_anievent_structural(data)
  warn_anievent_channels_overlap(data)
  invisible(data)
}


#' Structural checks for an anievent (no overlap warning)
#'
#' Bundles the hard checks `validate_anievent()` runs: required
#' columns, column types, non-negative intervals, modifier shape.
#' Callers that already handle channel overlap (e.g. `add_events()`,
#' which splits into numbered sub-columns) call this directly to
#' avoid the redundant overlap warning.
#'
#' @keywords internal
ensure_anievent_structural <- function(data) {
  ensure_anievent_cols(data)
  ensure_anievent_col_types(data)
  ensure_anievent_intervals_nonnegative(data)
  ensure_anievent_modifiers_shape(data)
  invisible(TRUE)
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


#' Find the first overlapping bout pair within any (identity +
#' temporal-grouping + channel) group of an anievent.
#'
#' Returns `NULL` when no overlap exists; otherwise a small named list
#' identifying the channel and offending row.
#'
#' @keywords internal
find_anievent_channel_overlap <- function(data) {
  if (nrow(data) < 2) {
    return(NULL)
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
    if (nrow(sub) < 2) {
      next
    }
    sub <- sub[order(sub$start), ]
    overlaps <- sub$start[-1] < sub$stop[-nrow(sub)]
    if (any(overlaps)) {
      bad_row <- which(overlaps)[1] + 1
      return(list(channel = sub$channel[bad_row], row = bad_row))
    }
  }
  NULL
}


#' @keywords internal
warn_anievent_channels_overlap <- function(data) {
  hit <- find_anievent_channel_overlap(data)
  if (!is.null(hit)) {
    cli::cli_warn(c(
      "Two bouts of the same channel overlap for the same subject.",
      "x" = "Channel {.val {hit$channel}} has overlapping bouts at row {.val {hit$row}}.",
      "i" = "Overlap is permitted on the {.cls anievent}. {.fn add_events} will split into numbered sub-columns when converting to {.cls aniframe}."
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
