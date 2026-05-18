#' Convert a data frame to an anievent
#'
#' Builds an `anievent` from a data frame holding behavioural events in
#' long format (one row per bout or instant). The five mandatory columns
#' are `individual`, `variable`, `value`, `start`, and `stop`. An
#' optional `modifiers` list-column may carry per-event attributes (a
#' named list per row).
#'
#' @param data A data frame with the required columns.
#' @param metadata Optional list of metadata.
#' @param variables_what Character vector of identity columns. When
#'   `NULL` (default), auto-detected from a known list (`model`,
#'   `individual`, `track`, `subject`) — only those present in `data` are
#'   used. Pass an explicit value to use any other column name(s) as
#'   identity. An anievent with no identity column is permitted (e.g. a
#'   single-subject experiment).
#' @param variables_when Character vector of temporal columns. Defaults
#'   to `c("start", "stop")`.
#'
#' @return An anievent object.
#' @export
as_anievent <- function(
  data,
  metadata = list(),
  variables_what = NULL,
  variables_when = NULL
) {
  UseMethod("as_anievent")
}

#' @rdname as_anievent
#' @export
as_anievent.anievent <- function(
  data,
  metadata = list(),
  variables_what = NULL,
  variables_when = NULL
) {
  data
}

#' @rdname as_anievent
#' @export
as_anievent.data.frame <- function(
  data,
  metadata = list(),
  variables_what = NULL,
  variables_when = NULL
) {
  if (is.null(variables_what)) {
    recognised_what <- c("model", "individual", "track", "subject")
    variables_what <- recognised_what[recognised_what %in% names(data)]
  }
  if (is.null(variables_when)) {
    variables_when <- c("start", "stop")
  }

  ensure_anievent_cols(data)
  data <- standardise_anievent_cols(data, variables_what)

  present_what <- intersect(variables_what, names(data))
  standard_cols <- c(
    present_what,
    "variable",
    "value",
    "start",
    "stop"
  )
  if ("modifiers" %in% names(data)) {
    standard_cols <- c(standard_cols, "modifiers")
  }
  other_cols <- setdiff(names(data), standard_cols)
  data <- data[, c(standard_cols, other_cols)]

  data <- dplyr::arrange(
    data,
    dplyr::across(dplyr::all_of(present_what)),
    .data$start
  )

  data <- new_anievent(data)

  data <- set_metadata(data, metadata = metadata)
  data <- set_metadata(
    data,
    variables_what = variables_what,
    variables_when = variables_when,
    variables_where = character()
  )

  data
}


#' Validate the structural columns of an anievent
#'
#' @param data Data frame to validate.
#' @keywords internal
ensure_anievent_cols <- function(data) {
  required <- c("variable", "value", "start", "stop")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Missing required column{?s} for an anievent: {.val {missing}}.",
      "i" = "An anievent requires {.val {required}}.",
      "i" = "Identity columns (e.g. {.val individual}) are recognised via {.arg variables_what} but are not required."
    ))
  }
  invisible(TRUE)
}


#' Standardise column types for an anievent
#'
#' Coerces identity columns to factor/integer (mirroring the aniframe
#' convention), `variable` to character, `value` to factor, and
#' `start`/`stop` to numeric.
#'
#' @param data Data frame to standardise.
#' @param variables_what Identity variable names.
#'
#' @return Data frame with standardised column types.
#' @keywords internal
standardise_anievent_cols <- function(data, variables_what) {
  for (col in variables_what) {
    if (col %in% names(data)) {
      if (is.character(data[[col]])) {
        data[[col]] <- factor(data[[col]])
      } else if (is.numeric(data[[col]])) {
        data[[col]] <- as.integer(data[[col]])
      }
    }
  }

  if (!is.character(data[["variable"]])) {
    data[["variable"]] <- as.character(data[["variable"]])
  }
  if (!is.factor(data[["value"]])) {
    data[["value"]] <- factor(data[["value"]])
  }
  data[["start"]] <- as.numeric(data[["start"]])
  data[["stop"]] <- as.numeric(data[["stop"]])

  data
}
