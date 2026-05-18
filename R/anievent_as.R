#' Convert a data frame to an anievent
#'
#' Builds an `anievent` from a data frame holding behavioural events in
#' long format (one row per bout or instant). The four mandatory columns
#' are `channel`, `value`, `start`, and `stop`; identity columns travel
#' via `variables_what`. An optional `modifiers` list-column may carry
#' per-event modifier values (each cell a character vector, matching the
#' BORIS export format).
#'
#' @param data A data frame with the required columns.
#' @param metadata Optional list of metadata.
#' @param variables_what Character vector of identity columns. When
#'   `NULL` (default), auto-detected from a known list (`model`,
#'   `individual`, `track`, `subject`) — only those present in `data` are
#'   used. Pass an explicit value to use any other column name(s) as
#'   identity. An anievent with no identity column is permitted (e.g. a
#'   single-subject experiment).
#' @param variables_when Character vector of temporal columns. When
#'   `NULL` (default), auto-detected from a known grouping list
#'   (`observation`, `session`, `trial`) and concatenated with the
#'   required temporal endpoints `c("start", "stop")`. Pass explicitly
#'   to use other names for the grouping context.
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
    recognised_when_grouping <- c("observation", "session", "trial")
    detected_when <- recognised_when_grouping[
      recognised_when_grouping %in% names(data)
    ]
    variables_when <- c(detected_when, "start", "stop")
  }

  ensure_anievent_cols(data)
  data <- standardise_anievent_cols(data, variables_what, variables_when)

  present_what <- intersect(variables_what, names(data))
  present_when <- intersect(variables_when, names(data))
  event_cols <- c("channel", "value")
  if ("modifiers" %in% names(data)) {
    event_cols <- c(event_cols, "modifiers")
  }
  standard_cols <- c(present_what, present_when, event_cols)
  other_cols <- setdiff(names(data), standard_cols)
  data <- data[, c(standard_cols, other_cols)]

  present_when_grouping <- setdiff(present_when, c("start", "stop"))
  data <- dplyr::arrange(
    data,
    dplyr::across(dplyr::all_of(c(present_what, present_when_grouping))),
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
  required <- c("channel", "value", "start", "stop")
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
#' Coerces identity and temporal-grouping columns to factor/integer
#' (mirroring the aniframe convention), `channel` to character,
#' `value` to factor, and `start`/`stop` to numeric.
#'
#' @param data Data frame to standardise.
#' @param variables_what Identity variable names.
#' @param variables_when Temporal variable names — grouping columns
#'   (everything except `start`/`stop`) are coerced like identity
#'   columns; `start` and `stop` are forced numeric.
#'
#' @return Data frame with standardised column types.
#' @keywords internal
standardise_anievent_cols <- function(data, variables_what, variables_when) {
  categorical_vars <- c(
    variables_what,
    setdiff(variables_when, c("start", "stop"))
  )
  for (col in categorical_vars) {
    if (col %in% names(data)) {
      if (is.character(data[[col]])) {
        data[[col]] <- factor(data[[col]])
      } else if (is.numeric(data[[col]])) {
        data[[col]] <- as.integer(data[[col]])
      }
    }
  }

  if (!is.character(data[["channel"]])) {
    data[["channel"]] <- as.character(data[["channel"]])
  }
  if (!is.factor(data[["value"]])) {
    data[["value"]] <- factor(data[["value"]])
  }
  data[["start"]] <- as.numeric(data[["start"]])
  data[["stop"]] <- as.numeric(data[["stop"]])

  data
}
