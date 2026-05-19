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
as_anievent.aniframe <- function(
  data,
  metadata = list(),
  variables_what = NULL,
  variables_when = NULL
) {
  md <- get_metadata(data)
  ve <- md$variables_event
  if (is.null(ve) || (length(ve$state) == 0 && length(ve$point) == 0)) {
    cli::cli_abort(c(
      "The host {.cls aniframe} has no event columns declared.",
      "i" = "Populate {.field variables_event$state} and/or {.field variables_event$point} in metadata before conversion."
    ))
  }

  declared <- c(ve$state, ve$point)
  missing_cols <- setdiff(declared, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Some declared event columns are not present in the data: {.val {missing_cols}}.",
      "i" = "Either remove them from {.field variables_event} or add the columns."
    ))
  }

  host_what <- intersect(md$variables_what, names(data))
  grouping_when <- intersect(setdiff(md$variables_when, "time"), names(data))
  candidate_cols <- c(host_what, grouping_when)

  bare <- dplyr::as_tibble(data)
  bare <- dplyr::ungroup(bare)

  # Auto-detect the scope of each event channel — the minimal subset of
  # identity / grouping columns the value actually varies across. A
  # `behaviour` column constant across `keypoint`, for example, drops
  # keypoint from the bout grouping.
  channel_scopes <- list()
  for (col in declared) {
    channel_scopes[[col]] <- detect_event_scope(bare, col, candidate_cols)
  }

  # All channels must agree on scope. Different scopes mean the user is
  # mixing granularities (e.g. an individual-level "behaviour" with a
  # keypoint-level "limb_extended"); ask them to hand-pick
  # `variables_what` or split into multiple anievents.
  unique_scopes <- unique(channel_scopes)
  if (length(unique_scopes) > 1) {
    scope_lines <- vapply(
      names(channel_scopes),
      function(nm) {
        sc <- channel_scopes[[nm]]
        sc_str <- if (length(sc) == 0) "<none>" else paste(sc, collapse = ", ")
        paste0(nm, " -> ", sc_str)
      },
      character(1)
    )
    cli::cli_abort(c(
      "Declared event columns disagree on their identity scope.",
      "i" = "Detected scopes per channel:",
      stats::setNames(scope_lines, rep("*", length(scope_lines))),
      "i" = "Pass {.arg variables_what} explicitly or split the channels into separate {.cls anievent}s."
    ))
  }

  detected_scope <- unique_scopes[[1]]
  if (is.null(variables_what)) {
    variables_what <- intersect(detected_scope, host_what)
  }
  grouping_when <- intersect(detected_scope, grouping_when)
  group_cols <- c(variables_what, grouping_when)

  bouts <- list()
  if (length(ve$state) > 0) {
    for (col in ve$state) {
      bouts[[col]] <- rle_state_column(bare, col, group_cols)
    }
  }
  if (length(ve$point) > 0) {
    for (col in ve$point) {
      bouts[[col]] <- pick_point_column(bare, col, group_cols)
    }
  }

  out <- dplyr::bind_rows(bouts)
  # Coerce value back to a factor with the union of levels across channels
  if (!is.factor(out$value)) {
    out$value <- factor(out$value)
  }

  if (is.null(variables_when)) {
    variables_when <- c(grouping_when, "start", "stop")
  }

  inherited_metadata <- md[
    setdiff(
      names(md),
      c(
        "variables_what",
        "variables_when",
        "variables_where",
        "variables_event",
        "spec_version",
        "y_height",
        "origin",
        "coordinate_system",
        "connections"
      )
    )
  ]
  metadata <- utils::modifyList(inherited_metadata, metadata)

  as_anievent(
    out,
    metadata = metadata,
    variables_what = variables_what,
    variables_when = variables_when
  )
}


#' Detect the identity / grouping scope of one event column
#'
#' Returns the minimal subset of `candidate_cols` that the value of
#' `event_col` varies across (given `time`). A column is "redundant"
#' for an event channel when, after removing it from the candidate
#' set, `(remaining + time)` still uniquely determines the event value
#' on every non-`NA` row.
#'
#' Used by `as_anievent.aniframe()` so that, e.g., a `behaviour` column
#' that is constant across keypoints for each `(individual, time)`
#' drops `keypoint` from the resulting `anievent`'s grouping — instead
#' of emitting one identical bout per keypoint.
#'
#' The detection is greedy: at each iteration, drop any column whose
#' removal still leaves the event value uniquely determined; stop when
#' no further removal is possible.
#'
#' @keywords internal
detect_event_scope <- function(data, event_col, candidate_cols) {
  # All-NA channels carry no information; treat their scope as empty.
  if (all(is.na(data[[event_col]]))) {
    return(character())
  }

  scope <- candidate_cols
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (col in scope) {
      smaller <- setdiff(scope, col)
      # Count `NA` as a distinct value: an event present on some
      # identities but absent on others is genuinely identity-scoped.
      grouped <- data |>
        dplyr::group_by(
          dplyr::across(dplyr::all_of(c(smaller, "time")))
        ) |>
        dplyr::summarise(
          n = dplyr::n_distinct(.data[[event_col]]),
          .groups = "drop"
        )
      if (all(grouped$n <= 1)) {
        scope <- smaller
        changed <- TRUE
        break
      }
    }
  }
  scope
}


#' Run-length encode one state event column into bouts
#'
#' Within each `(group_cols)` partition, emit one row per maximal run
#' of identical non-`NA` values in `col`. `start` is the `time` of the
#' first frame in the run; `stop` is the `time` of the last frame.
#'
#' @keywords internal
rle_state_column <- function(data, col, group_cols) {
  data <- data[!is.na(data[[col]]), , drop = FALSE]
  if (nrow(data) == 0) {
    return(make_empty_bout_df(group_cols, col))
  }

  if (length(group_cols) > 0) {
    data <- data[
      do.call(order, c(data[group_cols], list(data$time))),
      ,
      drop = FALSE
    ]
    key <- do.call(paste, c(data[group_cols], list(sep = "\r")))
  } else {
    data <- data[order(data$time), , drop = FALSE]
    key <- rep("", nrow(data))
  }

  vals <- as.character(data[[col]])
  prev_key <- c("", key[-length(key)])
  prev_val <- c("", vals[-length(vals)])
  run_start <- key != prev_key | vals != prev_val
  run_id <- cumsum(run_start)

  first_idx <- !duplicated(run_id)
  last_idx <- !duplicated(run_id, fromLast = TRUE)

  out <- dplyr::tibble(
    channel = col,
    value = vals[first_idx],
    start = data$time[first_idx],
    stop = data$time[last_idx]
  )

  mod_col <- paste0(col, "_modifiers")
  if (mod_col %in% names(data)) {
    out$modifiers <- data[[mod_col]][first_idx]
  }

  if (length(group_cols) > 0) {
    grp_first <- data[first_idx, group_cols, drop = FALSE]
    out <- dplyr::bind_cols(grp_first, out)
  }
  out
}


#' Emit one row per non-`NA` frame of a point event column
#'
#' @keywords internal
pick_point_column <- function(data, col, group_cols) {
  mod_col <- paste0(col, "_modifiers")
  has_modifiers <- mod_col %in% names(data)

  keep <- !is.na(data[[col]])
  data <- data[keep, , drop = FALSE]
  if (nrow(data) == 0) {
    return(make_empty_bout_df(group_cols, col))
  }

  out <- dplyr::tibble(
    channel = col,
    value = as.character(data[[col]]),
    start = data$time,
    stop = data$time
  )

  if (has_modifiers) {
    out$modifiers <- data[[mod_col]]
  }

  if (length(group_cols) > 0) {
    out <- dplyr::bind_cols(data[, group_cols, drop = FALSE], out)
  }
  out
}


#' @keywords internal
make_empty_bout_df <- function(group_cols, col) {
  out <- dplyr::tibble(
    channel = character(),
    value = character(),
    start = numeric(),
    stop = numeric()
  )
  if (length(group_cols) > 0) {
    for (g in group_cols) {
      out[[g]] <- character()
    }
    out <- out[, c(group_cols, "channel", "value", "start", "stop")]
  }
  out
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
