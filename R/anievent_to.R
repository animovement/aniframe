#' Encode per-frame data into an anievent
#'
#' Run-length-encodes per-frame state and point variables into the
#' long-format [anievent()]. Works on a data frame (with bare-name
#' selection of the event columns and explicit `time` / identity)
#' or on an [aniframe()] (where everything is read from metadata).
#'
#' Distinct from [as_anievent()]: that one is a strict cast — the
#' input must already be in canonical anievent shape (one row per
#' bout, with `channel` / `type` / `label` / `start` / `stop`).
#' `to_anievent()` is the encoding verb that *produces* that shape
#' from per-frame data.
#'
#' @param data A data frame or an [aniframe()].
#' @param time For data-frame input, the column holding per-frame
#'   times. Bare name (tidyselect). Required.
#' @param state For data-frame input, columns to run-length-encode
#'   as state bouts. Bare names (tidyselect). Logical columns produce
#'   bouts on TRUE-runs, labelled by the column name; factor or
#'   character columns produce one bout per contiguous non-`NA` run
#'   of the same value.
#' @param point For data-frame input, columns to encode as point
#'   bouts. Bare names (tidyselect). Logical columns produce one
#'   point bout per TRUE frame, labelled by the column name; factor
#'   or character columns produce one bout per non-`NA` frame.
#' @param variables_what For data-frame input, identity columns
#'   (e.g. `individual`). Bare names (tidyselect). Bouts are
#'   isolated per identity group.
#' @param variables_when For data-frame input, additional temporal-
#'   grouping columns (e.g. `observation`, `session`, `trial`). Bare
#'   names (tidyselect). Like identity, these isolate bouts.
#' @param metadata Optional list of metadata attached to the result.
#'   For an aniframe input, fields like `unit_time` and
#'   `sampling_rate` are propagated automatically; `metadata`
#'   overrides those.
#' @param ... Passed to methods.
#'
#' @return An [anievent()].
#'
#' @examples
#' \dontrun{
#' library(tibble)
#' df <- tibble(
#'   individual = 1L,
#'   time = 1:8,
#'   behaviour = factor(c("REM", "REM", "REM", "wake", "wake", "REM", "REM", NA)),
#'   woke_up = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
#'   call = c(NA, "alarm", NA, NA, NA, NA, NA, NA)
#' )
#' to_anievent(
#'   df,
#'   time = time,
#'   state = c(behaviour, woke_up),
#'   point = call,
#'   variables_what = individual
#' )
#' }
#'
#' @export
to_anievent <- function(data, ...) {
  UseMethod("to_anievent")
}

#' @rdname to_anievent
#' @export
to_anievent.anievent <- function(data, ...) {
  data
}

#' @rdname to_anievent
#' @export
to_anievent.data.frame <- function(
  data,
  time,
  state = NULL,
  point = NULL,
  variables_what = NULL,
  variables_when = NULL,
  metadata = list(),
  ...
) {
  time_col <- colnames(dplyr::select(data, {{ time }}))
  if (length(time_col) != 1L) {
    cli::cli_abort(c(
      "{.arg time} must select exactly one column.",
      "x" = "Selected {length(time_col)} column{?s}: {.val {time_col}}."
    ))
  }

  state_cols <- colnames(dplyr::select(data, {{ state }}))
  point_cols <- colnames(dplyr::select(data, {{ point }}))
  if (length(state_cols) == 0L && length(point_cols) == 0L) {
    cli::cli_abort(c(
      "At least one of {.arg state} or {.arg point} must select a column.",
      "i" = "Use bare names: e.g. {.code state = c(behaviour, posture)}."
    ))
  }

  what_cols <- colnames(dplyr::select(data, {{ variables_what }}))
  when_cols <- colnames(dplyr::select(data, {{ variables_when }}))

  to_anievent_from_columns(
    data,
    time_col = time_col,
    state_cols = state_cols,
    point_cols = point_cols,
    what_cols = what_cols,
    when_cols = when_cols,
    metadata = metadata
  )
}

#' @rdname to_anievent
#' @export
to_anievent.aniframe <- function(
  data,
  variables_what = NULL,
  variables_when = NULL,
  metadata = list(),
  ...
) {
  md <- get_metadata(data)
  ve <- md$variables_event
  if (is.null(ve) || (length(ve$state) == 0 && length(ve$point) == 0)) {
    cli::cli_abort(c(
      "The {.cls aniframe} has no event columns declared.",
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

  bare <- dplyr::ungroup(dplyr::as_tibble(data))

  if (is.null(variables_what)) {
    # Auto-detect scope on **identity** columns only. Temporal-grouping
    # columns (observation / session / trial) carry distinct contexts
    # and must not be merged. Identity columns that are themselves
    # singletons are protected inside `detect_event_scope()`.
    channel_scopes <- list()
    for (col in declared) {
      channel_scopes[[col]] <- detect_event_scope(bare, col, host_what)
    }
    unique_scopes <- unique(channel_scopes)
    if (length(unique_scopes) > 1) {
      scope_lines <- vapply(
        names(channel_scopes),
        function(nm) {
          sc <- channel_scopes[[nm]]
          sc_str <- if (length(sc) == 0) {
            "<none>"
          } else {
            paste(sc, collapse = ", ")
          }
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
  } else {
    detected_scope <- variables_what
  }

  if (!is.null(variables_when)) {
    grouping_when <- setdiff(variables_when, c("start", "stop"))
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

  to_anievent_from_columns(
    bare,
    time_col = "time",
    state_cols = ve$state,
    point_cols = ve$point,
    what_cols = detected_scope,
    when_cols = grouping_when,
    metadata = metadata
  )
}


#' String-keyed kernel shared by `to_anievent` methods
#'
#' Walks `state_cols` and `point_cols`, encodes each via the
#' run-length / point-pick helpers, binds the bouts together, and
#' casts the result via [as_anievent()].
#'
#' @keywords internal
to_anievent_from_columns <- function(
  data,
  time_col,
  state_cols,
  point_cols,
  what_cols,
  when_cols,
  metadata
) {
  group_cols <- c(what_cols, when_cols)

  bouts <- list()
  for (col in state_cols) {
    sub <- rle_state_column(data, col, time_col, group_cols)
    if (nrow(sub) > 0) {
      sub$type <- "state"
    }
    bouts[[paste0("state__", col)]] <- sub
  }
  for (col in point_cols) {
    sub <- pick_point_column(data, col, time_col, group_cols)
    if (nrow(sub) > 0) {
      sub$type <- "point"
    }
    bouts[[paste0("point__", col)]] <- sub
  }

  out <- dplyr::bind_rows(bouts)
  if (!is.factor(out$label)) {
    out$label <- factor(out$label)
  }

  variables_when <- c(when_cols, "start", "stop")

  as_anievent(
    out,
    metadata = metadata,
    variables_what = what_cols,
    variables_when = variables_when
  )
}


#' Normalise an event-column vector to character labels
#'
#' Logical → column name on TRUE, `NA` on FALSE. Factor / character →
#' character. Lets a single kernel handle both binary (logical) and
#' multi-level (factor / character) inputs.
#'
#' @keywords internal
normalise_event_values <- function(x, col_name) {
  if (is.logical(x)) {
    return(ifelse(x, col_name, NA_character_))
  }
  as.character(x)
}


#' Detect the identity / grouping scope of one event column
#'
#' Returns the minimal subset of `candidate_cols` that the value of
#' `event_col` varies across (given `time_col`). Used by
#' `to_anievent.aniframe()` to drop redundant identity columns —
#' e.g. a `behaviour` column that is constant across `keypoint`
#' for each `(individual, time)` drops `keypoint` from the resulting
#' anievent's grouping.
#'
#' @keywords internal
detect_event_scope <- function(
  data,
  event_col,
  candidate_cols,
  time_col = "time"
) {
  if (all(is.na(data[[event_col]]))) {
    return(character())
  }

  scope <- candidate_cols
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (col in scope) {
      if (dplyr::n_distinct(data[[col]]) <= 1) {
        next
      }
      smaller <- setdiff(scope, col)
      grouped <- data |>
        dplyr::group_by(
          dplyr::across(dplyr::all_of(c(smaller, time_col)))
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
#' Within each `group_cols` partition, emit one row per maximal run
#' of identical non-`NA` (normalised) values in `col`. `NA` rows
#' break runs (so a value sequence like `c("REM", NA, "REM")` becomes
#' two bouts, not one). `start` is the `time_col` value at the first
#' frame in the run; `stop` is the value at the last frame.
#'
#' @keywords internal
rle_state_column <- function(data, col, time_col, group_cols) {
  vals <- normalise_event_values(data[[col]], col)
  if (length(vals) == 0 || all(is.na(vals))) {
    return(make_empty_bout_df(group_cols, col))
  }

  if (length(group_cols) > 0) {
    ord <- do.call(order, c(data[group_cols], list(data[[time_col]])))
    data <- data[ord, , drop = FALSE]
    vals <- vals[ord]
    key <- do.call(paste, c(data[group_cols], list(sep = "\r")))
  } else {
    ord <- order(data[[time_col]])
    data <- data[ord, , drop = FALSE]
    vals <- vals[ord]
    key <- rep("", nrow(data))
  }

  # Run-start detection on the full sequence (NAs included). A run
  # starts when the group key changes, OR the NA-ness flips, OR the
  # value changes (both sides non-NA). NA rows form their own runs
  # and get dropped from the emit list below.
  prev_key <- c(NA_character_, key[-length(key)])
  prev_val <- c(NA_character_, vals[-length(vals)])
  key_changed <- is.na(prev_key) | key != prev_key
  na_flip <- xor(is.na(vals), is.na(prev_val))
  val_changed <- !is.na(vals) & !is.na(prev_val) & vals != prev_val
  run_start <- key_changed | na_flip | val_changed
  run_id <- cumsum(run_start)

  first_idx <- which(!duplicated(run_id))
  last_idx <- which(!duplicated(run_id, fromLast = TRUE))

  keep_runs <- !is.na(vals[first_idx])
  first_idx <- first_idx[keep_runs]
  last_idx <- last_idx[keep_runs]

  out <- dplyr::tibble(
    channel = col,
    label = vals[first_idx],
    start = data[[time_col]][first_idx],
    stop = data[[time_col]][last_idx]
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
pick_point_column <- function(data, col, time_col, group_cols) {
  vals <- normalise_event_values(data[[col]], col)
  mod_col <- paste0(col, "_modifiers")
  has_modifiers <- mod_col %in% names(data)

  keep <- !is.na(vals)
  data <- data[keep, , drop = FALSE]
  vals <- vals[keep]
  if (length(vals) == 0) {
    return(make_empty_bout_df(group_cols, col))
  }

  out <- dplyr::tibble(
    channel = col,
    label = vals,
    start = data[[time_col]],
    stop = data[[time_col]]
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
    label = character(),
    start = numeric(),
    stop = numeric()
  )
  if (length(group_cols) > 0) {
    for (g in group_cols) {
      out[[g]] <- character()
    }
    out <- out[, c(group_cols, "channel", "label", "start", "stop")]
  }
  out
}
