#' Add events from an anievent onto an aniframe host
#'
#' Joins an [anievent()] onto the per-frame time grid of an [aniframe()]
#' host, adding one column per `channel` of the anievent. For each row
#' of the host, the new column carries the `value` of the active bout
#' on that channel for the matching subject (and observation /
#' session / trial, if those are present on both sides); `NA` where no
#' bout is active.
#'
#' @param data An aniframe — the host whose time grid receives the events.
#' @param events An anievent.
#' @param variables_event Optional `list(state = ..., point = ...)`
#'   declaring how to classify each channel on the resulting aniframe's
#'   `variables_event` metadata. Resolution priority: this argument
#'   (when supplied) overrides everything else; otherwise the events'
#'   own `variables_event` metadata is used; any channels still
#'   unclassified are auto-detected from the bouts (a channel is
#'   `point` iff every bout has `start == stop`, else `state`). Set
#'   this argument when the auto-detect rule would mislabel, e.g. a
#'   state channel where every bout happens to be single-frame.
#'
#' @return The host aniframe with one new column per channel of
#'   `events`. The new columns are factor-valued and `NA` outside any
#'   bout. Each channel is registered in
#'   `metadata$variables_event$state` or `$point` according to its
#'   bouts (a channel is `point` iff every bout has `start == stop`,
#'   otherwise `state`).
#'
#' @details
#' Before joining, the events are validated (`validate_anievent()`) —
#' this catches overlapping bouts in the same channel for the same
#' subject, since channels are mutually exclusive by definition.
#'
#' Time-unit handling:
#' * If both objects share `unit_time`, the join uses the values as-is.
#' * If they differ but both are SI (`ms`, `s`, `m`, `h`), `events` is
#'   converted to the host's unit.
#' * If one side is in `"frame"` or `"unknown"`, a `sampling_rate` on
#'   either side is used to bridge to seconds; without one the function
#'   errors.
#'
#' Identity / grouping columns: the join uses the intersection of
#' `(variables_what)` and the grouping part of `variables_when`
#' (everything except `time` on host, `start`/`stop` on events) between
#' the two objects. Frames outside any bout get `NA`; new column names
#' colliding with existing host columns error out.
#'
#' @examples
#' \dontrun{
#' af <- aniframe(individual = 1L, time = 1:10, x = 1:10, y = 1:10)
#' ae <- anievent(
#'   individual = 1L,
#'   channel = c("behaviour", "behaviour"),
#'   value = c("REM", "wake"),
#'   start = c(1, 5),
#'   stop = c(4, 10)
#' )
#' add_events(af, ae)
#' }
#'
#' @export
add_events <- function(data, events, variables_event = NULL) {
  ensure_is_aniframe(data)
  ensure_is_anievent(events)
  ensure_anievent_structural(events)

  events <- reconcile_unit_time(data, events)

  channel_names <- unique(events$channel)

  host_md <- get_metadata(data)
  events_md <- get_metadata(events)

  host_what <- intersect(host_md$variables_what, names(data))
  events_what <- intersect(events_md$variables_what, names(events))
  host_grouping <- intersect(
    setdiff(host_md$variables_when, "time"),
    names(data)
  )
  events_grouping <- intersect(
    setdiff(events_md$variables_when, c("start", "stop")),
    names(events)
  )

  join_keys <- c(
    intersect(host_what, events_what),
    intersect(host_grouping, events_grouping)
  )

  channel_type <- resolve_channel_type(
    channel_names,
    events,
    variables_event %||% events_md$variables_event
  )

  data <- interval_join_channels(data, events, channel_names, join_keys)
  emitted <- attr(data, ".emitted_per_channel")
  attr(data, ".emitted_per_channel") <- NULL

  state_channels <- channel_names[channel_type == "state"]
  point_channels <- channel_names[channel_type == "point"]

  declared <- host_md$variables_event %||%
    list(state = character(), point = character())
  declared$state <- unique(c(
    declared$state,
    unlist(emitted[state_channels], use.names = FALSE)
  ))
  declared$point <- unique(c(
    declared$point,
    unlist(emitted[point_channels], use.names = FALSE)
  ))

  set_metadata(data, variables_event = declared)
}


#' Reconcile time units between host and events
#'
#' Converts `events` so its temporal columns share `unit_time` with
#' `data`. Pure SI conversions are auto-resolved; frame ↔ SI requires
#' a `sampling_rate` to be available on either side.
#'
#' @keywords internal
reconcile_unit_time <- function(data, events) {
  host_unit <- as.character(get_metadata(data, "unit_time"))
  events_unit <- as.character(get_metadata(events, "unit_time"))

  if (identical(host_unit, events_unit)) {
    return(events)
  }

  si_units <- c("ns", "us", "ms", "s", "m", "h")
  frame_like <- c("frame", "unknown")

  if (host_unit %in% si_units && events_unit %in% si_units) {
    return(set_unit_time(events, host_unit))
  }

  # frame <-> unknown: both placeholders, no calibration needed.
  if (host_unit %in% frame_like && events_unit %in% frame_like) {
    return(set_metadata(events, unit_time = host_unit))
  }

  # Crossing the frame/SI boundary needs a sampling_rate.
  sr <- get_metadata(events, "sampling_rate")
  if (is.null(sr) || is.na(sr)) {
    sr <- get_metadata(data, "sampling_rate")
  }

  if (is.null(sr) || is.na(sr)) {
    cli::cli_abort(c(
      "Cannot reconcile {.field unit_time} = {.val {events_unit}} (events) with {.val {host_unit}} (host).",
      "i" = "Crossing the {.val frame}/SI boundary needs a {.field sampling_rate} on either object."
    ))
  }

  if (events_unit %in% frame_like) {
    # frame_like -> SI: scale frames to seconds, then SI -> host_unit
    events <- set_sampling_rate(events, sr)
    if (!identical(host_unit, "s")) {
      events <- set_unit_time(events, host_unit)
    }
    return(events)
  }

  # events SI -> host frame_like: scale SI -> seconds, then seconds -> frames
  if (!identical(events_unit, "s")) {
    events <- set_unit_time(events, "s")
  }
  set_unit_time(events, host_unit, calibration_factor = sr)
}


#' Interval-join channels from an anievent onto the host time grid
#'
#' For each channel, adds one or more factor columns to `data`:
#' * **No overlap within an identity-group**: a single column
#'   `<channel>` whose value at each frame is the bout `value` active
#'   there (`NA` outside any bout).
#' * **Overlap within an identity-group**: the bouts are greedy
#'   interval-coloured into the smallest set of disjoint tracks.
#'   Track 1 stays as `<channel>`; tracks 2.. are emitted as
#'   `<channel>_2`, `<channel>_3`, ... Each output column is itself
#'   mutually exclusive, matching the structural invariant the
#'   `aniframe` side requires.
#'
#' If the anievent carries non-empty modifiers for a channel, parallel
#' `<channel>_modifiers` (and `<channel>_2_modifiers` etc.)
#' list-columns are added.
#'
#' @keywords internal
interval_join_channels <- function(data, events, channel_names, join_keys) {
  events <- dplyr::as_tibble(events)
  events_has_modifiers <- "modifiers" %in% names(events)

  unmatched_acc <- list()
  total_bouts <- 0L
  total_unmatched <- 0L
  emitted_per_channel <- list()

  for (ch in channel_names) {
    bouts <- events[events$channel == ch, , drop = FALSE]
    total_bouts <- total_bouts + nrow(bouts)

    channel_has_modifiers <- events_has_modifiers &&
      any(lengths(bouts$modifiers) > 0)

    if (length(join_keys) > 0) {
      host_key <- do.call(
        paste,
        c(data[join_keys], list(sep = "\r"))
      )
      ev_key <- do.call(
        paste,
        c(bouts[join_keys], list(sep = "\r"))
      )
      matched <- ev_key %in% unique(host_key)
      if (any(!matched)) {
        total_unmatched <- total_unmatched + sum(!matched)
        unmatched_acc[[length(unmatched_acc) + 1L]] <- bouts[
          !matched,
          join_keys,
          drop = FALSE
        ]
      }
    } else {
      host_key <- rep("", nrow(data))
      ev_key <- rep("", nrow(bouts))
    }

    tracks <- assign_bout_tracks(bouts, ev_key)
    n_tracks <- if (length(tracks) > 0) max(tracks) else 0L
    emitted <- character(n_tracks)

    for (t in seq_len(n_tracks)) {
      col_name <- if (t == 1L) ch else paste0(ch, "_", t)
      if (col_name %in% names(data)) {
        cli::cli_abort(c(
          "Channel column would collide with existing host column: {.val {col_name}}.",
          "i" = "Rename the channel on the {.cls anievent} or the host column."
        ))
      }
      emitted[t] <- col_name

      new_col <- factor(rep(NA, nrow(data)), levels = levels(bouts$value))
      if (channel_has_modifiers) {
        new_mod_col <- vector("list", nrow(data))
        for (j in seq_len(nrow(data))) {
          new_mod_col[[j]] <- character()
        }
      }

      track_bouts <- which(tracks == t)
      for (i in track_bouts) {
        mask <- host_key == ev_key[i] &
          data$time >= bouts$start[i] &
          data$time <= bouts$stop[i]
        new_col[mask] <- bouts$value[i]
        if (channel_has_modifiers) {
          mods <- bouts$modifiers[[i]]
          if (length(mods) > 0) {
            for (j in which(mask)) {
              new_mod_col[[j]] <- mods
            }
          }
        }
      }

      data[[col_name]] <- new_col
      if (channel_has_modifiers) {
        data[[paste0(col_name, "_modifiers")]] <- new_mod_col
      }
    }

    emitted_per_channel[[ch]] <- emitted
  }

  report_unmatched_events(unmatched_acc, total_bouts, total_unmatched)
  attr(data, ".emitted_per_channel") <- emitted_per_channel
  data
}


#' Greedy interval-colour bouts per identity-group into tracks
#'
#' Bouts that overlap (or touch at a single point — `start == stop` of
#' previous bout) are assigned to different tracks within their
#' identity-group, so each track is mutually exclusive. Bouts in
#' different groups can share a track (different rows on the
#' aniframe). Track 1 is preferred where possible.
#'
#' @param bouts A data frame of bouts (one channel's worth).
#' @param ev_key A character vector the same length as `nrow(bouts)`
#'   identifying which identity-group each bout belongs to.
#' @return Integer vector of track assignments (same length as
#'   `nrow(bouts)`).
#' @keywords internal
assign_bout_tracks <- function(bouts, ev_key) {
  order_idx <- order(ev_key, bouts$start)
  tracks_per_bout <- integer(nrow(bouts))
  # Per-group state: numeric vector of `max_stop_so_far` for each track
  # used in that group. Indexed by character key.
  group_tracks <- list()

  for (idx in order_idx) {
    grp_key <- ev_key[idx]
    grp_state <- group_tracks[[grp_key]] %||% numeric(0)

    assigned <- FALSE
    for (t in seq_along(grp_state)) {
      if (grp_state[t] < bouts$start[idx]) {
        grp_state[t] <- bouts$stop[idx]
        tracks_per_bout[idx] <- t
        assigned <- TRUE
        break
      }
    }
    if (!assigned) {
      grp_state <- c(grp_state, bouts$stop[idx])
      tracks_per_bout[idx] <- length(grp_state)
    }

    group_tracks[[grp_key]] <- grp_state
  }

  tracks_per_bout
}


#' Report event bouts whose identity / temporal-grouping keys don't
#' exist in the host
#'
#' Emits an informational message when *some* bouts were dropped
#' because their join keys were not in the host, and a warning when
#' *all* bouts were dropped (no matched events at all — almost
#' certainly a user error).
#'
#' @keywords internal
report_unmatched_events <- function(
  unmatched_acc,
  total_bouts,
  total_unmatched
) {
  if (total_unmatched == 0L) {
    return(invisible())
  }

  unmatched_df <- dplyr::distinct(dplyr::bind_rows(unmatched_acc))
  formatted <- vapply(
    seq_len(nrow(unmatched_df)),
    function(i) {
      paste(
        vapply(
          names(unmatched_df),
          function(c) paste0(c, "=", unmatched_df[[c]][i]),
          character(1)
        ),
        collapse = ", "
      )
    },
    character(1)
  )

  if (total_unmatched == total_bouts) {
    cli::cli_warn(c(
      "{.fn add_events}: no event bouts matched the host's identity / temporal context.",
      "x" = "{total_unmatched} bout{?s} dropped; the resulting event column{?s} {?is/are} entirely {.val NA}.",
      "i" = "Unmatched key{?s}: {.val {formatted}}."
    ))
  } else {
    cli::cli_inform(c(
      "i" = "{.fn add_events}: {total_unmatched} of {total_bouts} bout{?s} dropped because their key{?s} {?is/are} not in the host.",
      "*" = "Unmatched: {.val {formatted}}."
    ))
  }
}


#' Resolve state vs point per channel
#'
#' Priority: explicit `declared` argument → auto-detect by duration
#' (point iff every bout has `start == stop`, else state). A channel
#' present in both `declared$state` and `declared$point` is resolved
#' as state (state wins on tie — overlap of declarations is most
#' likely a user error and state is the more conservative choice).
#'
#' @keywords internal
resolve_channel_type <- function(channel_names, events, declared) {
  declared_state <- declared$state %||% character()
  declared_point <- declared$point %||% character()
  vapply(
    channel_names,
    function(ch) {
      if (ch %in% declared_state) {
        return("state")
      }
      if (ch %in% declared_point) {
        return("point")
      }
      sub <- events[events$channel == ch, , drop = FALSE]
      if (all(sub$start == sub$stop)) "point" else "state"
    },
    character(1)
  )
}


#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
