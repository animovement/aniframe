#' Title-case and pluralise a metadata column name for the print header
#'
#' Used by [tbl_sum.aniframe()] to derive row labels from `variables_what` /
#' `variables_when` (e.g. `"individual"` -> `"Individuals"`).
#'
#' @keywords internal
title_case_pluralised <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), "s")
}

#' Build the "Time" interval row for the aniframe print summary
#'
#' Returns `NULL` when the interval cannot be expressed in seconds (e.g.
#' `unit_time = "frame"` with no `sampling_rate`, or `unit_time = "unknown"`).
#' When `start_datetime` is set in metadata, formats absolute datetimes;
#' otherwise formats elapsed time as `HH:MM:SS`. Switches to millisecond
#' precision (`HH:MM:SS.fff`) when the recording is shorter than one second.
#'
#' @keywords internal
format_time_interval <- function(x, md) {
  if (!"time" %in% names(x)) {
    return(NULL)
  }

  time_min <- suppressWarnings(min(x$time, na.rm = TRUE))
  time_max <- suppressWarnings(max(x$time, na.rm = TRUE))
  if (!is.finite(time_min) || !is.finite(time_max)) {
    return(NULL)
  }

  spu <- seconds_per_time_unit(
    as.character(md$unit_time),
    md$sampling_rate
  )
  if (is.null(spu) || !is.finite(spu)) {
    return(NULL)
  }

  secs_min <- time_min * spu
  secs_max <- time_max * spu

  # Use sub-second (millisecond) precision when the recording is shorter
  # than one second (otherwise rounding both endpoints to integer seconds
  # collapses them to the same value, e.g. an 88 ms run -> "00:00:00 to
  # 00:00:00").
  fractional <- (secs_max - secs_min) < 1

  start_dt <- md$start_datetime
  if (
    !is.null(start_dt) &&
      length(start_dt) == 1 &&
      !is.na(start_dt)
  ) {
    fmt <- if (fractional) {
      "%Y-%m-%d %H:%M:%OS3"
    } else {
      "%Y-%m-%d %H:%M:%S"
    }
    label <- paste(
      format(start_dt + secs_min, fmt),
      "to",
      format(start_dt + secs_max, fmt)
    )
  } else {
    label <- paste(
      format_seconds_as_hms(secs_min, fractional = fractional),
      "to",
      format_seconds_as_hms(secs_max, fractional = fractional)
    )
  }

  c("Time" = label)
}

#' Multiplier from a metadata `unit_time` value to seconds
#'
#' Returns `NA_real_` when conversion is not possible (e.g. `"frame"` without
#' a `sampling_rate`, or `"unknown"`).
#'
#' @keywords internal
seconds_per_time_unit <- function(unit, sampling_rate) {
  switch(
    unit,
    "frame" = if (
      !is.null(sampling_rate) &&
        length(sampling_rate) == 1 &&
        !is.na(sampling_rate) &&
        sampling_rate > 0
    ) {
      1 / sampling_rate
    } else {
      NA_real_
    },
    "ns" = 1e-9,
    "us" = 1e-6,
    "ms" = 1e-3,
    "s" = 1,
    "m" = 60,
    "h" = 3600,
    NA_real_
  )
}

#' Format seconds as HH:MM:SS (or HH:MM:SS.fff)
#'
#' @param s Numeric seconds.
#' @param fractional If `TRUE`, format with millisecond precision
#'   (`HH:MM:SS.fff`). Defaults to `FALSE` (integer seconds, rounded).
#'
#' @keywords internal
format_seconds_as_hms <- function(s, fractional = FALSE) {
  s <- as.numeric(s)
  if (s < 0) {
    return(paste0("-", format_seconds_as_hms(-s, fractional = fractional)))
  }
  if (fractional) {
    hours <- floor(s / 3600)
    mins <- floor((s %% 3600) / 60)
    secs <- s - hours * 3600 - mins * 60
    sprintf("%02d:%02d:%06.3f", hours, mins, secs)
  } else {
    s_int <- round(s)
    hours <- s_int %/% 3600
    mins <- (s_int %% 3600) %/% 60
    secs <- s_int %% 60
    sprintf("%02d:%02d:%02d", hours, mins, secs)
  }
}
