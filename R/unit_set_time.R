#' Set the temporal unit of an aniframe object
#'
#' @description
#' Converts time values in an aniframe object to a different unit of measurement.
#' The function handles both automatic unit conversion between standard time units
#' and custom calibration from frame or arbitrary units.
#'
#' @param data An aniframe object containing time data.
#' @param to_unit Character string specifying the target time unit. Must be
#'   one of the permitted units defined in `default_metadata()$unit_time`
#'   (typically "ms", "s", "m", "h" for milliseconds, seconds, minutes, hours).
#' @param calibration_factor Numeric value for scaling time values.
#'   Default is 1. When converting from standard time units (ms, s, m, h), this
#'   is ignored and the appropriate conversion factor is calculated automatically.
#'   When converting from "frame" or "unknown" units, you must provide a
#'   calibration factor to define the relationship between the current units and
#'   the target unit.
#'
#' @return An aniframe object with time values converted to the specified unit
#'   and updated metadata reflecting the new unit_time.
#'
#' @details
#' The function performs the following operations:
#' * Validates that `to_unit` is a permitted time unit
#' * Determines the current time unit from the object's metadata
#' * If converting from standard time units (ms, s, m, h) to another standard
#'   unit, automatically calculates the conversion factor
#' * If converting from "frame" or "unknown" units with `calibration_factor = 1`,
#'   issues an informational message and returns data unchanged
#' * Applies the calibration factor to the time column
#' * Updates the object's unit_time metadata
#'
#' @examples
#' \dontrun{
#' # Convert from milliseconds to seconds (automatic conversion)
#' data_s <- set_unit_time(data, to_unit = "s")
#'
#' # Convert from frames to seconds with custom calibration
#' # (e.g., 30 frames per second means 1 frame = 1/30 seconds)
#' data_s <- set_unit_time(data, to_unit = "s", calibration_factor = 1/30)
#'
#' # Convert from hours to minutes (automatic conversion)
#' data_m <- set_unit_time(data, to_unit = "m")
#' }
#'
#' @export
set_unit_time <- function(data, to_unit, calibration_factor = 1) {
  ensure_is_aniframe(data)

  # Check that to_unit is permitted
  if (!to_unit %in% levels(default_metadata()[["unit_time"]])) {
    cli::cli_abort(
      "Time unit can only be {levels(default_metadata()[[\"unit_time\"]])}, not {to_unit}."
    )
  }

  current_unit_time <- get_metadata(data, "unit_time")

  if (calibration_factor == 1 && current_unit_time %in% c("frame", "unknown")) {
    cli::cli_alert_info(
      "calibration_factor is not set, data remains unchanged."
    )
  } else if (calibration_factor == 1) {
    calibration_factor <- get_conversion_factor_time(
      from_unit = as.character(current_unit_time),
      to_unit = to_unit
    )
  }

  # Calibrate data
  data <- data |>
    dplyr::mutate(
      time = .data$time * calibration_factor
    ) |>
    as_aniframe() |>
    set_metadata(unit_time = to_unit)
  data
}

#' Set the sampling rate of an aniframe object
#'
#' @description
#' Sets the sampling rate (in Hz) for an aniframe object and optionally converts
#' time values from frames to seconds. If the data is already in SI time units,
#' only the metadata is updated without modifying the time values.
#'
#' @param data An aniframe object containing time data.
#' @param sampling_rate Numeric value specifying the sampling rate in Hz
#'   (samples per second). For example, a sampling rate of 30 means 30 frames
#'   per second.
#'
#' @return An aniframe object with updated sampling_rate metadata and, if
#'   applicable, time values converted from frames to seconds.
#'
#' @details
#' The function performs the following operations:
#' * Checks the current unit_time in the object's metadata
#' * If unit_time is "frame" or "unknown", converts time values to seconds
#'   using the formula: time_in_seconds = time_in_frames / sampling_rate
#' * If unit_time is already an SI unit (ms, s, m, h), leaves time values
#'   unchanged and issues an informational message
#' * Updates the sampling_rate in the object's metadata regardless of the
#'   current unit_time
#'
#' This function is particularly useful when working with motion capture or
#' video data where time is initially recorded as frame numbers.
#'
#' @examples
#' \dontrun{
#' # Set sampling rate for data in frames (converts to seconds)
#' data_with_rate <- set_sampling_rate(data, sampling_rate = 30)
#'
#' # Set sampling rate for data already in SI units (updates metadata only)
#' data_with_rate <- set_sampling_rate(data, sampling_rate = 100)
#' }
#'
#' @export
set_sampling_rate <- function(data, sampling_rate) {
  ensure_is_aniframe(data)
  if (!get_metadata(data, "unit_time") %in% c("frame", "unknown")) {
    cli::cli_alert_info(
      "unit_time is already set to a SI unit (not {c(\"frame\", \"unknown\")}). Data remains unchanged, but sampling_rate has been changed in the metadata"
    )
  } else {
    data <- data |>
      set_unit_time("s", calibration_factor = 1 / sampling_rate)
  }

  data <- data |>
    set_metadata(
      sampling_rate = sampling_rate
    )

  data
}

#' @keywords internal
get_conversion_factor_time <- function(from_unit, to_unit) {
  conv <- conversion_factors_time()
  conv[to_unit, from_unit]
}

#' @keywords internal
conversion_factors_time <- function() {
  m <- matrix(
    c(
      1,
      1 / 1000,
      1 / (1000 * 60),
      1 / (1000 * 60 * 60),
      1000,
      1,
      1 / 60,
      1 / (60 * 60),
      1000 * 60,
      60,
      1,
      1 / 60,
      1000 * 60 * 60,
      60 * 60,
      60,
      1
    ),
    nrow = 4,
    byrow = FALSE
  )

  # Attach row‑ and column‑names
  permitted_units <- c("ms", "s", "m", "h")
  rownames(m) <- permitted_units
  colnames(m) <- permitted_units
  m
}
