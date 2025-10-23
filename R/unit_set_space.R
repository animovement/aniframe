#' Set the spatial unit of an aniframe object
#'
#' @description
#' Converts spatial coordinates (x, y, z) in an aniframe object to a different
#' unit of measurement. The function handles both automatic unit conversion
#' between standard units and custom calibration from pixel or arbitrary units.
#'
#' @param data An aniframe object containing spatial coordinate data.
#' @param to_unit Character string specifying the target spatial unit. Must be
#'   one of the permitted units defined in `default_metadata()$unit_space`.
#' @param calibration_factor Numeric value for scaling spatial coordinates.
#'   Default is 1. When converting from standard units (mm, cm, m), this is
#'   ignored and the appropriate conversion factor is calculated automatically.
#'   When converting from "px" or "unknown", you must provide a calibration factor
#'   to define the relationship between the current units and the target unit.
#'
#' @return An aniframe object with spatial coordinates converted to the specified
#'   unit and updated metadata reflecting the new unit_space.
#'
#' @details
#' The function performs the following operations:
#' * Validates that `to_unit` is a permitted spatial unit
#' * Determines the current spatial unit from the object's metadata
#' * If converting from standard units (mm, cm, m) to another standard unit,
#'   automatically calculates the conversion factor
#' * If converting from "px" or "unknown" units with `calibration_factor = 1`,
#'   issues an informational message and returns data unchanged
#' * Applies the calibration factor to all spatial columns (x, y, z) that exist
#'   in the data
#' * Updates the object's unit_space metadata
#'
#' @examples
#' \dontrun{
#' # Convert from millimeters to centimeters (automatic conversion)
#' data_cm <- set_unit_space(data, to_unit = "cm")
#'
#' # Convert from pixels to millimeters with custom calibration
#' # (e.g., 1 pixel = 0.5 mm)
#' data_mm <- set_unit_space(data, to_unit = "mm", calibration_factor = 0.5)
#' }
#'
#' @export
set_unit_space <- function(data, to_unit, calibration_factor = 1) {
  ensure_is_aniframe(data)

  # Check that to_unit is permitted
  if (!to_unit %in% levels(default_metadata()[["unit_space"]])) {
    cli::cli_abort(
      "Space unit can only be {levels(default_metadata()[[\"unit_space\"]])}, not {to_unit}."
    )
  }

  current_unit_space <- get_metadata(data, "unit_space")
  if (calibration_factor == 1 && current_unit_space %in% c("px", "none")) {
    cli::cli_alert_info(
      "calibration_factor is not set, data remains unchanged."
    )
  } else if (calibration_factor == 1) {
    calibration_factor <- get_conversion_factor_space(
      from_unit = as.character(current_unit_space),
      to_unit = to_unit
    )
  }

  # Calibrate data
  space_cols <- c("x", "y", "z")
  cols_to_calibrate <- space_cols %in% names(data)
  data <- data |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(space_cols),
        .fns = ~ .x * calibration_factor,
        .names = "{.col}" # keep the original column names
      )
    )
  as_aniframe(data) |>
    set_metadata(unit_space = to_unit)
}

#' @keywords internal
get_conversion_factor_space <- function(from_unit, to_unit) {
  conv <- conversion_factors_space()
  conv[to_unit, from_unit]
}

#' @keywords internal
conversion_factors_space <- function() {
  m <- matrix(
    c(1, 1 / 10, 1 / 1000, 10, 1, 1 / 100, 1000, 100, 1),
    nrow = 3,
    byrow = FALSE
  )

  # Attach row‑ and column‑names
  permitted_units <- c("mm", "cm", "m")
  rownames(m) <- permitted_units
  colnames(m) <- permitted_units
  m
}
