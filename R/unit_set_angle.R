#' Set the angular unit of an aniframe object
#'
#' @description
#' Converts angular measurements (e.g., headings, orientations) stored in an
#' aniframe object from one unit to another. The function supports conversion
#' between degrees (`"deg"`) and radians (`"rad"`). It validates the requested
#' target unit, checks that the supplied columns exist and contain numeric
#' data, performs the conversion, and updates the object's metadata.
#'
#' @param data An aniframe object containing angular data.
#' @param cols Character vector of column names that hold the angular values to
#'   be converted. All listed columns must be present in `data` and be numeric.
#' @param to_unit Character string specifying the target angular unit. Must be
#'   one of the permitted units defined in `default_metadata()$unit_angle`
#'   (typically `"deg"` or `"rad"`).
#'
#' @return An aniframe object with the selected angular columns converted to
#'   the specified unit and with its `unit_angle` metadata updated accordingly.
#'
#' @details
#' The function proceeds through the following steps:\cr
#' • Verifies that `to_unit` is a permitted angular unit.\cr
#' • Confirms that each name in `cols` exists in `data` and that the corresponding
#'   columns are numeric.\cr
#' • Retrieves the current angular unit from the object's metadata.\cr
#' • If the current unit already matches `to_unit`, an informational message is
#'   displayed and the data are returned unchanged.\cr
#' • Otherwise, the appropriate conversion function is applied to each column:
#'   * `rad_to_deg()` when converting from radians to degrees.\cr
#'   * `deg_to_rad()` when converting from degrees to radians.\cr
#' • Finally, the object's metadata are updated with the new `unit_angle`.
#'
#' @examples
#' \dontrun{
#' # Assume `df` is an aniframe with heading angles stored in columns
#' # "head_left" and "head_right" measured in radians
#' df_deg <- set_unit_angle(df,
#'                           cols = c("head_left", "head_right"),
#'                           to_unit = "deg")
#'
#' # Convert back to radians
#' df_rad <- set_unit_angle(df_deg,
#'                           cols = c("head_left", "head_right"),
#'                           to_unit = "rad")
#' }
#'
#' @export
set_unit_angle <- function(data, cols, to_unit) {
  ensure_is_aniframe(data)

  # Check that to_unit is permitted
  if (!to_unit %in% levels(default_metadata()[["unit_angle"]])) {
    cli::cli_abort(
      "Angular unit can only be {levels(default_metadata()[[\"unit_angle\"]])}, not {to_unit}."
    )
  }

  # Check that provided columns are numeric
  if (!all(cols %in% names(data))){
    cli::cli_abort("All provided columns must be in the data.")
  }
  if (!all(vapply(data[, cols, drop = FALSE], is.numeric, logical(1)))){
    cli::cli_abort("All provided columns must be numeric.")
  }


  current_unit_angle <- get_metadata(data, "unit_angle")
  if (identical(as.character(current_unit_angle), to_unit)) {
    cli::cli_alert_info(
      "Angular unit is already {to_unit}."
    )
  } else if (to_unit == "deg"){
    cols_to_calibrate <- cols %in% names(data)
    data <- data |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::any_of(cols),
          .fns = ~ rad_to_deg(.x),
          .names = "{.col}" # keep the original column names
        )
      )
  } else if (to_unit == "rad"){
    cols_to_calibrate <- cols %in% names(data)
    data <- data |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::any_of(cols),
          .fns = ~ deg_to_rad(.x),
          .names = "{.col}" # keep the original column names
        )
      )
  }

  # Set metadata
  data <- as_aniframe(data) |>
    set_metadata(unit_angle = to_unit)

  data
}
