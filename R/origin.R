#' Set the coordinate origin
#'
#' @description
#' Sets or updates the `origin` metadata field, which records where the (0,0)
#' coordinate sits relative to the recording frame. When the new origin
#' differs from the current one, the y coordinates are reflected around
#' `y_height` so the data is expressed in the new convention.
#'
#' @param data An aniframe object.
#' @param origin Character. One of `"bottom_left"` or `"top_left"`.
#'
#' @return The aniframe with reflected y coordinates (when the origin changed)
#'   and updated `origin` metadata.
#'
#' @details
#' The flip uses the formula `y_new = y_height - y_old`. The `y_height`
#' metadata field must therefore be set when the origin actually changes; if
#' it is `NA`, this function errors and asks the user to set it via
#' [set_y_height()]. When the supplied `origin` matches the current value,
#' the data is returned unchanged.
#'
#' @seealso [set_y_height()]
#'
#' @examples
#' \dontrun{
#' data <- example_aniframe()
#' data <- set_y_height(data, y_height = 1080)
#' data <- set_origin(data, origin = "top_left")
#' }
#'
#' @export
set_origin <- function(data, origin) {
  ensure_is_aniframe(data)

  if (!is.character(origin) || length(origin) != 1) {
    cli::cli_abort("{.arg origin} must be a single character string.")
  }

  current <- as.character(get_metadata(data, "origin"))
  if (identical(current, origin)) {
    return(data)
  }

  # The vertical axis is whichever column carries the `y` role, not one
  # literally named `y` (#109). A frame with no y axis -- a polar one --
  # has an origin convention, but not one this reflection can change.
  axes <- get_axes(data)
  if (!"y" %in% names(axes)) {
    cli::cli_abort(c(
      "This aniframe has no {.field y} axis to reflect.",
      "i" = "{.field coordinate_system} is {.val {get_coordinate_system(data)}}.",
      "i" = "Changing the origin reflects the vertical axis, so the frame needs one."
    ))
  }
  y_col <- axes[["y"]]
  ensure_has_column(data, y_col)

  y_height <- get_metadata(data, "y_height")
  if (length(y_height) == 0 || is.na(y_height)) {
    cli::cli_abort(c(
      "Cannot change origin: {.field y_height} is not set.",
      "i" = "Set it with {.code set_y_height(data, y_height = ...)}."
    ))
  }

  data <- reflect_axis(data, axis = y_col, reference = y_height)
  # Level membership of `origin` is validated here by `set_metadata`.
  set_metadata(data, origin = origin)
}


#' Set the y-axis frame height
#'
#' @description
#' Sets or updates the `y_height` metadata field, which records the height of
#' the recording frame in y-axis units. Used by [set_origin()] when reflecting
#' y coordinates between origin conventions (e.g. `bottom_left` <-> `top_left`).
#'
#' Reader functions in `aniread` populate `y_height` automatically from the
#' source (e.g. video frame height). For aniframes constructed manually,
#' [as_aniframe()] falls back to `max(y)`. Use this function to set the true
#' frame height when the auto-fallback is not appropriate.
#'
#' @param data An aniframe object.
#' @param y_height A single positive finite numeric value.
#'
#' @return The aniframe with updated `y_height` metadata.
#'
#' @seealso [set_origin()]
#'
#' @examples
#' \dontrun{
#' data <- example_aniframe()
#' data <- set_y_height(data, y_height = 1080)
#' }
#'
#' @export
set_y_height <- function(data, y_height) {
  ensure_is_aniframe(data)

  if (
    !is.numeric(y_height) ||
      length(y_height) != 1 ||
      !is.finite(y_height) ||
      y_height <= 0
  ) {
    cli::cli_abort(
      "{.arg y_height} must be a single positive finite numeric value."
    )
  }

  if ("y" %in% names(data)) {
    max_y <- suppressWarnings(max(data$y, na.rm = TRUE))
    if (is.finite(max_y) && y_height < max_y) {
      cli::cli_warn(c(
        "{.arg y_height} ({y_height}) is less than {.code max(y)} ({max_y}).",
        "i" = "Reflection across this height would produce negative y values."
      ))
    }
  }

  set_metadata(data, y_height = y_height)
}


#' Reflect a spatial axis around a reference value
#'
#' @description
#' Internal helper that reflects values in a numeric column around a reference,
#' computed as `reference - data[[axis]]`. Currently used by [set_origin()] to
#' flip the y-axis when changing the coordinate origin convention, but
#' parameterised so it can be reused for other axes (e.g. z) later.
#'
#' @param data A data frame (typically an aniframe) containing `axis`.
#' @param axis Character. Name of the column to reflect.
#' @param reference Numeric. A single finite value to reflect around.
#'
#' @return The data with `axis` replaced by `reference - data[[axis]]`.
#' @keywords internal
reflect_axis <- function(data, axis, reference) {
  if (!is.character(axis) || length(axis) != 1) {
    cli::cli_abort("{.arg axis} must be a single column name.")
  }
  ensure_has_column(data, axis)
  if (
    !is.numeric(reference) ||
      length(reference) != 1 ||
      !is.finite(reference)
  ) {
    cli::cli_abort(
      "{.arg reference} must be a single finite numeric value."
    )
  }
  data[[axis]] <- reference - data[[axis]]
  data
}
