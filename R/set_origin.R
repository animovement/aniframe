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
