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
