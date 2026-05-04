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
