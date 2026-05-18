#' Convert NaN to NA in numeric columns
#'
#' Replaces all `NaN` values with `NA` in numeric columns of a data frame.
#'
#' @param data A data frame.
#' @return A data frame with `NaN` values replaced by `NA` in numeric columns.
#' @export
convert_nan_to_na <- function(data) {
  dplyr::mutate(
    data,
    dplyr::across(dplyr::where(is.numeric), function(x) {
      ifelse(is.nan(x), NA, x)
    })
  )
}

#' Convert radians to degrees
#'
#' @param x Numeric vector of angles (radians).
#' @return Numeric vector of angles expressed in degrees.
#' @export
rad_to_deg <- function(x) {
  (x * 180) / pi
}

#' Convert degrees to radians
#'
#' @param x Numeric vector of angles (degrees).
#' @return Numeric vector of angles expressed in radians.
#' @export
deg_to_rad <- function(x) {
  (x * pi) / 180
}

#' Re-clothe a dispatched result with its animovement class and metadata
#'
#' After a generic strips a result down to a plain tibble (via
#' `NextMethod()`), restore the animovement class (`aniframe` or
#' `anievent`) and re-attach the metadata captured from the original
#' input.
#'
#' @param x The bare result returned by `NextMethod()`.
#' @param md Metadata captured before dispatch via [get_metadata()].
#' @param constructor Internal class constructor — `new_aniframe` or
#'   `new_anievent`.
#'
#' @return `x` with the animovement class and metadata restored.
#' @keywords internal
preserve_animovement_class <- function(x, md, constructor) {
  x <- constructor(x)
  set_metadata(x, metadata = md)
}
