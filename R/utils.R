#' @keywords internal
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
