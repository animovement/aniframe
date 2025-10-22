#' Calculate angular difference
#' @param from_angle From angle
#' @param to_angle To angle
#' @keywords internal
calculate_angular_difference <- function(from_angle, to_angle) {
  diff_angle <- from_angle - to_angle
  dplyr::case_when(
    diff_angle > pi ~ diff_angle - 2 * pi,
    diff_angle < -pi ~ diff_angle + 2 * pi,
    .default = diff_angle
  )
}

#' @keywords internal
rad_to_deg <- function(x) {
  (x * 180) / pi
}

#' @keywords internal
deg_to_rad <- function(x) {
  (x * pi) / 180
}

#' @keywords internal
constrain_angles_radians <- function(x) {
  (x %% (2 * pi))
}
