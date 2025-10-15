#' @keywords internal
cartesian_to_rho <- function(x, y) {
  sqrt(x^2 + y^2)
}

#' @keywords internal
cartesian_to_phi <- function(x, y, centered = FALSE) {
  angle <- dplyr::case_when(
    x > 0 & y > 0 ~ atan(x / y),
    x < 0 & y > 0 ~ pi - atan(-x / y),
    x < 0 & y < 0 ~ atan(-x / -y) + pi,
    x > 0 & y < 0 ~ 2 * pi - atan(x / -y)
  )

  # Keep between pi and -pi
  angle <- constrain_angles_radians(angle)

  if (centered == TRUE) {
    angle <- dplyr::case_when(
      angle > pi ~ angle - 2 * pi,
      angle < -pi ~ angle + 2 * pi,
      .default = angle
    )
  }

  return(angle)
}

#' @keywords internal
polar_to_x <- function(rho, phi) {
  rho * cos(phi)
}

#' @keywords internal
polar_to_y <- function(rho, phi) {
  rho * sin(phi)
}
