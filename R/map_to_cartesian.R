#' Map from polar to Cartesian coordinates
#'
#' @param data an aniframe with polar coordinates
#'
#' @return an aniframe with Cartesian coordinates
#' @export
map_to_cartesian <- function(data) {
  ensure_is_aniframe(data)
  if (is_polar(data)) {
    data <- map_to_cartesian_polar(data)
  } else if (is_cylindrical(data)) {
    data <- map_to_cartesian_cylindrical(data)
  } else if (is_spherical(data)) {
    data <- map_to_cartesian_spherical(data)
  } else {
    cli::cli_abort("Data is neither polar, cylindrical or spherical.")
  }

  as_aniframe(data)
}

#' @keywords internal
map_to_cartesian_polar <- function(data) {
  ensure_is_polar(data)
  data |>
    dplyr::mutate(
      x = polar_to_x(.data$rho, .data$phi),
      y = polar_to_y(.data$rho, .data$phi),
      z = as.numeric(NA)
    ) |>
    dplyr::select(-c("rho", "phi"))
}

#' @keywords internal
map_to_cartesian_cylindrical <- function(data) {
  ensure_is_cylindrical(data)
  data |>
    dplyr::mutate(
      x = polar_to_x(.data$rho, .data$phi),
      y = polar_to_y(.data$rho, .data$phi),
      z = .data$z
    ) |>
    dplyr::select(-c("rho", "phi"))
}

#' @keywords internal
map_to_cartesian_spherical <- function(data) {
  ensure_is_spherical(data)
  data |>
    dplyr::mutate(
      x = polar_to_x(.data$rho, .data$phi),
      y = polar_to_y(.data$rho, .data$phi),
      z = spherical_to_z(.data$rho, .data$theta)
    ) |>
    dplyr::select(-c("rho", "phi", "theta"))
}
