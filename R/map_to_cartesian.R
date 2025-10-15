#' Map from polar to Cartesian coordinates
#'
#' @param data an aniframe with polar coordinates
#'
#' @return an aniframe with Cartesian coordinates
#' @export
map_to_cartesian <- function(data) {
  ensure_is_aniframe()
  if (is_polar(data)) {
    data <- map_to_cartesian_polar(data)
  } else if (is_cylindrical(data)) {
    data <- map_to_cartesian_cylindrical(data)
  } else if (is_spherical(data)) {
    data <- map_to_cartesian_spherical(data)
  } else {
    cli::cli_abort("Data is neither polar, cylindrical or spherical.")
  }

  aniframe::as_aniframe(data)
}

# TODO: Make individual functions for polar, cylindrical and spherical

map_to_cartesian_polar <- function(data) {
  ensure_is_polar(data)
  data |>
    dplyr::mutate(
      x = polar_to_x(.data$rho, .data$phi),
      y = polar_to_y(.data$rho, .data$phi),
      z = NA
    )
}

map_to_cartesian_cylindrical <- function(data) {
  ensure_is_cylindrical(data)
  data |>
    dplyr::mutate(
      x = polar_to_x(.data$rho, .data$rho),
      y = polar_to_y(.data$rho, .data$rho),
      # TODO: z = polar_to_z()
    )
}

map_to_cartesian_spherical <- function(data) {
  # TODO: ensure_coord_spherical(data)
  data |>
    dplyr::mutate(
      x = polar_to_x(.data$rho, .data$theta),
      y = polar_to_y(.data$rho, .data$theta),
      # TODO: z = polar_to_z()
    )
}
