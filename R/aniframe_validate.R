validate_cols <- function(data) {
  # Validate required columns
  has_pos <- has_position(data, allowed_position_cols())

  if (!has_pos) {
    cli::cli_abort(
      "Missing positional data. Make sure to include x/y/z at least."
    )
  }

  if (!"time" %in% names(data)) {
    cli::cli_abort("Missing a time column.")
  }

  invisible(data)
}

#' @keywords internal
allowed_position_cols <- function(){
  list(
    cartesian_1d_x = c("x"),
    cartesian_1d_y = c("y"),
    cartesian_1d_z = c("z"),
    cartesian_2d_xy = c("x", "y"),
    cartesian_2d_xz = c("x", "z"),
    cartesian_2d_yz = c("y", "z"),
    cartesian_3d = c("x", "y", "z"),
    polar = c("rho", "phi"),
    cylindrical = c("rho", "phi", "z"),
    spherical = c("rho", "phi", "theta")
  )
}

#' @keywords internal
has_position <- function(data, allowed_position_cols) {
  any(sapply(allowed_position_cols, function(req) all(req %in% names(data))))
}

#' @keywords internal
matching_position_system <- function(data) {
  # Keep only the groups whose required columns are all present
  if (is_cartesian(data)){
    return("cartesian")
  } else if (is_polar(data)){
    return("polar")
  } else if (is_cylindrical(data)){
    return("cylindrical")
  } else if (is_spherical(data)){
    return("spherical")
  }
}
