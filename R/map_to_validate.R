#' @keywords internal
is_cartesian <- function(data) {
  is_cartesian_1d(data) || is_cartesian_2d(data) || is_cartesian_3d(data)
}

#' @keywords internal
ensure_is_cartesian <- function(data) {
  if (!is_cartesian(data)) {
    cli::cli_abort(
      "This data frame is not in a Cartesian coordinate system. Requires at least one of 'x', 'y', or 'z'."
    )
  }
}

#' @keywords internal
is_cartesian_1d <- function(data, stop = FALSE) {
  forbidden <- c("rho", "phi", "theta")
  present_forbidden <- intersect(names(data), forbidden)
  cartesian_axes <- c("x", "y", "z")
  present_axes   <- intersect(names(data), cartesian_axes)

  if (length(present_forbidden) > 0L) {
    invisible(FALSE)
  } else if (length(present_axes) != 1L) {
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}

#' @keywords internal
ensure_is_cartesian_1d <- function(data) {
  if (!is_cartesian_1d(data)) {
    cli::cli_abort(
      "This data frame is not in a 1D Cartesian coordinate system. Requires only 'x', 'y' or 'z'."
    )
  }
}

#' @keywords internal
is_cartesian_2d <- function(data) {
  # Must contain x and y
  if (!all(c("x", "y") %in% names(data))) return(invisible(FALSE))

  # If z exists, it must be entirely NA (or absent)
  if ("z" %in% names(data) && !all(is.na(data$z))) {
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}

#' @keywords internal
ensure_is_cartesian_2d <- function(data) {
  if (!is_cartesian_2d(data)) {
    cli::cli_abort(
      "This data frame is not in a 2D Cartesian coordinate system. Requires 'x' and 'y', with no 'z'."
    )
  }
}

#' @keywords internal
is_cartesian_3d <- function(data) {
  # Must contain x, y, and z
  if (!all(c("x", "y", "z") %in% names(data))) {
    return(invisible(FALSE))
  }

  # All required columns are present
  invisible(TRUE)
}

#' @keywords internal
ensure_is_cartesian_3d <- function(data) {
  if (!is_cartesian_3d(data)) {
    cli::cli_abort(
      "This data frame is not in a 3D Cartesian coordinate system. Requires 'x', 'y' and 'z' columns with non-NA values."
    )
  }
}

#' @keywords internal
is_polar <- function(data) {
  all(c("rho", "phi") %in% names(data)) && !any(c("theta", "z") %in% names(data))
}

#' @keywords internal
ensure_is_polar <- function(data) {
  if (!is_polar(data)) {
    cli::cli_abort("This data frame is not in a polar coordinate system.")
  }
}

#' @keywords internal
is_cylindrical <- function(data) {
  all(c("rho", "phi", "z") %in% names(data)) && !any(c("theta") %in% names(data))
}

#' @keywords internal
ensure_is_cylindrical <- function(data) {
  if (!is_cylindrical(data)) {
    cli::cli_abort("This data frame is not in a cylindrical coordinate system.")
  }
}

#' @keywords internal
is_spherical <- function(data) {
  all(c("rho", "phi", "theta") %in% names(data)) && !any(c("z") %in% names(data))
}

#' @keywords internal
ensure_is_spherical <- function(data) {
  if (!is_spherical(data)) {
    cli::cli_abort("This data frame is not in a spherical coordinate system.")
  }
}
