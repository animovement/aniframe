#' @keywords internal
is_cartesian <- function(data) {
  any(c("x", "y", "z") %in% names(data))
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
is_polar <- function(data) {
  all(c("rho", "phi") %in% names(data))
}

#' @keywords internal
ensure_is_polar <- function(data) {
  if (!is_polar(data)) {
    cli::cli_abort("This data frame is not in a polar coordinate system.")
  }
}

#' @keywords internal
is_cylindrical <- function(data) {
  all(c("rho", "phi", "z") %in% names(data))
}

#' @keywords internal
ensure_is_cylindrical <- function(data) {
  if (!is_cylindrical(data)) {
    cli::cli_abort("This data frame is not in a cylindrical coordinate system.")
  }
}

#' @keywords internal
is_spherical <- function(data) {
  all(c("rho", "phi", "theta") %in% names(data))
}

#' @keywords internal
ensure_is_spherical <- function(data) {
  if (!is_spherical(data)) {
    cli::cli_abort("This data frame is not in a spherical coordinate system.")
  }
}
