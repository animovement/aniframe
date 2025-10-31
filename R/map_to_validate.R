#' Test whether a data frame uses a Cartesian coordinate system
#'
#' Returns `TRUE` if the data frame satisfies *any* of the 1‑D, 2‑D or 3‑D
#' Cartesian checks defined in the helper functions.
#'
#' @param data A data frame.
#' @return Logical scalar.
#' @export
is_cartesian <- function(data) {
  is_cartesian_1d(data) ||
    is_cartesian_2d(data) ||
    is_cartesian_3d(data)
}


#' Internal guard for Cartesian checks
#'
#' Stops with a clear error message if `data` is not Cartesian.
#'
#' @param data A data frame.
#' @keywords internal
ensure_is_cartesian <- function(data) {
  if (!is_cartesian(data)) {
    cli::cli_abort(
      "This data frame is not in a Cartesian coordinate system. Requires at least one of 'x', 'y', or 'z'."
    )
  }
}


#' Test for a 1‑D Cartesian coordinate system
#'
#' The data frame must contain **exactly one** of `x`, `y` or `z` and none of the
#' polar columns (`rho`, `phi`, `theta`).
#'
#' @param data A data frame.
#' @param stop Unused placeholder kept for API compatibility.
#' @return Logical scalar (invisible).
#' @export
is_cartesian_1d <- function(data, stop = FALSE) {
  forbidden        <- c("rho", "phi", "theta")
  present_forbidden <- intersect(names(data), forbidden)
  cartesian_axes   <- c("x", "y", "z")
  present_axes     <- intersect(names(data), cartesian_axes)

  if (length(present_forbidden) > 0L) {
    invisible(FALSE)
  } else if (length(present_axes) != 1L) {
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}


#' Internal guard for 1‑D Cartesian checks
#'
#' @param data A data frame.
#' @keywords internal
ensure_is_cartesian_1d <- function(data) {
  if (!is_cartesian_1d(data)) {
    cli::cli_abort(
      "This data frame is not in a 1D Cartesian coordinate system. Requires only 'x', 'y' or 'z'."
    )
  }
}


#' Test for a 2‑D Cartesian coordinate system
#'
#' Requires columns `x` and `y`.  Column `z` may be present only if it is
#' completely `NA`.
#'
#' @param data A data frame.
#' @return Logical scalar (invisible).
#' @export
is_cartesian_2d <- function(data) {
  # Must contain x and y
  if (!all(c("x", "y") %in% names(data))) {
    return(invisible(FALSE))
  }

  # If z exists, it must be entirely NA (or absent)
  if ("z" %in% names(data) && !all(is.na(data$z))) {
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}


#' Internal guard for 2‑D Cartesian checks
#'
#' @param data A data frame.
#' @keywords internal
ensure_is_cartesian_2d <- function(data) {
  if (!is_cartesian_2d(data)) {
    cli::cli_abort(
      "This data frame is not in a 2D Cartesian coordinate system. Requires 'x' and 'y', with no 'z'."
    )
  }
}


#' Test for a 3‑D Cartesian coordinate system
#'
#' Requires non‑missing columns `x`, `y` and `z`.
#'
#' @param data A data frame.
#' @return Logical scalar (invisible).
#' @export
is_cartesian_3d <- function(data) {
  # Must contain x, y, and z
  if (!all(c("x", "y", "z") %in% names(data))) {
    invisible(FALSE)
  } else if (all(c("x", "y", "z") %in% names(data)) && all(is.na(data$z))) {
    invisible(FALSE)
  } else {
    # All required columns are present
    invisible(TRUE)
  }
}


#' Internal guard for 3‑D Cartesian checks
#'
#' @param data A data frame.
#' @keywords internal
ensure_is_cartesian_3d <- function(data) {
  if (!is_cartesian_3d(data)) {
    cli::cli_abort(
      "This data frame is not in a 3D Cartesian coordinate system. Requires 'x', 'y' and 'z' columns with non-NA values."
    )
  }
}


#' Test whether a data frame uses a polar coordinate system
#'
#' Requires columns `rho` and `phi` and forbids `theta` or `z`.
#'
#' @param data A data frame.
#' @return Logical scalar.
#' @export
is_polar <- function(data) {
  all(c("rho", "phi") %in% names(data)) &&
    !any(c("theta", "z") %in% names(data))
}


#' Internal guard for polar checks
#'
#' @param data A data frame.
#' @keywords internal
ensure_is_polar <- function(data) {
  if (!is_polar(data)) {
    cli::cli_abort("This data frame is not in a polar coordinate system.")
  }
}


#' Test whether a data frame uses a cylindrical coordinate system
#'
#' Requires `rho`, `phi` and `z`; forbids `theta`.
#'
#' @param data A data frame.
#' @return Logical scalar.
#' @export
is_cylindrical <- function(data) {
  all(c("rho", "phi", "z") %in% names(data)) &&
    !any(c("theta") %in% names(data))
}


#' Internal guard for cylindrical checks
#'
#' @param data A data frame.
#' @keywords internal
ensure_is_cylindrical <- function(data) {
  if (!is_cylindrical(data)) {
    cli::cli_abort("This data frame is not in a cylindrical coordinate system.")
  }
}


#' Test whether a data frame uses a spherical coordinate system
#'
#' Requires `rho`, `phi` and `theta`; forbids `z`.
#'
#' @param data A data frame.
#' @return Logical scalar.
#' @export
is_spherical <- function(data) {
  all(c("rho", "phi", "theta") %in% names(data)) &&
    !any(c("z") %in% names(data))
}


#' Internal guard for spherical checks
#'
#' @param data A data frame.
#' @keywords internal
ensure_is_spherical <- function(data) {
  if (!is_spherical(data)) {
    cli::cli_abort("This data frame is not in a spherical coordinate system.")
  }
}
