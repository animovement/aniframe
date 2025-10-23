#' @keywords internal
cartesian_to_rho <- function(x, y, z = NULL) {
  if (is.null(z)) {
    sqrt(x^2 + y^2)
  } else {
    sqrt(x^2 + y^2 + z^2)
  }
}

#' @keywords internal
cartesian_to_phi <- function(x, y, centered = FALSE) {
  # atan2(y, x) returns angles in [-pi, pi]
  angle <- atan2(y, x)

  if (!centered) {
    # map to [0, 2*pi)
    angle <- (angle %% (2 * pi))
  }
  angle
}

#' Polar angle θ (angle from the +z axis) – vectorised
#'
#' @param x numeric vector of x‑coordinates
#' @param y numeric vector of y‑coordinates
#' @param z numeric vector of z‑coordinates
#' @return numeric vector of angles in radians
#' @keywords internal
cartesian_to_theta <- function(x, y, z) {
  # Full 3‑D radius for each observation
  rho <- cartesian_to_rho(x, y, z)

  # Initialise theta with zeros (covers the origin case automatically)
  theta <- numeric(length(rho))

  # Identify rows where rho > 0 (i.e., not the origin)
  idx <- rho > 0

  # Compute acos only where it is safe
  theta[idx] <- acos(z[idx] / rho[idx])

  invisible(theta)
}

#' @keywords internal
polar_to_x <- function(rho, phi) {
  rho * cos(phi)
}

#' @keywords internal
polar_to_y <- function(rho, phi) {
  rho * sin(phi)
}

#' Convert cylindrical radius ρ and polar angle θ to the Cartesian z‑coordinate.
#'
#' @param rho   Numeric vector – cylindrical radius (√(x²+y²)).
#' @param theta Numeric vector – polar angle measured from the +z axis (radians).
#' @return      Numeric vector of z‑coordinates (same length as input).
#' @keywords internal
spherical_to_z <- function(rho, theta) {
  # Initialise output with NA so that any non‑finite input stays NA.
  z <- rep(NA_real_, length(rho))

  ## -----------------------------------------------------------------
  ## 1.  Identify well‑behaved (non‑pole, finite) entries
  ## -----------------------------------------------------------------
  ok_idx <- is.finite(rho) &
    is.finite(theta) &
    abs(sin(theta)) > .Machine$double.eps # sin(theta) ≠ 0  →  not a pole

  if (any(ok_idx)) {
    # Regular case:  z = ρ / tan(θ)  (equivalently ρ * cot(θ))
    z[ok_idx] <- rho[ok_idx] / tan(theta[ok_idx])
  }

  ## -----------------------------------------------------------------
  ## 2.  Handle the two pole regions (θ ≈ 0  or  θ ≈ π)
  ## -----------------------------------------------------------------
  # Positive‑z pole (θ ≈ 0)
  pos_pole_idx <- is.finite(rho) &
    is.finite(theta) &
    !ok_idx &
    theta < 0.5 * .Machine$double.eps

  if (any(pos_pole_idx)) {
    # By definition the point lies on the +z axis → z = 0
    z[pos_pole_idx] <- 0
  }

  # Negative‑z pole (θ ≈ π)
  neg_pole_idx <- is.finite(rho) &
    is.finite(theta) &
    !ok_idx &
    abs(theta - pi) < 0.5 * .Machine$double.eps

  if (any(neg_pole_idx)) {
    # Point lies on the –z axis → z = 0 (sign is irrelevant because radius = 0)
    z[neg_pole_idx] <- 0
  }

  ## -----------------------------------------------------------------
  ## 3.  Anything left untouched remains NA (covers NA/NaN/Inf inputs)
  ## -----------------------------------------------------------------
  invisible(z)
}
