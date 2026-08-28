#' Convert radians to degrees
#'
#' @param x Numeric vector of angles (radians).
#' @return Numeric vector of angles expressed in degrees.
#' @examples
#' rad_to_deg(pi)
#' rad_to_deg(c(0, pi / 2, pi))
#'
#' @family angle utilities
#' @export
rad_to_deg <- function(x) {
  (x * 180) / pi
}


#' Convert degrees to radians
#'
#' @param x Numeric vector of angles (degrees).
#' @return Numeric vector of angles expressed in radians.
#' @examples
#' deg_to_rad(180)
#' deg_to_rad(c(0, 90, 180))
#'
#' @family angle utilities
#' @export
deg_to_rad <- function(x) {
  (x * pi) / 180
}


#' Constrain angles to a standard range
#'
#' Wraps a vector of angles to a standard interval using modulo arithmetic.
#'
#' @param x A numeric vector of angles, in radians.
#' @param modulo A character string (default `"2pi"`) giving the target range:
#'   \describe{
#'     \item{`"2pi"`}{Wrap to `[0, 2*pi)`.}
#'     \item{`"pi"`}{Wrap to `(-pi, pi]`.}
#'     \item{`"asis"`}{Return unchanged.}
#'   }
#' @return A numeric vector the same length as `x`, wrapped to the chosen range.
#' @examples
#' angles <- c(-pi, 0, pi, 2 * pi, 3 * pi)
#'
#' wrap_angle(angles, "2pi")
#'
#' # The same angles on the signed interval
#' wrap_angle(angles, "pi")
#'
#' # "asis" is a no-op, useful when the range is chosen by a caller
#' wrap_angle(angles, "asis")
#'
#' @family angle utilities
#' @export
wrap_angle <- function(x, modulo = c("2pi", "pi", "asis")) {
  modulo <- match.arg(modulo)

  switch(
    modulo,
    "2pi" = x %% (2 * pi),
    "pi" = pi - ((pi - x) %% (2 * pi)),
    "asis" = x
  )
}


#' Remove wrapping from a sequence of angles
#'
#' Reverses the discontinuity introduced by wrapping, by accumulating the
#' shortest step between successive angles. A heading that crosses `2*pi`
#' therefore continues to increase rather than jumping back to zero, which is
#' what makes it differentiable. `NA` values are preserved in place.
#'
#' @param x A numeric vector of angles, in radians.
#' @return A numeric vector the same length as `x`, without wrapping
#'   discontinuities.
#' @examples
#' # A heading turning steadily past a full circle, wrapped to [0, 2*pi)
#' wrapped <- wrap_angle(seq(0, 3 * pi, length.out = 7), "2pi")
#' wrapped
#'
#' # Unwrapping restores the steady progression
#' unwrap_angle(wrapped)
#'
#' @family angle utilities
#' @export
unwrap_angle <- function(x) {
  if (length(x) == 0L) {
    return(x)
  }

  if (all(is.na(x))) {
    return(x)
  }

  result <- numeric(length(x))
  result[is.na(x)] <- NA_real_

  non_na_idx <- which(!is.na(x))
  x_clean <- x[non_na_idx]

  angle_diff <- diff(x_clean)
  angle_diff_wrapped <- wrap_angle(angle_diff, modulo = "pi")
  unwrapped_clean <- c(x_clean[1], x_clean[1] + cumsum(angle_diff_wrapped))

  result[non_na_idx] <- unwrapped_clean
  result
}
