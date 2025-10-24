#' Difference of angular values
#'
#' Computes lagged differences between successive angles (in radians) and
#' converts each raw subtraction into the shortest signed angular distance
#' using `calculate_angular_difference()`.  The output mimics `base::diff()`,
#' returning `NA`s for the first `lag` positions so it works nicely inside
#' `dplyr::mutate()`.
#'
#' @param x   Numeric vector of angles (radians).
#' @param lag Positive integer indicating the lag (default = 1L). Must be
#'            an integer ≥ 1.
#' @return Numeric vector of the same length as `x`. The first `lag` entries
#'         are `NA`; subsequent entries contain the angular differences.
#' @examples
#' # Simple example
#' angles <- c(0, pi/2, pi, 3*pi/2)
#' diff_angle(angles)
#'
#' # Using a lag of 2
#' diff_angle(angles, lag = 2L)
#' @export
diff_angle <- function(x, lag = 1L) {
  # Input validation – mimic base::diff's checks
  if (!is.numeric(x)) {
    cli::cli_abort("`x` must be a numeric vector of angles (in radians).")
  }
  if (lag < 1L || !is.integer(lag)) {
    cli::cli_abort("`lag` must be a positive integer.")
  }

  # Base case: no work to do
  if (length(x) <= lag) {
    return(numeric(0))
  }

  # Compute successive differences recursively, just like base::diff
  result <- x[(lag + 1):length(x)] - x[seq_len(length(x) - lag)]

  # Apply the angular‑distance conversion element‑wise
  result <- mapply(
    calculate_angular_difference,
    from_angle = x[seq_len(length(x) - lag)],
    to_angle = x[(lag + 1):length(x)]
  )

  # Prepend NAs to make it work in dplyr mutate functions
  result <- c(rep(NA, lag), result)
  result
}

#' Calculate angular difference
#'
#' Computes the shortest signed angular distance (in radians) from
#' `from_angle` to `to_angle`.
#'
#' @param from_angle Numeric. Starting angle (radians).
#' @param to_angle   Numeric. Target angle (radians).
#' @return Numeric scalar – the angular difference wrapped to \[-π, π\].
#' @export
calculate_angular_difference <- function(from_angle, to_angle) {
  diff_angle <- constrain_angles_radians(to_angle - from_angle)
  dplyr::case_when(
    diff_angle > pi ~ diff_angle - 2 * pi,
    .default        = diff_angle
  )
}

#' Constrain angles to \[0, 2π)
#'
#' Wraps any numeric vector to the interval \[0, 2π) using modulo arithmetic.
#'
#' @param x Numeric vector of angles (radians).
#' @return Numeric vector of the same length, each element in \[0, 2π).
#' @export
constrain_angles_radians <- function(x) {
  x %% (2 * pi)
}

#' Convert radians to degrees
#'
#' @param x Numeric vector of angles (radians).
#' @return Numeric vector of angles expressed in degrees.
#' @export
rad_to_deg <- function(x) {
  (x * 180) / pi
}

#' Convert degrees to radians
#'
#' @param x Numeric vector of angles (degrees).
#' @return Numeric vector of angles expressed in radians.
#' @export
deg_to_rad <- function(x) {
  (x * pi) / 180
}
