#' First‑Order Angular Difference with NA Padding
#'
#' A thin wrapper around base `diff()` that returns the smallest signed angular
#' distance (in radians) between successive elements of a numeric vector.  The
#' result is padded with `NA`s (equal to the chosen `lag`) so the output can be
#' used directly inside `dplyr::mutate()` without breaking row alignment.
#'
#' @param x A numeric vector of angles expressed in **radians**.
#' @param lag Integer ≥ 1. Number of positions to shift when computing the
#'   difference (identical to the `lag` argument of `base::diff`). Default is
#'   `1L`.
#' @return A numeric vector of length `length(x)`.  The first `lag` entries are
#'   `NA`; the remaining entries contain the minimal signed angular distance
#'   between successive angles, wrapped to the interval \(-π, π]\`.
#' @details
#' * **Angular wrapping** – Each raw difference `to_angle - from_angle` is fed
#'   to `calculate_angular_difference()`, which constrains the result to the
#'   shortest rotation (‑π … π).  This prevents the artificial jump that occurs
#'   at the ±π boundary.
#' * **NA padding** – Unlike `base::diff()`, which returns a vector of length
#'   `length(x) - lag`, this implementation prepends `lag` `NA`s.  This makes the
#'   function safe for use inside `dplyr::mutate()` where row‑wise alignment is
#'   required.
#' @keywords internal
diff_angle <- function(x, lag = 1L) {
  # Input validation – mimic base::diff's checks
  if (!is.numeric(x)) {
    cli::cli_abort("`x` must be a numeric vector of angles (in radians).")
  }
  if (lag < 1L) {
    cli::cli_abort("`lag` must be a positive integer.")
  }

  # Base case: no work to do
  if (length(x) <= lag) return(numeric(0))

  # Compute successive differences recursively, just like base::diff
  result <- x[(lag + 1):length(x)] - x[seq_len(length(x) - lag)]

  # Apply the angular‑distance conversion element‑wise
  result <- mapply(calculate_angular_difference,
                   from_angle = x[seq_len(length(x) - lag)],
                   to_angle   = x[(lag + 1):length(x)])

  # Prepend NAs to make it work in dplyr mutate functions
  result <- c(rep(NA, lag), result)
  result
}

#' Calculate angular difference
#' @param from_angle From angle
#' @param to_angle To angle
#' @keywords internal
calculate_angular_difference <- function(from_angle, to_angle) {
  diff_angle <- constrain_angles_radians(to_angle - from_angle)
  dplyr::case_when(
    diff_angle > pi ~ diff_angle - 2 * pi,
    .default = diff_angle
  )
}

#' @keywords internal
constrain_angles_radians <- function(x) {
  (x %% (2 * pi))
}

#' @keywords internal
rad_to_deg <- function(x) {
  (x * 180) / pi
}

#' @keywords internal
deg_to_rad <- function(x) {
  (x * pi) / 180
}
