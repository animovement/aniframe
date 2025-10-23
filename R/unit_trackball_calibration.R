#' Calculate trackball calibration factor
#'
#' Computes a calibration factor for converting optical flow sensor units to
#' real-world distance units. The function compares the known physical distance
#' traveled by a trackball (based on its diameter and number of rotations) to
#' the measured distance in sensor units.
#'
#' The returned calibration factor can be used in `set_unit_space`.
#'
#' @param data An aniframe object containing x and y coordinates from the
#'   optical flow sensor.
#' @param ball_diameter Numeric. The diameter of the trackball in real-world
#'   units (e.g., mm or cm).
#' @param ball_rotations Numeric. The number of complete rotations the ball
#'   was rotated during calibration.
#'
#' @return A numeric calibration factor (real distance / measured distance).
#'   Multiply sensor readings by this factor to convert to real-world units.
#'
#' @details The function:
#' \itemize{
#'   \item Calculates the real distance traveled as circumference × rotations
#'   \item Measures the distance in sensor units using the axis with maximum travel
#'   \item Returns the ratio of real distance to measured distance
#' }
#'
#' @examples
#' \dontrun{
#' # Calibrate with a 50mm diameter ball rotated 10 times
#' cal_factor <- get_trackball_calibration_factor(
#'   data = sensor_data,
#'   ball_diameter = 50,
#'   ball_rotations = 10
#' )
#' }
#'
#' @export
get_trackball_calibration_factor <- function(
  data,
  ball_diameter,
  ball_rotations
) {
  ensure_is_aniframe(data)
  ball_circum <- pi * ball_diameter
  real_dist <- ball_circum * ball_rotations
  max_x <- dplyr::last(data$x) - dplyr::first(data$x)
  max_y <- dplyr::last(data$y) - dplyr::first(data$y)
  measured_dist <- c(abs(max_x), abs(max_y))[which.max(c(
    abs(max_x),
    abs(max_y)
  ))]

  calibration_factor <- real_dist / measured_dist
  calibration_factor
}
