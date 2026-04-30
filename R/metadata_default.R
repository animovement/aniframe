#' Default metadata structure
#'
#' @description
#' Returns a list containing the default metadata fields and their initial
#' values for an aniframe object. Most fields are initialized as `NA` and
#' should be set appropriately for your data.
#'
#' @return A named list with the following fields:
#' * `source`: Data source identifier (character, NA)
#' * `source_version`: Version of the data source (character, NA)
#' * `filename`: Original filename (character, NA)
#' * `sampling_rate`: Sampling rate in Hz (numeric, NA)
#' * `start_datetime`: Start date and time of recording (POSIXct, NA)
#' * `reference_frame`: Reference frame (factor, "allocentric")
#' * `coordinate_system`: Coordinate system (factor, "cartesian")
#' * `origin`: Location of the (0,0) coordinate relative to the recording
#'   frame (factor, "bottom_left"). Permitted values: "bottom_left", "top_left".
#' * `y_height`: Height of the recording frame in y-axis units (numeric, NA).
#'   Used by [set_origin()] to reflect y coordinates when switching origin
#'   conventions.
#'
#' @seealso [set_metadata()], [get_metadata()]
#'
#' @export
default_metadata <- function() {
  metadata <- list(
    source = as.character(NA),
    source_version = as.character(NA),
    filename = as.character(NA),
    sampling_rate = as.numeric(NA),
    start_datetime = as.POSIXct(NA),
    variables_what = c("individual", "keypoint"),
    variables_when = c("time"),
    variables_where = c("x", "y"),
    unit_space = factor(
      "px",
      levels = c(
        "px",
        "none",
        "nm",
        "um",
        "mm",
        "cm",
        "m",
        "km"
      )
    ),
    unit_angle = factor(
      "rad",
      levels = c(
        "rad",
        "deg"
      )
    ),
    unit_time = factor(
      "frame",
      levels = c(
        "unknown",
        "frame",
        "ns",
        "us",
        "ms",
        "s",
        "m",
        "h"
      )
    ),
    reference_frame = factor(
      "allocentric",
      levels = c(
        "allocentric",
        "egocentric"
      )
    ),
    coordinate_system = factor(
      "cartesian_2d",
      levels = c(
        "unknown",
        "cartesian_1d",
        "cartesian_2d",
        "cartesian_3d",
        "polar",
        "cylindrical",
        "spherical"
      )
    ),
    origin = factor(
      "bottom_left",
      levels = c(
        "bottom_left",
        "top_left"
      )
    ),
    y_height = as.numeric(NA)
  )

  class(metadata) <- "aniframe_metadata"
  metadata
}
