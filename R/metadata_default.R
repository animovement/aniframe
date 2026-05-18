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
#' * `filename`: Original filename(s) (character vector, NA). Accepts a
#'   vector of length 1 or more — readers that load from multiple files
#'   (e.g. `aniread::read_trackball()`) populate this with all source paths.
#' * `sampling_rate`: Sampling rate in Hz (numeric, NA)
#' * `start_datetime`: Start date and time of recording (POSIXct, NA)
#' * `reference_frame`: Reference frame (factor, "allocentric")
#' * `coordinate_system`: Coordinate system (factor, "cartesian")
#' * `origin`: Location of the (0,0) coordinate relative to the recording
#'   frame (factor, "bottom_left"). Permitted values: "bottom_left", "top_left".
#' * `y_height`: Height of the recording frame in y-axis units (numeric, NA).
#'   Used by [set_origin()] to reflect y coordinates when switching origin
#'   conventions.
#' * `connections`: Named list of connection tables, one per identity or
#'   temporal variable (typically `keypoint` for skeletons; could also be
#'   `individual` for social networks). Each entry is a 2-column tibble of
#'   `from`/`to` pairs. Default is an empty list. Manage via
#'   [set_connections()], [get_connections()], [add_connections()] and
#'   [remove_connections()].
#' * `variables_event`: Named list with two entries, `state` and `point`,
#'   each a character vector naming columns that carry per-frame
#'   categorical event labels. `state` columns are interval-valued
#'   (durative behaviours, ordered coarse to fine to encode nesting);
#'   `point` columns are instantaneous (zero-duration events). Foundation
#'   for the `anievent` class and downstream event-handling utilities.
#'   Default is an empty list for each.
#' * `spec_version`: Named list of semantic version strings, one per class
#'   in the animovement ecosystem (currently `aniframe` and `anievent`).
#'   Versions the full data contract of each class (mandatory columns,
#'   validator, and the metadata fields the class uses), independently of
#'   the package version. Objects serialised before this field existed are
#'   tolerated by [ensure_valid_metadata()]; new objects always get it.
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
    variables_event = list(
      state = character(),
      point = character()
    ),
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
    y_height = as.numeric(NA),
    connections = list(),
    spec_version = list(
      aniframe = "1.0.0",
      anievent = "0.1.0"
    )
  )

  class(metadata) <- "aniframe_metadata"
  metadata
}
