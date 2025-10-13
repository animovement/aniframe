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
#' * `point_of_reference`: Point of reference (factor, "bottom_left")
#'
#' @seealso [set_metadata()], [get_metadata()]
#'
#' @export
default_metadata <- function() {
  list(
    source = as.character(NA),
    source_version = as.character(NA),
    filename = as.character(NA),
    sampling_rate = as.numeric(NA),
    start_datetime = as.POSIXct(NA),
    reference_frame = factor("allocentric"),
    coordinate_system = factor("cartesian"),
    point_of_reference = factor("bottom_left")
  )
}
