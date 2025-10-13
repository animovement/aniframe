#' Set metadata for an aniframe
#'
#' @description
#' Sets or updates metadata for an aniframe object. Metadata can be provided
#' either as named arguments or as a list. If the aniframe already has metadata,
#' the new values will be merged with existing values, with new values taking
#' precedence.
#'
#' Default metadata fields include:
#' * `source`: Data source identifier
#' * `source_version`: Version of the data source
#' * `filename`: Original filename
#' * `sampling_rate`: Sampling rate in Hz
#' * `start_datetime`: Start date and time of recording
#' * `reference_frame`: Reference frame (default: "allocentric")
#' * `coordinate_system`: Coordinate system (default: "cartesian")
#' * `point_of_reference`: Point of reference (default: "bottom_left")
#'
#' @param data An aniframe object
#' @param ... Named metadata values (e.g., `sampling_rate = 30, source = "sleap"`)
#' @param metadata Alternatively, a named list of metadata. Cannot be used
#'   simultaneously with `...`
#'
#' @return The aniframe object with updated metadata
#'
#' @seealso [get_metadata()], [default_metadata()]
#'
#' @examples
#' \dontrun{
#' # Set metadata using named arguments
#' data <- set_metadata(data, sampling_rate = 30, source = "sleap")
#'
#' # Set metadata using a list
#' md <- list(sampling_rate = 30, source = "sleap")
#' data <- set_metadata(data, metadata = md)
#' }
#'
#' @export
set_metadata <- function(data, ..., metadata = NULL) {
  # ------------------------------------------------------------------
  # Process the inputs
  # ------------------------------------------------------------------
  dot_args <- list(...)

  # Ensure that the user provides input with *either* ... or a metadata list
  if (!is.null(metadata) && !rlang::is_empty(dot_args)) {
    cli::cli_abort("Metadata input can only be provided as either name-value pairs *or* a list through the {.arg metadata} parameter, not both.")
  } else if (!is.null(metadata)) {
    user_md <- metadata
  } else if (!rlang::is_empty(dot_args)) {
    ensure_is_list(dot_args)
    user_md <- dot_args
  } else {
    user_md <- list()
  }

  # ------------------------------------------------------------------
  # Does the data have metadata or not?
  # ------------------------------------------------------------------
  if (!check_metadata_exists(data)){
    new_md <- default_metadata()
  } else {
    new_md <- get_metadata(data)
  }

  # ------------------------------------------------------------------
  # Combine and attach metadata
  # ------------------------------------------------------------------
  new_md <- utils::modifyList(new_md, user_md)
  validate_metadata(new_md)
  data <- attach_metadata(data, new_md)

  # ------------------------------------------------------------------
  # Calibrate time
  # ------------------------------------------------------------------
  # TODO: Figure out whether it makes sense to include these special cases in the aniframe package

  # has_sr   <- "sampling_rate" %in% names(user_md)
  # sr_new   <- if (has_sr) user_md[["sampling_rate"]] else NULL
  # if (has_sr) user_md[["sampling_rate"]] <- NULL
  # if (has_sr) {
  #   data <- calibrate_time(data, sampling_rate = sr_new)
  # }

  data
}
