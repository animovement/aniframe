#' Custom tibble summary for aniframe
#'
#' @param x An aniframe object
#' @param ... Additional arguments (unused)
#' @return Named character vector with summary information
#' @importFrom pillar tbl_sum
#' @export
tbl_sum.aniframe <- function(x, ...) {
  default_header <- NextMethod()

  # Initialize new header
  new_header <- c(
    "Individuals" = paste(unique(x$individual), collapse = ", "),
    "Keypoints" = paste(unique(x$keypoint), collapse = ", ")
  )

  # Add sessions if column exists
  if ("session" %in% names(x)) {
    n_sessions <- length(unique(x$session))
    new_header <- c(new_header, "Sessions" = as.character(n_sessions))
  }

  # Add trials if column exists
  if ("trial" %in% names(x)) {
    new_header <- c(new_header, "Trials" = paste(unique(x$trial), collapse = ", "))
  }

  # Add sampling rate if available in metadata
  sampling_rate <- get_metadata(x)$sampling_rate
  if (!is.null(sampling_rate) && !is.na(sampling_rate)) {
    new_header <- c(new_header, "Sampling rate" = paste(sampling_rate, "Hz"))
  }

  new_header
}
