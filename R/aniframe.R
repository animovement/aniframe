#' Create an aniframe data frame
#'
#' Creates a specialized data frame for animal tracking data with required
#' columns for positional data (x/y/z) and time, plus optional columns for
#' individual, keypoint, trial, and session identifiers.
#'
#' @param ... Name-value pairs to create columns in the data frame
#' @param metadata Optional list of metadata
#' @param .rows Number of rows (passed to tibble)
#' @param .name_repair How to repair column names (passed to tibble)
#'
#' @return An aniframe object (tibble with aniframe class)
#' @export
#'
#' @examples
#' aniframe(
#'   individual = rep(1:2, each = 25),
#'   time = rep(1:10, 5),
#'   x = rnorm(50),
#'   y = rnorm(50),
#'   trial = 1
#' )
aniframe <- function(
    ...,
    metadata = list(),
    .rows = NULL,
    .name_repair = c("check_unique", "unique", "universal", "minimal")
) {
  dots <- list(...)

  # Check if a single tibble was passed
  if (length(dots) == 1 && is.data.frame(dots[[1]])) {
    x <- dots[[1]]
  } else {
    # Otherwise construct a tibble from the arguments
    x <- dplyr::tibble(..., .rows = .rows, .name_repair = .name_repair)
  }

  # Convert the tibble to aniframe
  x <- as_aniframe(x, metadata = metadata)
  x
}
