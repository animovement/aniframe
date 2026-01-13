#' Create an aniframe data frame
#'
#' Creates a specialized data frame for movement data with columns defining
#' entity identity, timepoints, and spatial position.
#'
#' @param ... Name-value pairs to create columns in the data frame.
#' @param metadata Optional list of metadata.
#' @param variables_what Character vector of identity columns that together
#'   define a unique entity. Defaults to `c("individual", "keypoint")`.
#' @param variables_when Character vector of temporal columns that together
#'   define a unique timepoint. Defaults to `"time"`.
#' @param variables_where Character vector of spatial columns that together
#'   define position. Defaults to `c("x", "y")`.
#' @param .rows Number of rows (passed to tibble).
#' @param .name_repair How to repair column names (passed to tibble).
#'
#' @return An aniframe object (tibble with aniframe class).
#' @export
#'
#' @examples
#' aniframe(
#'   individual = rep(1:2, each = 25),
#'   time = rep(1:10, 5),
#'   x = rnorm(50),
#'   y = rnorm(50)
#' )
#'
#' # Custom variables
#' aniframe(
#'   track = rep(1:3, each = 10),
#'   trial = 1,
#'   time = rep(1:10, 3),
#'   x = rnorm(30),
#'   y = rnorm(30),
#'   variables_what = "track",
#'   variables_when = c("trial", "time")
#' )
aniframe <- function(
  ...,
  metadata = list(),
  variables_what = NULL,
  variables_when = NULL,
  variables_where = NULL,
  .rows = NULL,
  .name_repair = c("check_unique", "unique", "universal", "minimal")
) {
  dots <- rlang::list2(...)

  # Check if a single data frame was passed
  if (length(dots) == 1 && is.data.frame(dots[[1]])) {
    x <- dots[[1]]
  } else {
    x <- dplyr::tibble(..., .rows = .rows, .name_repair = .name_repair)
  }

  as_aniframe(
    x,
    metadata = metadata,
    variables_what = variables_what,
    variables_when = variables_when,
    variables_where = variables_where
  )
}
