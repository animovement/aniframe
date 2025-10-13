#' Create example aniframe data
#'
#' @return An example aniframe object
#' @export
#'
#' @examples
#' example_aniframe()
example_aniframe <- function() {
  aniframe(
    individual = rep(1:2, each = 25),
    keypoint = "centroid",
    time = rep(1:10, 5),
    x = stats::rnorm(50),
    y = stats::rnorm(50),
    trial = 1
  )
}
