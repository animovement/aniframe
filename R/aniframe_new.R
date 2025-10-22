# Constructor and main creation functions for aniframe class

#' Create a new aniframe object (internal constructor)
#'
#' @param x A data frame to convert to aniframe
#' @return An aniframe object
#' @keywords internal
new_aniframe <- function(x) {
  class(x) <- c("aniframe", class(x))
  class(x) <- unique(class(x))
  x
}
