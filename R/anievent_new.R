# Constructor for the anievent class

#' Create a new anievent object (internal constructor)
#'
#' @param x A data frame to convert to anievent.
#' @return An anievent object.
#' @keywords internal
new_anievent <- function(x) {
  class(x) <- c("anievent", class(x))
  class(x) <- unique(class(x))
  x
}
