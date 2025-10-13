#' Check if object is an aniframe
#'
#' @param x An object to test
#' @return Logical: TRUE if x inherits from aniframe
#' @export
is_aniframe <- function(x) {
  inherits(x, "aniframe")
}
