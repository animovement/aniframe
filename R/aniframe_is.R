#' Check if object is an aniframe
#'
#' @param x An object to test
#' @return Logical: TRUE if x inherits from aniframe
#' @export
is_aniframe <- function(x) {
  inherits(x, "aniframe")
}

#' Ensure object is an aniframe
#'
#' @param x An object to test
#' @return Error if not an aniframe
#' @export
ensure_is_aniframe <- function(x) {
  if (!is_aniframe(x)) {
    cli::cli_abort("Data is not an aniframe.")
  }
}
