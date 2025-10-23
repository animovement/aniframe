#' Check if object is an aniframe
#'
#' @param x An object to test
#' @return Logical: TRUE if x inherits from aniframe
#' @export
is_aniframe <- function(x) {
  inherits(x, "aniframe")
}

#' @keywords internal
ensure_is_aniframe <- function(data) {
  if (!is_aniframe(data)) {
    cli::cli_abort("Data is not an aniframe.")
  }
}
