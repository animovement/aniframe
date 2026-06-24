#' Check if object is an anievent
#'
#' @param x An object to test.
#' @return Logical: `TRUE` if `x` inherits from `anievent`.
#' @export
is_anievent <- function(x) {
  inherits(x, "anievent")
}

#' Ensure object is an anievent
#'
#' @param x An object to test.
#' @return Errors if `x` is not an anievent; otherwise returns invisibly.
#' @export
ensure_is_anievent <- function(x) {
  if (!is_anievent(x)) {
    cli::cli_abort("Data is not an anievent.")
  }
}
