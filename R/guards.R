# ------------------------------------------------------------------
# Simple structural guards
# ------------------------------------------------------------------
#' @keywords internal
is_class <- function(x, cls) {
  cls %in% class(x)
}

#' @keywords internal
ensure_class <- function(x, cls) {
  if (!is_class(x, cls)) {
    cli::cli_abort("Expected an object of class {cls}, but got {class(x)}.")
  }
}

#' @keywords internal
ensure_has_column <- function(data, col) {
  if (!col %in% names(data)) {
    cli::cli_abort("Column {.val {col}} not found in data.")
  }
}


#' Ensure the object is one of the animovement frame classes
#'
#' @param data Object to test.
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_is_aniframe_or_anievent <- function(data) {
  if (!is_aniframe(data) && !is_anievent(data)) {
    cli::cli_abort("Data is neither an aniframe nor an anievent.")
  }
  invisible(TRUE)
}
