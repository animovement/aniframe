#' Get metadata
#'
#' @param data aniframe
#'
#' @return the metadata associated with the aniframe
#' @export
get_metadata <- function(data) {
  ensure_metadata_exists(data)
  x <- attr(data, "metadata")
  class(x) <- c("aniframe_metadata", "list")
  x
}
