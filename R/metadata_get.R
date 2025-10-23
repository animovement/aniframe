#' Get metadata
#'
#' @param data aniframe
#' @param fields If only specific metadata fields should be returned.
#'
#' @return the metadata associated with the aniframe
#' @export
get_metadata <- function(data, fields = NULL) {
  ensure_metadata_exists(data)
  x <- attr(data, "metadata")
  if (!is.null(fields) && length(fields) == 1) {
    x <- x[names(x) %in% fields][[1]]
  } else if (!is.null(fields) && length(fields) > 1) {
    x <- x[names(x) %in% fields]
    class(x) <- c("aniframe_metadata", "data.frame")
  } else {
    class(x) <- c("aniframe_metadata", "list")
  }
  x
}
