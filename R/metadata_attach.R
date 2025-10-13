#' @keywords Internal
attach_metadata <- function(data, metadata) {
  attr(data, "metadata") <- metadata
  data
}
