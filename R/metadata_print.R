#' @export
print.aniframe_metadata <- function(x, ...) {
  cli::cli_h2("aniframe metadata")

  if (length(x) == 0) {
    cli::cli_alert_info("No metadata available")
    return(invisible(x))
  }

  for (name in names(x)) {
    value <- x[[name]]
    value_class <- class(value)[1]  # Get the first class

    # Format the value based on type
    if (length(value) == 1 && is.na(value)) {
      cli::cli_text("{.field {name}} {.emph ({value_class})}: {.emph <NA>}")
    } else if (is.factor(value)) {
      cli::cli_text("{.field {name}} {.emph ({value_class})}: {.val {as.character(value)}}")
    } else if (length(value) > 1) {
      cli::cli_text("{.field {name}} {.emph ({value_class})}: {.val {paste(value, collapse = ', ')}}")
    } else {
      cli::cli_text("{.field {name}} {.emph ({value_class})}: {.val {value}}")
    }
  }

  invisible(x)
}
