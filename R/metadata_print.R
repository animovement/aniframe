#' Print method for aniframe metadata
#'
#' Renders the metadata as a single block — captured via
#' [cli::cli_format_method()] and emitted with [cat()] — so there's no
#' leading newline and no blank lines between entries. This makes the
#' output render cleanly in HTML contexts such as Quarto / R Markdown.
#'
#' @param x An `aniframe_metadata` list.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.aniframe_metadata <- function(x, ...) {
  out <- cli::cli_format_method({
    cli::cli_h1("aniframe metadata")

    if (length(x) == 0) {
      cli::cli_alert_info("No metadata available")
    } else {
      for (name in names(x)) {
        value <- x[[name]]
        value_class <- class(value)[1]

        if (length(value) == 1 && is.na(value)) {
          cli::cli_text(
            "{.field {name}} {.emph ({value_class})}: {.emph <NA>}"
          )
        } else if (is.factor(value)) {
          levels_str <- paste(levels(value), collapse = ", ")
          cli::cli_text(
            "{.field {name}} {.emph ({value_class})}: {.val {as.character(value)}}"
          )
          cli::cli_text("  {.emph [levels: {levels_str}]}")
        } else if (length(value) > 1) {
          cli::cli_text(
            "{.field {name}} {.emph ({value_class})}: {.val {paste(value, collapse = ', ')}}"
          )
        } else {
          cli::cli_text(
            "{.field {name}} {.emph ({value_class})}: {.val {value}}"
          )
        }
      }
    }
  })

  cat(trimws(paste(out, collapse = "\n")), "\n", sep = "")
  invisible(x)
}
