validate_cols <- function(data) {
  # Validate required columns
  has_position <- any(c("x", "y", "z") %in% names(data))
  if (!has_position) {
    cli::cli_abort(
      "Missing positional data. Make sure to include x/y/z at least."
    )
  }

  if (!"time" %in% names(data)) {
    cli::cli_abort("Missing a time column.")
  }

  invisible(data)
}
