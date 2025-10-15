ensure_is_cartesian <- function(data) {
  if (all(!c("x", "y") %in% names(data))) {
    cli::cli_abort("This data frame is not in a Cartesian coordinate system.")
  }
}

ensure_is_polar <- function(data) {
  if (all(!c("rho", "theta") %in% names(data))) {
    cli::cli_abort("This data frame is not in a polar coordinate system.")
  }
}
