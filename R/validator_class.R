# ------------------------------------------------------------------
# Simple class helpers (keep as‑is)
# ------------------------------------------------------------------
check_class <- function(x, cls) {
  cls %in% class(x)
}

ensure_class <- function(x, cls) {
  if (!check_class(cls)) {
    cli::cli_abort("Expected an object of class {cls}, but got {class(x)}.")
  }
}
