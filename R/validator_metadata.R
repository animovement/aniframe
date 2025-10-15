validate_metadata <- function(metadata) {
  ensure_is_list(metadata)
  ensure_all_metadata_fields_present(metadata)
  ensure_metadata_fields_are_correct_class(metadata)
}

# ------------------------------------------------------------------
# Does the object have a "metadata" attribute?
# ------------------------------------------------------------------
check_metadata_exists <- function(data) {
  "metadata" %in% names(attributes(data)) |> invisible()
}

ensure_metadata_exists <- function(data) {
  if (!check_metadata_exists(data)) {
    cli::cli_abort(
      "Metadata hasn't been initiated. Initialise it with `set_metadata`."
    )
  }
}

# ------------------------------------------------------------------
# Is the "metadata" attribute a list?
# ------------------------------------------------------------------
check_is_list <- function(x) {
  is.list(x) && !is.data.frame(x) |> invisible()
}

ensure_is_list <- function(x) {
  if (!check_is_list(x)) {
    cli::cli_abort(
      "Metadata should be a list, but it is of class {class(metadata)}."
    )
  }
}

# ------------------------------------------------------------------
# Are all the necessary metadata fields present?
# ------------------------------------------------------------------
check_all_metadata_fields_present <- function(metadata) {
  mandatory_metadata_fields <- names(default_metadata())
  all(mandatory_metadata_fields %in% names(metadata)) |>
    invisible()
}

ensure_all_metadata_fields_present <- function(metadata) {
  if (!check_all_metadata_fields_present(metadata)) {
    cli::cli_abort(
      "The dataframe does not have the mandatory 'metadata' fields."
    )
  }
}

# ------------------------------------------------------------------
# Are all the necessary metadata fields of the correct class?
# ------------------------------------------------------------------
check_metadata_fields_are_correct_class <- function(metadata) {
  # ---- Class check for each supplied field ----------------------------
  supplied_names <- names(metadata)
  matches <- c()
  for (nm in supplied_names) {
    user_val <- metadata[[nm]]
    default_val <- default_metadata()[[nm]]

    # `identical()` returns a single logical even if class() gives a vector
    if (!identical(class(user_val), class(default_val))) {
      matches <- c(matches, FALSE)
      cli::cli_alert_info(
        "Metadata field '{nm}' is of class {class(user_val)}, but it should be of class {class(default_val)}."
      )
    } else {
      matches <- c(matches, TRUE)
    }
  }
  all(matches) |> invisible()
}

ensure_metadata_fields_are_correct_class <- function(metadata) {
  if (!check_metadata_fields_are_correct_class(metadata)) {
    cli::cli_abort(
      "Metadata fields are not of the correct types."
    )
  }
}
