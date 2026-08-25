ensure_valid_metadata <- function(metadata) {
  ensure_is_list(metadata)
  ensure_all_metadata_fields_present(metadata)
  ensure_metadata_fields_are_correct_class(metadata)
}

# Fields added after the initial schema. Their absence is tolerated on
# read so previously serialised objects continue to validate; new objects
# always have them via `default_metadata()`.
optional_metadata_fields <- function() {
  c("spec_version", "variables_event", "variables_index")
}

# Normalise user-supplied `variables_event` into canonical form. Accepts
# partial input — supplying only `state` or only `point` is fine, and the
# missing side defaults to `character()`. `NULL`, empty, and all-`NA`
# entries collapse to `character()` so callers can write
# `list(point = "call")` or `list(state = "x", point = NA)` without having
# to spell out both sides or wrap values in `as.character()`. Genuinely
# wrong types (e.g. integers) are left untouched for the validator to
# reject. Stored metadata always carries both entries as character vectors.
normalise_variables_event_entry <- function(v) {
  if (is.null(v) || length(v) == 0L || all(is.na(v))) {
    return(character())
  }
  if (is.character(v)) {
    return(v[!is.na(v)])
  }
  v
}

normalise_variables_event <- function(x) {
  if (is.null(x) || !is.list(x)) {
    return(x)
  }
  list(
    state = normalise_variables_event_entry(x$state),
    point = normalise_variables_event_entry(x$point)
  )
}

# Structural check for `variables_event`: must be a list with character
# vectors at `$state` and `$point`, and the two sets must not overlap (a
# column cannot be both state and point).
ensure_valid_variables_event <- function(x) {
  if (is.null(x)) {
    return(invisible())
  }
  if (!is.list(x) || !all(c("state", "point") %in% names(x))) {
    cli::cli_abort(c(
      "{.field variables_event} must be a list with entries {.val state} and {.val point}.",
      "i" = "Got names: {.val {names(x)}}."
    ))
  }
  if (!is.character(x$state) || !is.character(x$point)) {
    cli::cli_abort(
      "Both {.field variables_event$state} and {.field variables_event$point} must be character vectors."
    )
  }
  overlap <- intersect(x$state, x$point)
  if (length(overlap) > 0) {
    cli::cli_abort(c(
      "A column cannot be both a state and a point event variable.",
      "x" = "Overlapping: {.val {overlap}}."
    ))
  }
  invisible()
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
      "Metadata hasn't been initiated. Initialise it with {.fn set_metadata}."
    )
  }
}

# ------------------------------------------------------------------
# Is the "metadata" attribute a list?
# ------------------------------------------------------------------
is_list <- function(x) {
  is.list(x) && !is.data.frame(x) |> invisible()
}

ensure_is_list <- function(x) {
  if (!is_list(x)) {
    cli::cli_abort(
      "Metadata should be a list, but it is of class {class(metadata)}."
    )
  }
}

# ------------------------------------------------------------------
# Are all the necessary metadata fields present?
# ------------------------------------------------------------------
check_all_metadata_fields_present <- function(metadata) {
  mandatory_metadata_fields <- setdiff(
    names(default_metadata()),
    optional_metadata_fields()
  )
  all(mandatory_metadata_fields %in% names(metadata)) |>
    invisible()
}

ensure_all_metadata_fields_present <- function(metadata) {
  if (!check_all_metadata_fields_present(metadata)) {
    cli::cli_abort(
      "The object does not have the mandatory metadata fields."
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

    # Allow NA for any field (NA values can have any class)
    if (length(user_val) == 1 && is.na(user_val)) {
      matches <- c(matches, TRUE)
    } else if (!identical(class(user_val), class(default_val))) {
      matches <- c(matches, FALSE)
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
