#' Validate a complete metadata list and attach it
#'
#' The write path shared by [set_metadata()] and the internal callers
#' that legitimately write structural fields — the constructors and the
#' variable setters. Unlike [set_metadata()] it applies no field-level
#' policy: the caller has already decided what the metadata should be.
#'
#' @param data An aniframe or anievent object.
#' @param metadata A complete metadata list.
#'
#' @return `data`, with `metadata` attached.
#' @keywords internal
write_metadata <- function(data, metadata) {
  ensure_valid_metadata(metadata)
  ensure_valid_variables_event(metadata$variables_event)
  attach_metadata(data, metadata)
}


#' Refuse the metadata fields that have their own setters
#'
#' [set_metadata()] writes the metadata list and nothing else, which is
#' what makes it safe to use everywhere. The `variables_*` fields need
#' more than that: they name columns, so the names have to be checked
#' against the frame, and for the three structural roles the frame has to
#' be retyped, reordered, regrouped and its derived fields refreshed.
#' Writing one of them as a *field* is therefore refused, and the
#' dedicated setters do the job instead.
#'
#' Restoring a **complete** metadata object is a different operation, and
#' is allowed. Rebuilding a frame and putting its metadata back is the
#' round-trip the class-preserving methods perform internally, and
#' downstream packages do it too — `animetric::summarise_keypoints()`
#' recomputes a frame and restores the metadata it captured beforehand.
#' Refusing that left them no way to carry metadata across a rebuild at
#' all.
#'
#' @param user_md The metadata the caller supplied.
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_no_declaration_fields <- function(user_md) {
  # A complete metadata object is a wholesale replacement of the
  # attribute, not a field write.
  if (has_all_metadata_fields(user_md)) {
    return(invisible(TRUE))
  }

  offending <- intersect(names(user_md), list_declaration_metadata_fields())

  if (length(offending) > 0) {
    setters <- vapply(
      offending,
      function(field) {
        # The index's setter is named for the concept, not the field.
        if (identical(field, "variables_index")) {
          "set_index"
        } else {
          paste0("set_", field)
        }
      },
      character(1)
    )
    cli::cli_abort(c(
      # Message strings are code, not comments, so they must stay ASCII:
      # R CMD check warns on non-ASCII in R sources, and CI errors on
      # warnings.
      "{.fn set_metadata} cannot write {.field {offending}} directly.",
      "i" = "{cli::qty(offending)}{?This field declares/These fields declare} which columns carry identity, time, position and events. Writing {cli::qty(offending)}{?it/them} here would leave the metadata naming columns the frame may not have, and the frame ordered and grouped as it was before.",
      "i" = "Use {.fn {setters}} instead, which validate the columns exist and restructure the frame to match.",
      "i" = "A complete metadata object can still be restored wholesale, as in {.code set_metadata(data, metadata = get_metadata(x))}."
    ))
  }

  invisible(TRUE)
}


#' Coerce a metadata value to the factor its field expects
#'
#' @param value Character or factor value.
#' @param field Name of the metadata field.
#'
#' @return A factor with the field's full set of levels.
#' @keywords internal
as_metadata_factor <- function(value, field) {
  factor(as.character(value), levels = levels(list_default_metadata()[[field]]))
}


#' Set metadata
#'
#' @description
#' Sets or updates metadata for an aniframe or anievent object. Metadata can
#' be provided either as named arguments or as a list. If the object already
#' has metadata, the new values will be merged with existing values, with new
#' values taking precedence.
#'
#' Character values for factor fields will be automatically converted to factors
#' if they match allowed levels.
#'
#' Default metadata fields include:
#' * `source`: Data source identifier
#' * `source_version`: Version of the software that wrote the file, where
#'   the file states one
#' * `source_format`: The export layout the file was read as
#' * `filename`: Original filename(s) — accepts a character vector
#'   (length 1 or more) for readers that load from multiple files
#' * `sampling_rate`: Sampling rate in Hz
#' * `start_datetime`: Start date and time of recording
#' * `reference_frame`: Reference frame (default: "allocentric")
#' * `coordinate_system`: Coordinate system (default: "cartesian")
#' * `axis_directions`: Which way each axis points, keyed by axis role
#' * `axis_extents`: How far each axis runs, keyed by axis role
#'
#' @param data An aniframe or anievent object.
#' @param ... Named metadata values (e.g., `sampling_rate = 30, source = "sleap"`)
#' @param metadata Alternatively, a named list of metadata. Cannot be used
#'   simultaneously with `...`
#'
#' @return The object with updated metadata.
#'
#' @seealso [get_metadata()], [list_default_metadata()]
#'
#' @examples
#' \dontrun{
#' # Set metadata using named arguments
#' data <- set_metadata(data, sampling_rate = 30, source = "sleap")
#'
#' # Set metadata using a list
#' md <- list(sampling_rate = 30, source = "sleap")
#' data <- set_metadata(data, metadata = md)
#' }
#'
#' @export
set_metadata <- function(data, ..., metadata = NULL) {
  # ------------------------------------------------------------------
  # Process the inputs
  # ------------------------------------------------------------------
  dot_args <- list(...)

  # Ensure that the user provides input with *either* ... or a metadata list
  if (!is.null(metadata) && !rlang::is_empty(dot_args)) {
    cli::cli_abort(
      "Metadata input can only be provided as either name-value pairs *or* a list through the {.arg metadata} parameter, not both."
    )
  } else if (!is.null(metadata)) {
    user_md <- metadata
  } else if (!rlang::is_empty(dot_args)) {
    ensure_is_list(dot_args)
    user_md <- dot_args
  } else {
    user_md <- list()
  }

  # ------------------------------------------------------------------
  # Refuse the fields that have their own setters
  # ------------------------------------------------------------------
  ensure_no_declaration_fields(user_md)

  # ------------------------------------------------------------------
  # Convert character values to factors where appropriate
  # ------------------------------------------------------------------
  if (length(user_md) > 0) {
    names_md <- names(user_md)
    defaults <- list_default_metadata()

    for (n in names_md) {
      # Check if this field exists in defaults and should be a factor
      if (n %in% names(defaults) && is.factor(defaults[[n]])) {
        # If the user provided a character, try to convert to factor
        if (is.character(user_md[[n]])) {
          # Check if it's a valid level
          if (!user_md[[n]] %in% levels(defaults[[n]])) {
            cli::cli_abort(
              "Metadata field {.field {n}} can only be {.val {levels(defaults[[n]])}} not {.val {user_md[[n]]}}."
            )
          }
          # Convert to factor with correct levels
          user_md[[n]] <- factor(
            user_md[[n]],
            levels = levels(defaults[[n]])
          )
        } else if (is.factor(user_md[[n]])) {
          # If already a factor, check if it's a valid level
          if (!as.character(user_md[[n]]) %in% levels(defaults[[n]])) {
            cli::cli_abort(
              "Metadata field {.field {n}} can only be {.val {levels(defaults[[n]])}} not {.val {as.character(user_md[[n]])}}."
            )
          }
          # Ensure it has the correct levels
          user_md[[n]] <- factor(
            as.character(user_md[[n]]),
            levels = levels(defaults[[n]])
          )
        }
      } else if (n %in% names(defaults) && is_class(defaults[[n]], "POSIXct")) {
        if (length(user_md[[n]]) == 1 && is.na(user_md[[n]])) {
          # Convert NA to POSIXct NA to maintain correct class
          user_md[[n]] <- as.POSIXct(NA_character_)
        } else {
          user_md[[n]] <- anytime::anytime(user_md[[n]])
        }
      }
    }
  }

  # ------------------------------------------------------------------
  # Does the data have metadata or not?
  # ------------------------------------------------------------------
  if (!has_metadata(data)) {
    new_md <- list_default_metadata()
  } else {
    new_md <- get_metadata(data)
  }

  # ------------------------------------------------------------------
  # Combine and attach metadata
  # ------------------------------------------------------------------
  # `utils::modifyList()` recurses into list-valued entries, which means a
  # field whose value is a list of data.frames (e.g. `connections`) would
  # be merged row-wise and break. Replace list-valued fields directly,
  # then merge the rest with `modifyList`.
  list_valued <- names(user_md)[vapply(user_md, is.list, logical(1))]
  for (k in list_valued) {
    new_md[[k]] <- user_md[[k]]
  }
  user_md[list_valued] <- NULL

  new_md <- utils::modifyList(new_md, user_md)
  data <- write_metadata(data, new_md)

  # TODO: Figure out whether it makes sense to include these special cases in the aniframe package

  # has_sr   <- "sampling_rate" %in% names(user_md)
  # sr_new   <- if (has_sr) user_md[["sampling_rate"]] else NULL
  # if (has_sr) user_md[["sampling_rate"]] <- NULL
  # if (has_sr) {
  #   data <- calibrate_time(data, sampling_rate = sr_new)
  # }

  data
}
