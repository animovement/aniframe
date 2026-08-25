# Declaring the structural variables (#82)
#
# `variables_what`, `variables_when` and `variables_where` are not
# ordinary metadata — they are the frame's structure. They decide how
# columns are typed, which order columns and rows come in, what the frame
# is grouped by, and (from `variables_where`) the `coordinate_system`.
#
# Writing them without redoing that work leaves the frame and its own
# description disagreeing: the print header updates, so it looks like it
# worked, while the grouping still reflects the old declaration. They
# therefore get dedicated setters that do the whole job, and
# `set_metadata()` refuses them.

#' The metadata fields that declare which columns carry which role
#'
#' Writing any of these has consequences beyond the metadata list — at
#' the least the named columns must exist, and for the three structural
#' roles the frame is retyped, reordered and regrouped to match — so they
#' are reachable only through their own setters.
#'
#' @return Character vector of metadata field names.
#' @keywords internal
declaration_metadata_fields <- function() {
  c(
    "variables_what",
    "variables_when",
    "variables_where",
    "variables_event",
    "variables_when_index"
  )
}


#' Read a variable role from the metadata
#'
#' @param data An aniframe or anievent object.
#' @param role One of `"what"`, `"when"`, `"where"`.
#'
#' @return Character vector of column names.
#' @keywords internal
get_variables <- function(data, role) {
  as.character(get_metadata(data, paste0("variables_", role)))
}


#' Declare one variable role and restructure the frame to match
#'
#' The shared kernel behind the `set_` / `add_` / `remove_` functions.
#' Reads the other two roles from the metadata so the frame is always
#' restructured against a complete, consistent declaration.
#'
#' @param data An aniframe or anievent object.
#' @param role One of `"what"`, `"when"`, `"where"`.
#' @param variables Character vector of column names to declare.
#'
#' @return `data`, restructured and re-declared.
#' @keywords internal
declare_variables <- function(data, role, variables) {
  ensure_is_aniframe_or_anievent(data)
  ensure_variables_chr(variables)

  declared <- list(
    what = get_variables(data, "what"),
    when = get_variables(data, "when"),
    where = get_variables(data, "where")
  )
  declared[[role]] <- unname(variables)

  restructure_frame(data, declared$what, declared$when, declared$where)
}


#' Ensure a declaration is a character vector
#'
#' Guards the `add_` / `remove_` paths in particular, where `union()` and
#' `setdiff()` would otherwise silently coerce.
#'
#' @param variables Value supplied by the caller.
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_variables_chr <- function(variables) {
  if (!is.character(variables)) {
    cli::cli_abort(
      "{.arg variables} must be a character vector, not {.cls {class(variables)}}."
    )
  }
  invisible(TRUE)
}


#' Ensure the object is one of the animovement frame classes
#'
#' @param data Object to test.
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_is_aniframe_or_anievent <- function(data) {
  if (!is_aniframe(data) && !is_anievent(data)) {
    cli::cli_abort("Data is neither an aniframe nor an anievent.")
  }
  invisible(TRUE)
}


#' Restructure a frame to match a declaration
#'
#' Dispatches to the per-class restructure. The two classes share the
#' metadata substrate but not their layout: an aniframe is grouped and
#' ordered by identity then time, an anievent is ordered by identity then
#' bout start and is never grouped.
#'
#' @param data An aniframe or anievent object.
#' @param variables_what,variables_when,variables_where The full
#'   declaration to apply.
#'
#' @return `data`, restructured, with the declaration recorded.
#' @keywords internal
restructure_frame <- function(
  data,
  variables_what,
  variables_when,
  variables_where
) {
  if (is_anievent(data)) {
    if (length(variables_where) > 0) {
      cli::cli_abort(c(
        "An {.cls anievent} has no spatial variables.",
        "i" = "{.field variables_where} is always empty on an anievent; spatial position lives on the {.cls aniframe} it was encoded from."
      ))
    }
    return(restructure_anievent(data, variables_what, variables_when))
  }

  restructure_aniframe(data, variables_what, variables_when, variables_where)
}


#' Strip a frame back to its dplyr classes
#'
#' The structural steps operate on a plain frame, so they neither
#' dispatch back into the class-preserving methods nor trigger the
#' `ungroup()` "use with care" warning when a declaration leaves nothing
#' to group by.
#'
#' @param data An aniframe or anievent object.
#'
#' @return `data` with the animovement classes removed.
#' @keywords internal
strip_animovement_class <- function(data) {
  class(data) <- intersect(class(data), base_frame_classes())
  data
}


#' Restructure an aniframe
#'
#' The tail of [as_aniframe()], factored out so that construction and
#' re-declaration cannot drift apart: validate the declared columns
#' exist, standardise their types, relocate, arrange, regroup, and
#' refresh the derived `coordinate_system`.
#'
#' @param data An aniframe object.
#' @param variables_what,variables_when,variables_where The declaration
#'   to apply.
#'
#' @return `data`, restructured, with the declaration recorded.
#' @keywords internal
restructure_aniframe <- function(
  data,
  variables_what,
  variables_when,
  variables_where
) {
  cls <- class(data)
  md <- get_metadata(data)
  index <- resolve_index(md)
  bare <- strip_animovement_class(data)

  # The index is declared separately and is never one of the context
  # variables. Normalising here rather than at each caller keeps frames
  # built before the field existed coherent too: their `variables_when`
  # still lists the index column, and grouping by it would put every row
  # in its own group.
  variables_when <- setdiff(variables_when, index)

  ensure_aniframe_cols(
    bare,
    variables_what,
    variables_when,
    variables_where,
    index
  )
  bare <- standardise_aniframe_cols(
    bare,
    variables_what,
    variables_when,
    variables_where,
    index
  )

  # Column order: what, when, index, where, confidence, everything else.
  standard_cols <- unique(
    c(variables_what, variables_when, index, variables_where)
  )
  if ("confidence" %in% names(bare)) {
    standard_cols <- c(standard_cols, "confidence")
  }
  bare <- bare[, c(standard_cols, setdiff(names(bare), standard_cols))]

  # Order by identity, then temporal context, then position within it —
  # the index sorts last, which keeps each trajectory contiguous.
  bare <- dplyr::arrange(
    bare,
    dplyr::across(dplyr::all_of(variables_what)),
    dplyr::across(dplyr::all_of(c(variables_when, index)))
  )

  # Group by identity + temporal context. `variables_when` is exactly the
  # context now, so there is nothing to exclude — the index is not in it.
  grouping_vars <- c(variables_what, variables_when)
  bare <- regroup_frame(bare, grouping_vars)

  md$variables_what <- variables_what
  md$variables_when <- variables_when
  md$variables_where <- variables_where
  md$variables_when_index <- index
  md$coordinate_system <- as_metadata_factor(
    infer_coordinate_system(variables_where),
    "coordinate_system"
  )

  preserve_animovement_class(bare, cls, md)
}


#' Restructure an anievent
#'
#' The anievent counterpart to [restructure_aniframe()]: validate,
#' standardise types, relocate, and order by identity then bout start.
#' An anievent is not grouped.
#'
#' @param data An anievent object.
#' @param variables_what,variables_when The declaration to apply.
#'
#' @return `data`, restructured, with the declaration recorded.
#' @keywords internal
restructure_anievent <- function(data, variables_what, variables_when) {
  cls <- class(data)
  md <- get_metadata(data)
  bare <- strip_animovement_class(data)

  ensure_anievent_cols(bare)
  ensure_declared_cols_exist(bare, variables_what, "what")
  ensure_declared_cols_exist(
    bare,
    setdiff(variables_when, c("start", "stop")),
    "when"
  )
  bare <- standardise_anievent_cols(bare, variables_what, variables_when)

  event_cols <- c("channel", "type", "label")
  if ("modifiers" %in% names(bare)) {
    event_cols <- c(event_cols, "modifiers")
  }
  standard_cols <- c(variables_what, variables_when, event_cols)
  bare <- bare[, c(standard_cols, setdiff(names(bare), standard_cols))]

  when_grouping <- setdiff(variables_when, c("start", "stop"))
  bare <- dplyr::arrange(
    bare,
    dplyr::across(dplyr::all_of(c(variables_what, when_grouping))),
    .data$start
  )

  md$variables_what <- variables_what
  md$variables_when <- variables_when
  # An anievent carries no spatial variables — position lives on the
  # aniframe it was encoded from.
  md$variables_where <- character()

  preserve_animovement_class(bare, cls, md)
}


#' Group a frame by the given columns, or ungroup it when there are none
#'
#' @param data A plain (non-animovement) data frame.
#' @param grouping_vars Character vector of columns to group by.
#'
#' @return `data`, grouped or ungrouped.
#' @keywords internal
regroup_frame <- function(data, grouping_vars) {
  if (length(grouping_vars) == 0) {
    return(dplyr::ungroup(data))
  }

  suppressWarnings(
    dplyr::group_by(data, dplyr::across(dplyr::all_of(grouping_vars)))
  )
}


#' Ensure declared columns are present
#'
#' Shared by construction ([ensure_aniframe_cols()]) and re-declaration,
#' so a column that isn't there is reported the same way whichever route
#' the caller took.
#'
#' @param data A data frame.
#' @param cols Character vector of declared column names.
#' @param role One of `"what"`, `"when"`, `"where"`.
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_declared_cols_exist <- function(data, cols, role) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) == 0) {
    return(invisible(TRUE))
  }

  lead <- switch(
    role,
    what = "Identity variable{?s} not found in data",
    when = "Temporal variable{?s} not found in data",
    where = "Missing spatial variable{?s}",
    event = "Event variable{?s} not found in data"
  )

  cli::cli_abort(c(
    paste0(lead, ": {.val {missing_cols}}."),
    "i" = "Create the column first, then declare it."
  ))
}


# ------------------------------------------------------------------
# Public API
# ------------------------------------------------------------------

#' Declare which columns carry identity, time and position
#'
#' @description
#' `variables_what`, `variables_when` and `variables_where` name the
#' columns that carry, respectively, entity identity, temporal position
#' and spatial position. They are the frame's structure rather than a
#' description of it: [as_aniframe()] uses them to coerce column types,
#' order columns and rows, group the frame, and derive
#' `coordinate_system`.
#'
#' These functions declare them *and* restructure the frame to match, so
#' the two cannot drift apart. [set_metadata()] refuses these three
#' fields for that reason.
#'
#' * `set_variables_*()` replaces the declaration.
#' * `add_variables_*()` appends to it — the common case, and one that
#'   avoids the footgun of having to restate the existing variables.
#' * `remove_variables_*()` drops from it.
#' * `get_variables_*()` reads it.
#'
#' The column must exist before it can be declared, so the order is
#' always create-then-declare:
#'
#' ```r
#' data |>
#'   dplyr::mutate(id = "hi") |>
#'   add_variables_what("id")
#' ```
#'
#' @param data An aniframe or anievent object.
#' @param variables Character vector of column names.
#'
#' @return For the setters, `data` restructured and re-declared. For the
#'   getters, a character vector of column names.
#'
#' @seealso [validate_aniframe()], which reports a frame whose metadata
#'   has drifted out of sync by some other route.
#'
#' @examples
#' af <- aniframe(time = 1:5, x = 1:5, y = 1:5)
#'
#' # Declaring an identity column groups the frame by it
#' af |>
#'   dplyr::mutate(id = "a") |>
#'   add_variables_what("id") |>
#'   dplyr::group_vars()
#'
#' # Declaring a third spatial column refreshes coordinate_system
#' af |>
#'   dplyr::mutate(z = 0) |>
#'   add_variables_where("z") |>
#'   get_metadata("coordinate_system")
#'
#' @name variables
NULL


#' @rdname variables
#' @export
get_variables_what <- function(data) {
  ensure_is_aniframe_or_anievent(data)
  get_variables(data, "what")
}

#' @rdname variables
#' @export
get_variables_when <- function(data) {
  ensure_is_aniframe_or_anievent(data)
  get_variables(data, "when")
}

#' @rdname variables
#' @export
get_variables_where <- function(data) {
  ensure_is_aniframe_or_anievent(data)
  get_variables(data, "where")
}

#' @rdname variables
#' @export
set_variables_what <- function(data, variables) {
  declare_variables(data, "what", variables)
}

#' @rdname variables
#' @export
set_variables_when <- function(data, variables) {
  declare_variables(data, "when", variables)
}

#' @rdname variables
#' @export
set_variables_where <- function(data, variables) {
  declare_variables(data, "where", variables)
}

#' @rdname variables
#' @export
add_variables_what <- function(data, variables) {
  ensure_is_aniframe_or_anievent(data)
  ensure_variables_chr(variables)
  declare_variables(data, "what", union(get_variables(data, "what"), variables))
}

#' @rdname variables
#' @export
add_variables_when <- function(data, variables) {
  ensure_is_aniframe_or_anievent(data)
  ensure_variables_chr(variables)

  # `variables_when` holds only the temporal context, so a new column
  # simply joins it — the index sorts after all of them regardless, and is
  # declared separately.
  declare_variables(data, "when", union(get_variables(data, "when"), variables))
}

#' @rdname variables
#' @export
add_variables_where <- function(data, variables) {
  ensure_is_aniframe_or_anievent(data)
  ensure_variables_chr(variables)
  declare_variables(
    data,
    "where",
    union(get_variables(data, "where"), variables)
  )
}

#' @rdname variables
#' @export
remove_variables_what <- function(data, variables) {
  ensure_is_aniframe_or_anievent(data)
  ensure_variables_chr(variables)
  declare_variables(
    data,
    "what",
    setdiff(get_variables(data, "what"), variables)
  )
}

#' @rdname variables
#' @export
remove_variables_when <- function(data, variables) {
  ensure_is_aniframe_or_anievent(data)
  ensure_variables_chr(variables)
  declare_variables(
    data,
    "when",
    setdiff(get_variables(data, "when"), variables)
  )
}

#' @rdname variables
#' @export
remove_variables_where <- function(data, variables) {
  ensure_is_aniframe_or_anievent(data)
  ensure_variables_chr(variables)
  declare_variables(
    data,
    "where",
    setdiff(get_variables(data, "where"), variables)
  )
}
