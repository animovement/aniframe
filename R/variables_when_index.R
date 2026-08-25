# The index (#109)
#
# `variables_when` used to do two jobs at once: name the column the frame
# is indexed by, and name the surrounding temporal context. The two were
# told apart by the literal string "time", which forced every frame to
# have a column of that name and made the distinction unrecoverable
# downstream without repeating the same literal.
#
# `index` names the column instead. It is one of the `variables_when`; the
# context is the rest of them.

#' The column an aniframe is indexed by
#'
#' Exactly one column, of any name, holding the position of each row
#' within its trajectory. It is always one of the `variables_when`; the
#' rest of that vector is the surrounding temporal context — session,
#' trial, observation — and is what the frame is grouped by.
#'
#' @param data An aniframe or anievent object.
#'
#' @return Length-one character vector naming the index column.
#'
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' get_index(af)
#'
#' @seealso [set_index()] to change it, [get_variables_when()] for the
#'   full set of temporal columns.
#' @export
get_index <- function(data) {
  ensure_is_aniframe_or_anievent(data)
  resolve_index(get_metadata(data))
}


#' Resolve the index from a metadata list
#'
#' Objects serialised before the field existed have no `index`. They were
#' built when a literal `time` column was mandatory, so that is what they
#' are indexed by, and defaulting here keeps them working untouched.
#'
#' @param md A metadata list.
#'
#' @return Length-one character vector.
#' @keywords internal
resolve_index <- function(md) {
  idx <- md[["variables_when_index"]]
  if (is.null(idx) || length(idx) != 1L || is.na(idx)) {
    return("time")
  }
  as.character(idx)
}


#' Declare which column an aniframe is indexed by
#'
#' Changing the index changes what the frame is grouped by and the order
#' its rows come in, so — like the `variables_*` declarations — it is not
#' reachable through [set_metadata()] and has its own setter, which does
#' the restructuring too.
#'
#' The column is added to `variables_when` if it is not already there.
#'
#' @param data An aniframe object.
#' @param column Length-one character vector naming the index column. It
#'   must exist in `data` and be numeric.
#'
#' @return `data`, re-indexed and restructured.
#'
#' @examples
#' df <- data.frame(frame = 1:3, individual = "a", x = c(1, 2, 3), y = c(0, 1, 0))
#' af <- as_aniframe(df, index = "frame")
#' get_index(af)
#'
#' @seealso [get_index()]
#' @export
set_index <- function(data, column) {
  ensure_is_aniframe(data)
  ensure_valid_index(data, column)

  md <- get_metadata(data)
  md[["variables_when_index"]] <- column
  data <- attach_metadata(data, md)

  # The index is one of the temporal variables, and declaring it restructures
  # the frame: it decides both the grouping and the within-group row order.
  declare_variables(
    data,
    "when",
    union(get_variables(data, "when"), column)
  )
}


#' Ensure a proposed index is usable
#'
#' @param data An aniframe object.
#' @param column The proposed index column.
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_valid_index <- function(data, column) {
  if (!is.character(column) || length(column) != 1L || is.na(column)) {
    cli::cli_abort(c(
      "{.arg column} must be a single column name.",
      "i" = "A frame has exactly one index."
    ))
  }
  if (!column %in% names(data)) {
    cli::cli_abort(
      "Column {.val {column}} is not present in the data."
    )
  }
  if (!is.numeric(data[[column]])) {
    cli::cli_abort(
      "Index column {.val {column}} must be numeric, not {.cls {class(data[[column]])}}."
    )
  }
  invisible(TRUE)
}


#' Ensure a `when` declaration still contains the index
#'
#' `variables_when_index` names one of the `variables_when`. Declaring a
#' set that leaves it out would leave the metadata pointing at a column the
#' frame no longer declares as temporal — the same desynchronisation that
#' dedicated setters exist to prevent (#82).
#'
#' @param data An aniframe object.
#' @param when The proposed `variables_when` declaration.
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_index_declared <- function(data, when) {
  index <- resolve_index(get_metadata(data))
  if (!index %in% when) {
    cli::cli_abort(c(
      "{.field variables_when} must include the index column {.val {index}}.",
      "i" = "The index is one of the temporal variables, not a separate declaration.",
      "i" = "To index the frame by a different column, use {.fn set_index}, which moves both."
    ))
  }
  invisible(TRUE)
}
