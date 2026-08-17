# Methods for the anievent class to preserve class through dplyr and
# base-R operations. Mirrors `aniframe_methods.R`; the capture /
# re-attach pattern is shared via `preserve_animovement_class()`, which
# restores the class vector captured before dispatch so downstream
# subclasses survive too (#81).

# ---- dplyr verb methods ----

#' Ungroup an anievent
#'
#' @param x An anievent object.
#' @param ... Additional arguments passed to dplyr::ungroup.
#' @return An ungrouped anievent.
#' @keywords internal
#' @export
ungroup.anievent <- function(x, ...) {
  cli::cli_warn(
    "Ungrouping an anievent data frame makes errors more likely. Proceed with care."
  )
  cls <- class(x)
  md <- get_metadata(x)
  class(x) <- setdiff(class(x), "anievent")
  x <- NextMethod()
  preserve_animovement_class(x, cls, md)
}

#' Group an anievent
#'
#' @param .data An anievent object.
#' @param ... Variables to group by.
#' @return A grouped anievent.
#' @keywords internal
#' @export
group_by.anievent <- function(.data, ...) {
  cls <- class(.data)
  md <- get_metadata(.data)
  x <- NextMethod()
  preserve_animovement_class(x, cls, md)
}

#' Mutate columns in an anievent
#'
#' @param .data An anievent object.
#' @param ... Name-value pairs of expressions.
#' @return An anievent with modified columns.
#' @keywords internal
#' @export
mutate.anievent <- function(.data, ...) {
  cls <- class(.data)
  md <- get_metadata(.data)
  x <- NextMethod()
  preserve_animovement_class(x, cls, md)
}

#' Select columns from an anievent
#'
#' @param .data An anievent object.
#' @param ... Columns to select.
#' @return An anievent with selected columns.
#' @keywords internal
#' @export
select.anievent <- function(.data, ...) {
  cls <- class(.data)
  md <- get_metadata(.data)
  x <- NextMethod()
  preserve_animovement_class(x, cls, md)
}

#' Filter rows of an anievent
#'
#' @param .data An anievent object.
#' @param ... Logical predicates.
#' @param .preserve Keep group structure.
#' @return A filtered anievent.
#' @keywords internal
#' @export
filter.anievent <- function(.data, ..., .preserve = FALSE) {
  cls <- class(.data)
  md <- get_metadata(.data)
  x <- NextMethod()
  preserve_animovement_class(x, cls, md)
}

#' Arrange rows of an anievent
#'
#' @param .data An anievent object.
#' @param ... Variables to order by.
#' @param .by_group If TRUE, arrange within groups.
#' @return An arranged anievent.
#' @keywords internal
#' @export
arrange.anievent <- function(.data, ..., .by_group = FALSE) {
  cls <- class(.data)
  md <- get_metadata(.data)
  x <- NextMethod()
  preserve_animovement_class(x, cls, md)
}

#' Rename columns in an anievent
#'
#' @param .data An anievent object.
#' @param ... Name-value pairs for renaming.
#' @return An anievent with renamed columns.
#' @keywords internal
#' @export
rename.anievent <- function(.data, ...) {
  cls <- class(.data)
  md <- get_metadata(.data)
  x <- NextMethod()
  preserve_animovement_class(x, cls, md)
}

#' Relocate columns in an anievent
#'
#' @param .data An anievent object.
#' @param ... Columns to relocate.
#' @return An anievent with relocated columns.
#' @keywords internal
#' @export
relocate.anievent <- function(.data, ...) {
  cls <- class(.data)
  md <- get_metadata(.data)
  class(.data) <- setdiff(class(.data), "anievent")
  x <- NextMethod()
  preserve_animovement_class(x, cls, md)
}

#' Slice rows from an anievent
#'
#' @param .data An anievent object.
#' @param ... Integer row positions.
#' @param .preserve Keep group structure.
#' @return A sliced anievent.
#' @keywords internal
#' @export
slice.anievent <- function(.data, ..., .preserve = FALSE) {
  cls <- class(.data)
  md <- get_metadata(.data)
  x <- NextMethod()
  preserve_animovement_class(x, cls, md)
}

# ---- Base R extraction methods ----

#' Subset anievent with [
#'
#' @param x An anievent object.
#' @param i Row indices.
#' @param j Column indices.
#' @param ... Additional arguments.
#' @param drop If TRUE, simplify to vector when possible.
#' @return A subset anievent.
#' @keywords internal
#' @export
`[.anievent` <- function(x, i, j, ..., drop = FALSE) {
  cls <- class(x)
  md <- get_metadata(x)
  class(x) <- setdiff(class(x), "anievent")
  x <- NextMethod()
  preserve_animovement_class(x, cls, md)
}

#' Extract single column from anievent with [[
#'
#' @param x An anievent object.
#' @param i Column index or name.
#' @param ... Additional arguments.
#' @return A vector or data frame.
#' @keywords internal
#' @export
`[[.anievent` <- function(x, i, ...) {
  cls <- class(x)
  md <- get_metadata(x)
  class(x) <- setdiff(class(x), "anievent")
  x <- NextMethod()
  if (is.data.frame(x)) {
    # nocov start
    x <- preserve_animovement_class(x, cls, md)
  } # nocov end
  x
}

#' Extract column from anievent with $
#'
#' @param x An anievent object.
#' @param name Column name.
#' @return A vector.
#' @keywords internal
#' @export
`$.anievent` <- function(x, name) {
  class(x) <- setdiff(class(x), "anievent")
  NextMethod()
}

# ---- Assignment methods ----

#' Subset assignment for anievent with [<-
#'
#' @param x An anievent object.
#' @param i Row indices.
#' @param j Column indices.
#' @param ... Additional arguments.
#' @param value Replacement values.
#' @return Modified anievent.
#' @keywords internal
#' @export
`[<-.anievent` <- function(x, i, j, ..., value) {
  cls <- class(x)
  md <- get_metadata(x)
  class(x) <- setdiff(class(x), "anievent")
  x <- NextMethod()
  preserve_animovement_class(x, cls, md)
}

#' Column assignment for anievent with [[<-
#'
#' @param x An anievent object.
#' @param i Column index or name.
#' @param ... Additional arguments.
#' @param value Replacement value.
#' @return Modified anievent.
#' @keywords internal
#' @export
`[[<-.anievent` <- function(x, i, ..., value) {
  cls <- class(x)
  md <- get_metadata(x)
  class(x) <- setdiff(class(x), "anievent")
  x <- NextMethod()
  preserve_animovement_class(x, cls, md)
}

#' Column assignment for anievent with $<-
#'
#' @param x An anievent object.
#' @param name Column name.
#' @param value Replacement value.
#' @return Modified anievent.
#' @keywords internal
#' @export
`$<-.anievent` <- function(x, name, value) {
  cls <- class(x)
  md <- get_metadata(x)
  class(x) <- setdiff(class(x), "anievent")
  x <- NextMethod()
  preserve_animovement_class(x, cls, md)
}

#' Rename columns with names<-
#'
#' @param x An anievent object.
#' @param value New column names.
#' @return Modified anievent.
#' @keywords internal
#' @export
`names<-.anievent` <- function(x, value) {
  cls <- class(x)
  md <- get_metadata(x)
  class(x) <- setdiff(class(x), "anievent")
  x <- NextMethod()
  preserve_animovement_class(x, cls, md)
}

# ---- Conversion methods ----

#' Convert anievent to regular data frame
#'
#' @param x An anievent object.
#' @param ... Additional arguments.
#' @return A regular data frame.
#' @keywords internal
#' @export
as.data.frame.anievent <- function(x, ...) {
  class(x) <- setdiff(class(x), "anievent")
  NextMethod()
}
