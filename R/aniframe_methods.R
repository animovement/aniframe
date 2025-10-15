# Methods for aniframe class to preserve class through dplyr operations

# ---- dplyr verb methods ----

#' Ungroup an aniframe
#'
#' @param x An aniframe object
#' @param ... Additional arguments passed to dplyr::ungroup
#' @return An ungrouped aniframe
#' @export
ungroup.aniframe <- function(x, ...) {
  # if (!quiet) {
  cli::cli_warn(
    "Ungrouping an aniframe data frame makes errors more likely. Proceed with care."
  )
  # }
  md <- get_metadata(x)
  class(x) <- setdiff(class(x), "aniframe")
  x <- NextMethod()
  x <- new_aniframe(x)
  x <- set_metadata(x, metadata = md)
  x
}

#' Group an aniframe
#'
#' @param .data An aniframe object
#' @param ... Variables to group by
#' @return A grouped aniframe
#' @export
group_by.aniframe <- function(.data, ...) {
  md <- get_metadata(.data)
  x <- NextMethod()
  x <- new_aniframe(x)
  x <- set_metadata(x, metadata = md)
  x
}

#' Mutate columns in an aniframe
#'
#' @param .data An aniframe object
#' @param ... Name-value pairs of expressions
#' @return An aniframe with modified columns
#' @export
mutate.aniframe <- function(.data, ...) {
  md <- get_metadata(.data)
  x <- NextMethod()
  x <- new_aniframe(x)
  x <- set_metadata(x, metadata = md)
  x
}

#' Select columns from an aniframe
#'
#' @param .data An aniframe object
#' @param ... Columns to select
#' @return An aniframe with selected columns
#' @export
select.aniframe <- function(.data, ...) {
  md <- get_metadata(.data)
  x <- NextMethod()
  x <- new_aniframe(x)
  x <- set_metadata(x, metadata = md)
  x
}

#' Filter rows of an aniframe
#'
#' @param .data An aniframe object
#' @param ... Logical predicates
#' @param .preserve Keep group structure
#' @return A filtered aniframe
#' @export
filter.aniframe <- function(.data, ..., .preserve = FALSE) {
  md <- get_metadata(.data)
  x <- NextMethod()
  x <- new_aniframe(x)
  x <- set_metadata(x, metadata = md)
  x
}

#' Arrange rows of an aniframe
#'
#' @param .data An aniframe object
#' @param ... Variables to order by
#' @param .by_group If TRUE, arrange within groups
#' @return An arranged aniframe
#' @export
arrange.aniframe <- function(.data, ..., .by_group = FALSE) {
  md <- get_metadata(.data)
  x <- NextMethod()
  x <- new_aniframe(x)
  x <- set_metadata(x, metadata = md)
  x
}

#' Rename columns in an aniframe
#'
#' @param .data An aniframe object
#' @param ... Name-value pairs for renaming
#' @return An aniframe with renamed columns
#' @export
rename.aniframe <- function(.data, ...) {
  md <- get_metadata(.data)
  x <- NextMethod()
  x <- new_aniframe(x)
  x <- set_metadata(x, metadata = md)
  x
}

#' Relocate columns in an aniframe
#'
#' @param .data An aniframe object
#' @param ... Columns to relocate
#' @return An aniframe with relocated columns
#' @export
relocate.aniframe <- function(.data, ...) {
  md <- get_metadata(.data)
  class(.data) <- setdiff(class(.data), "aniframe")
  x <- NextMethod()
  x <- new_aniframe(x)
  x <- set_metadata(x, metadata = md)
  x
}

#' Slice rows from an aniframe
#'
#' @param .data An aniframe object
#' @param ... Integer row positions
#' @param .preserve Keep group structure
#' @return A sliced aniframe
#' @export
slice.aniframe <- function(.data, ..., .preserve = FALSE) {
  md <- get_metadata(.data)
  x <- NextMethod()
  x <- new_aniframe(x)
  x <- set_metadata(x, metadata = md)
  x
}

# ---- Base R extraction methods ----

#' Subset aniframe with [
#'
#' @param x An aniframe object
#' @param i Row indices
#' @param j Column indices
#' @param ... Additional arguments
#' @param drop If TRUE, simplify to vector when possible
#' @return A subset aniframe
#' @export
`[.aniframe` <- function(x, i, j, ..., drop = FALSE) {
  md <- get_metadata(x)
  class(x) <- setdiff(class(x), "aniframe")
  x <- NextMethod()
  x <- new_aniframe(x)
  x <- set_metadata(x, metadata = md)
  x
}

#' Extract single column from aniframe with [[
#'
#' @param x An aniframe object
#' @param i Column index or name
#' @param ... Additional arguments
#' @return A vector or data frame
#' @export
`[[.aniframe` <- function(x, i, ...) {
  md <- get_metadata(x)
  class(x) <- setdiff(class(x), "aniframe")
  x <- NextMethod()
  if (is.data.frame(x)) {
    x <- new_aniframe(x)
    x <- set_metadata(x, metadata = md)
  }
  x
}

#' Extract column from aniframe with $
#'
#' @param x An aniframe object
#' @param name Column name
#' @return A vector
#' @export
`$.aniframe` <- function(x, name) {
  class(x) <- setdiff(class(x), "aniframe")
  NextMethod()
}

# ---- Assignment methods ----

#' Subset assignment for aniframe with [<-
#'
#' @param x An aniframe object
#' @param i Row indices
#' @param j Column indices
#' @param ... Additional arguments
#' @param value Replacement values
#' @return Modified aniframe
#' @export
`[<-.aniframe` <- function(x, i, j, ..., value) {
  md <- get_metadata(x)
  class(x) <- setdiff(class(x), "aniframe")
  x <- NextMethod()
  x <- new_aniframe(x)
  x <- set_metadata(x, metadata = md)
  x
}

#' Column assignment for aniframe with [[<-
#'
#' @param x An aniframe object
#' @param i Column index or name
#' @param ... Additional arguments
#' @param value Replacement value
#' @return Modified aniframe
#' @export
`[[<-.aniframe` <- function(x, i, ..., value) {
  md <- get_metadata(x)
  class(x) <- setdiff(class(x), "aniframe")
  x <- NextMethod()
  x <- new_aniframe(x)
  x <- set_metadata(x, metadata = md)
  x
}

#' Column assignment for aniframe with $<-
#'
#' @param x An aniframe object
#' @param name Column name
#' @param value Replacement value
#' @return Modified aniframe
#' @export
`$<-.aniframe` <- function(x, name, value) {
  md <- get_metadata(x)
  class(x) <- setdiff(class(x), "aniframe")
  x <- NextMethod()
  x <- new_aniframe(x)
  x <- set_metadata(x, metadata = md)
  x
}

#' Rename columns with names<-
#'
#' @param x An aniframe object
#' @param value New column names
#' @return Modified aniframe
#' @export
`names<-.aniframe` <- function(x, value) {
  md <- get_metadata(x)
  class(x) <- setdiff(class(x), "aniframe")
  x <- NextMethod()
  x <- new_aniframe(x)
  x <- set_metadata(x, metadata = md)
  x
}

# ---- Conversion methods ----

#' Convert aniframe to regular data frame
#'
#' @param x An aniframe object
#' @param ... Additional arguments
#' @return A regular data frame
#' @export
as.data.frame.aniframe <- function(x, ...) {
  class(x) <- setdiff(class(x), "aniframe")
  NextMethod()
}
