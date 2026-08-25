# Axis roles (#109)
#
# `variables_where` used to be a plain vector of column names, and the
# coordinate system was inferred by matching those names against a fixed
# list. The name *was* the role, so a frame whose coordinates were called
# anything else degraded to `unknown` and every spatial function refused it.
#
# It is now a mapping from axis role to column: `c(x = "u", y = "v")`. The
# set of roles stays closed — that is what makes `map_to_polar()` and unit
# conversion meaningful — while the spelling of the columns is free.

#' The axis role sets that define each coordinate system
#'
#' Closed by design. A transformation between coordinate systems is only
#' well defined because the roles on each side are known, so an
#' unrecognised role is rejected rather than accommodated.
#'
#' @return Named list mapping a comma-separated sorted role set to the
#'   coordinate system it defines.
#' @keywords internal
axis_role_sets <- function() {
  list(
    "x" = "cartesian_1d",
    "y" = "cartesian_1d",
    "z" = "cartesian_1d",
    "x,y" = "cartesian_2d",
    "x,z" = "cartesian_2d",
    "y,z" = "cartesian_2d",
    "x,y,z" = "cartesian_3d",
    "phi,rho" = "polar",
    "phi,rho,z" = "cylindrical",
    "phi,rho,theta" = "spherical"
  )
}


#' Every role any coordinate system recognises
#'
#' @return Character vector of role names.
#' @keywords internal
known_axis_roles <- function() {
  c("x", "y", "z", "rho", "phi", "theta")
}


#' Normalise a `variables_where` declaration into a role-to-column mapping
#'
#' An unnamed vector is the historical form, where the column name *is*
#' the role; it is read that way, which is what keeps every existing frame
#' and every reader's output working untouched.
#'
#' @param variables_where Character vector, optionally named by axis role.
#'
#' @return Named character vector: names are roles, values are columns.
#' @keywords internal
normalise_axes <- function(variables_where) {
  if (length(variables_where) == 0L) {
    return(stats::setNames(character(), character()))
  }
  nms <- names(variables_where)
  if (is.null(nms) || any(nms == "" | is.na(nms))) {
    # Unnamed, or partially named — the historical reading applies.
    return(stats::setNames(
      as.character(variables_where),
      as.character(variables_where)
    ))
  }
  stats::setNames(as.character(variables_where), nms)
}


#' Was this declaration written as an explicit role mapping?
#'
#' Explicit roles are validated strictly and an unrecognised one aborts.
#' A bare vector of column names keeps the older, lenient behaviour of
#' warning and falling back to `"unknown"`, because that is what readers
#' and existing frames rely on.
#'
#' @param variables_where The declaration as supplied.
#'
#' @return `TRUE` when every element carries a role name.
#' @keywords internal
axes_declared_by_role <- function(variables_where) {
  nms <- names(variables_where)
  length(variables_where) > 0L &&
    !is.null(nms) &&
    !any(nms == "" | is.na(nms))
}


#' Reject roles that no coordinate system defines
#'
#' Named by the offending role, at the point of declaration — as opposed
#' to silently degrading the frame to `"unknown"` and failing later in
#' whichever spatial function the user reaches for first.
#'
#' @param axes A normalised role-to-column mapping.
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_valid_axis_roles <- function(axes) {
  roles <- names(axes)

  unknown <- setdiff(roles, known_axis_roles())
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "{cli::qty(unknown)}{?Axis role/Axis roles} {.val {unknown}} {?is/are} not recognised.",
      "i" = "Roles are {.val {known_axis_roles()}}.",
      "i" = "The role set is closed so that transformations between coordinate systems stay well defined; the column names are free."
    ))
  }

  if (anyDuplicated(roles) > 0L) {
    dupes <- unique(roles[duplicated(roles)])
    cli::cli_abort(
      "{cli::qty(dupes)}{?Axis role/Axis roles} {.val {dupes}} {?is/are} declared more than once."
    )
  }

  if (!paste(sort(roles), collapse = ",") %in% names(axis_role_sets())) {
    cli::cli_abort(c(
      "Axis roles {.val {roles}} do not form a coordinate system.",
      "i" = "Recognised combinations are {.val {names(axis_role_sets())}}."
    ))
  }

  invisible(TRUE)
}


#' The axis roles of an aniframe, and the columns carrying them
#'
#' Where [get_variables_where()] gives the column names, this gives what
#' each of them *means*. Index by role to write a transformation that does
#' not care what the columns are called:
#' `data[[get_axes(data)[["x"]]]]`.
#'
#' @param data An aniframe object.
#'
#' @return Named character vector: names are axis roles (`x`, `y`, `z`,
#'   `rho`, `phi`, `theta`), values are the columns carrying them. Empty
#'   for a frame whose coordinate system is `"unknown"`.
#'
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' get_axes(af)
#'
#' df <- data.frame(time = 1:3, individual = "a", u = c(1, 2, 3), v = c(0, 1, 0))
#' renamed <- as_aniframe(df, variables_where = c(x = "u", y = "v"))
#' get_axes(renamed)
#' get_metadata(renamed, "coordinate_system")
#'
#' @seealso [get_variables_where()], [set_variables_where()]
#' @export
get_axes <- function(data) {
  ensure_is_aniframe(data)
  declared <- get_metadata(data, "variables_where")
  if (is.null(names(declared))) {
    return(stats::setNames(character(), character()))
  }
  normalise_axes(declared)
}
