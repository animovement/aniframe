# Coordinate-system predicates (#109)
#
# These used to match column *names* — `is_polar()` looked for columns
# literally called `rho` and `phi`. That was wrong in both directions once
# axis roles existed: a frame whose coordinates are called anything else
# was refused by every spatial function despite being polar, and a frame
# that still carried an undeclared `rho` column reported as spherical
# after `rho` had been dropped from the declaration.
#
# They read `coordinate_system` instead, which is derived from the axis
# roles on every construction and re-declaration, so the predicates and
# the metadata cannot disagree.

#' The coordinate system an aniframe is in
#'
#' Derived from the axis roles rather than declared: [set_axes()] says
#' which column carries which role, and the system follows from the set of
#' roles present. It is therefore not writable — see [set_axes()] to say
#' what the columns mean, or `anispace`'s `map_to_*()` functions to convert
#' the coordinates themselves.
#'
#' @param data An aniframe or anievent object.
#'
#' @return Length-one character vector: one of `"cartesian_1d"`,
#'   `"cartesian_2d"`, `"cartesian_3d"`, `"polar"`, `"cylindrical"`,
#'   `"spherical"` or `"unknown"`.
#'
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' get_coordinate_system(af)
#'
#' @seealso [get_axes()], [is_cartesian()], [is_polar()]
#' @export
get_coordinate_system <- function(data) {
  ensure_is_aniframe_or_anievent(data)
  as.character(get_metadata(data, "coordinate_system"))
}


#' Test whether an aniframe uses a Cartesian coordinate system
#'
#' Returns `TRUE` if the data frame satisfies *any* of the 1-D, 2-D or 3-D
#' Cartesian checks.
#'
#' @param data An aniframe.
#' @return A logical value.
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' is_cartesian(af)
#' @export
is_cartesian <- function(data) {
  startsWith(get_coordinate_system(data), "cartesian")
}


#' Internal guard for Cartesian checks
#'
#' Stops with a clear error message if `data` is not Cartesian.
#'
#' @param data An aniframe.
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' # Passes silently when the coordinate system matches
#' ensure_is_cartesian(af)
#' @export
ensure_is_cartesian <- function(data) {
  ensure_coordinate_system(data, is_cartesian(data), "Cartesian")
}


#' Test for a 1-D Cartesian coordinate system
#'
#' @param data An aniframe.
#' @param stop Unused, and kept only so the signature does not change.
#'   It has no effect.
#' @return A logical value.
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' is_cartesian_1d(af)
#' @export
is_cartesian_1d <- function(data, stop = FALSE) {
  identical(get_coordinate_system(data), "cartesian_1d")
}


#' Internal guard for 1-D Cartesian checks
#'
#' @param data An aniframe.
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' try(ensure_is_cartesian_1d(af))
#' @export
ensure_is_cartesian_1d <- function(data) {
  ensure_coordinate_system(data, is_cartesian_1d(data), "1D Cartesian")
}


#' Test for a 2-D Cartesian coordinate system
#'
#' @param data An aniframe.
#' @return A logical value.
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' is_cartesian_2d(af)
#' @export
is_cartesian_2d <- function(data) {
  identical(get_coordinate_system(data), "cartesian_2d")
}


#' Internal guard for 2-D Cartesian checks
#'
#' @param data An aniframe.
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' ensure_is_cartesian_2d(af)
#' @export
ensure_is_cartesian_2d <- function(data) {
  ensure_coordinate_system(data, is_cartesian_2d(data), "2D Cartesian")
}


#' Test for a 3-D Cartesian coordinate system
#'
#' @param data An aniframe.
#' @return A logical value.
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' is_cartesian_3d(af)
#' @export
is_cartesian_3d <- function(data) {
  identical(get_coordinate_system(data), "cartesian_3d")
}


#' Internal guard for 3-D Cartesian checks
#'
#' @param data An aniframe.
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' try(ensure_is_cartesian_3d(af))
#' @export
ensure_is_cartesian_3d <- function(data) {
  ensure_coordinate_system(data, is_cartesian_3d(data), "3D Cartesian")
}


#' Test whether an aniframe uses a polar coordinate system
#'
#' @param data An aniframe.
#' @return A logical value.
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' is_polar(af)
#' @export
is_polar <- function(data) {
  identical(get_coordinate_system(data), "polar")
}


#' Internal guard for polar checks
#'
#' @param data An aniframe.
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' # Passes silently when the coordinate system matches
#' try(ensure_is_polar(af))
#' @export
ensure_is_polar <- function(data) {
  ensure_coordinate_system(data, is_polar(data), "polar")
}


#' Test whether an aniframe uses a cylindrical coordinate system
#'
#' @param data An aniframe.
#' @return A logical value.
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' is_cylindrical(af)
#' @export
is_cylindrical <- function(data) {
  identical(get_coordinate_system(data), "cylindrical")
}


#' Internal guard for cylindrical checks
#'
#' @param data An aniframe.
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' # Passes silently when the coordinate system matches
#' try(ensure_is_cylindrical(af))
#' @export
ensure_is_cylindrical <- function(data) {
  ensure_coordinate_system(data, is_cylindrical(data), "cylindrical")
}


#' Test whether an aniframe uses a spherical coordinate system
#'
#' @param data An aniframe.
#' @return A logical value.
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' is_spherical(af)
#' @export
is_spherical <- function(data) {
  identical(get_coordinate_system(data), "spherical")
}


#' Internal guard for spherical checks
#'
#' @param data An aniframe.
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' # Passes silently when the coordinate system matches
#' try(ensure_is_spherical(af))
#' @export
ensure_is_spherical <- function(data) {
  ensure_coordinate_system(data, is_spherical(data), "spherical")
}


#' Abort when the frame is not in the coordinate system a caller needs
#'
#' Reports what the frame *is* in, and points at the two ways out: saying
#' what the columns mean, or converting the coordinates.
#'
#' @param data An aniframe object.
#' @param ok Result of the corresponding `is_*()` predicate.
#' @param wanted Human-readable name of the required coordinate system.
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_coordinate_system <- function(data, ok, wanted) {
  if (ok) {
    return(invisible(TRUE))
  }

  actual <- get_coordinate_system(data)
  hint <- if (identical(actual, "unknown")) {
    "Declare which axis each spatial column carries with {.fn set_axes}."
  } else {
    "Convert the coordinates first; {.pkg anispace} has the transformations."
  }

  cli::cli_abort(c(
    "This aniframe is not in a {wanted} coordinate system.",
    "i" = "{.field coordinate_system} is {.val {actual}}.",
    "i" = hint
  ))
}
