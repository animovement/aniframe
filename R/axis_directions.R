#' The directions an axis can point
#'
#' Six words in three opposed pairs, read from where the recording was made:
#' `right`/`left` across the view, `up`/`down` within it, and `back`/`forward`
#' toward and away from the viewer. What they correspond to in the world is
#' `reference_frame`'s to say.
#'
#' @return Character vector of the permitted directions.
#' @keywords internal
list_axis_directions <- function() {
  c("right", "left", "up", "down", "back", "forward")
}


#' The axis roles that are angles
#'
#' Turning a linear axis over changes what these mean, so a frame carrying
#' one cannot be flipped without recomputing them.
#'
#' @return Character vector of axis roles.
#' @keywords internal
list_angular_axis_roles <- function() {
  c("phi", "theta")
}


#' The axis roles that can point somewhere
#'
#' The Cartesian axes. `rho` is a distance and `phi` and `theta` are angles,
#' so none of them has a direction of its own.
#'
#' @return Character vector of axis roles.
#' @keywords internal
list_linear_axis_roles <- function() {
  c("x", "y", "z")
}


#' Which opposed pair each direction belongs to
#'
#' Two axes pointing along the same pair are parallel, which no frame can be.
#'
#' @return Named character vector, direction to pair.
#' @keywords internal
list_direction_pairs <- function() {
  c(
    right = "horizontal",
    left = "horizontal",
    up = "vertical",
    down = "vertical",
    back = "depth",
    forward = "depth"
  )
}


#' The direction opposite each direction
#'
#' @return Named character vector, direction to its opposite.
#' @keywords internal
list_direction_opposites <- function() {
  c(
    right = "left",
    left = "right",
    up = "down",
    down = "up",
    back = "forward",
    forward = "back"
  )
}


#' Each direction as a unit vector
#'
#' In a right-handed basis with `right`, `up` and `back` as the positive
#' axes — `back` being toward the viewer. [get_angle_direction()] and
#' [get_handedness()] are read off these.
#'
#' @return Named list of length-3 numeric vectors.
#' @keywords internal
list_direction_vectors <- function() {
  list(
    right = c(1, 0, 0),
    left = c(-1, 0, 0),
    up = c(0, 1, 0),
    down = c(0, -1, 0),
    back = c(0, 0, 1),
    forward = c(0, 0, -1)
  )
}


#' Get the direction each axis points
#'
#' @param data An aniframe or anievent object.
#'
#' @return Named character vector, axis role to direction. Empty when the
#'   frame declares none.
#'
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' af <- set_axis_directions(af, c(x = "right", y = "up"))
#' get_axis_directions(af)
#'
#' @seealso [set_axis_directions()], [get_axis_extents()]
#' @export
get_axis_directions <- function(data) {
  ensure_is_aniframe_or_anievent(data)
  resolve_axis_directions(get_metadata(data))
}


#' Read the axis directions out of metadata
#'
#' @param md A metadata list.
#'
#' @return Named character vector, empty when nothing is declared.
#' @keywords internal
resolve_axis_directions <- function(md) {
  declared <- md[["axis_directions"]]
  if (is.null(declared) || length(declared) == 0L) {
    return(stats::setNames(character(), character()))
  }
  declared <- declared[!is.na(declared)]
  stats::setNames(as.character(declared), names(declared))
}


#' Say which way an axis points
#'
#' @description
#' Records the direction of one or more axes, keyed by axis role. Roles not
#' named keep the direction they had, so flipping one axis leaves the rest
#' alone.
#'
#' Turning an axis to its opposite reflects that column around the axis
#' extent, so the data ends up expressed in the direction being declared.
#' Any other change is a re-description and leaves the values untouched.
#'
#' @param data An aniframe object.
#' @param directions Named character vector, axis role to direction — one of
#'   `right`, `left`, `up`, `down`, `back` or `forward`. `NA` clears an axis.
#'
#' @return The aniframe, with reflected coordinates for any axis turned to
#'   its opposite and the new directions recorded.
#'
#' @details
#' Directions are read from where the recording was made: `right`/`left`
#' across the view, `up`/`down` within it, `back`/`forward` toward and away
#' from the viewer. No two axes may point along the same pair.
#'
#' An axis runs from zero to its extent, so turning it over gives
#' `new = extent - old`. An axis with no declared extent is centred on its
#' origin instead, and turning it over negates it. Declare one with
#' [set_axis_extents()] for data that is measured from a corner, such as
#' video.
#'
#' @examples
#' af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
#' af <- set_axis_extents(af, c(y = 1080))
#' af <- set_axis_directions(af, c(x = "right", y = "down"))
#'
#' # Turning y over reflects it
#' af <- set_axis_directions(af, c(y = "up"))
#' get_axis_directions(af)
#'
#' @seealso [get_axis_directions()], [set_axis_extents()],
#'   [get_angle_direction()]
#' @export
set_axis_directions <- function(data, directions) {
  ensure_is_aniframe(data)
  ensure_valid_axis_directions(directions)

  current <- get_axis_directions(data)
  wanted <- merge_axis_map(current, directions)
  ensure_unopposed_axis_directions(wanted)

  flipped <- names(wanted)[vapply(
    names(wanted),
    function(role) {
      identical(
        unname(current[role]),
        unname(list_direction_opposites()[wanted[[role]]])
      )
    },
    logical(1)
  )]

  for (role in flipped) {
    data <- reflect_axis_role(data, role)
  }

  data <- set_metadata(data, axis_directions = wanted)

  # Three directions determine the handedness, so the recorded one is not
  # left saying otherwise.
  settled <- derive_handedness(wanted)
  if (!identical(settled, "unknown")) {
    data <- set_metadata(data, handedness = settled)
  }
  data
}


#' Reflect the column carrying an axis role around its extent
#'
#' @param data An aniframe object.
#' @param role An axis role.
#'
#' @return `data`, with that column reflected.
#' @keywords internal
reflect_axis_role <- function(data, role) {
  axes <- get_axes(data)
  if (!role %in% names(axes)) {
    # An angle is measured from the axes, so turning one over leaves every
    # stored angle facing the wrong way. Refusing is the honest answer:
    # relabelling the metadata would not make the values true.
    angular <- intersect(list_angular_axis_roles(), names(axes))
    if (length(angular) > 0L) {
      cli::cli_abort(c(
        "Cannot turn the {.field {role}} axis over on a {.val {get_coordinate_system(data)}} frame.",
        "i" = "{.val {angular}} {?is/are} measured from the axes, so every angle would have to be recomputed.",
        "i" = "{.pkg anispace} has the transformations."
      ))
    }
    # No column carries this role, so there is nothing to express
    # differently -- the direction is a fact about the space, not the data.
    return(data)
  }

  # An axis runs from zero to its extent, so its mirror is `extent - v`.
  # An axis with no declared extent is centred on its origin instead, and
  # turning it over negates it -- which is what a world-coordinate axis
  # wants, and what an image axis would get wrong.
  extents <- get_axis_extents(data)
  reference <- if (role %in% names(extents)) extents[[role]] else 0

  column <- axes[[role]]
  ensure_has_column(data, column)
  reflect_axis(data, axis = column, reference = reference)
}


#' Combine a partial axis map into the one already declared
#'
#' @param current,update Named vectors of the same type.
#'
#' @return `current` with `update` written over it, `NA` entries dropped.
#' @keywords internal
merge_axis_map <- function(current, update) {
  merged <- current
  for (role in names(update)) {
    merged[[role]] <- update[[role]]
  }
  merged <- merged[!is.na(merged)]
  merged[order(match(names(merged), list_linear_axis_roles()))]
}


#' Is this a usable map of axis roles to directions?
#'
#' @param directions Value supplied to [set_axis_directions()].
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_valid_axis_directions <- function(directions) {
  ensure_named_axis_map(directions, "directions", 'c(x = "right", y = "up")')
  # An all-`NA` vector arrives logical, and clearing an axis is a fair thing
  # to ask for.
  if (!is.character(directions) && !all(is.na(directions))) {
    cli::cli_abort(c(
      "{.arg directions} must be a character vector.",
      "i" = "One of {.val {list_axis_directions()}} for each axis."
    ))
  }

  given <- directions[!is.na(directions)]
  unknown <- setdiff(given, list_axis_directions())
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "{.val {unknown}} {?is/are} not {?a/} direction{?s}.",
      "i" = "An axis points {.val {list_axis_directions()}}."
    ))
  }

  ensure_unopposed_axis_directions(given)
}


#' Do these axes point along different pairs?
#'
#' @param directions Named character vector of directions.
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_unopposed_axis_directions <- function(directions) {
  pairs <- list_direction_pairs()[directions]
  clashing <- unique(pairs[duplicated(pairs)])
  if (length(clashing) > 0L) {
    offending <- names(directions)[pairs %in% clashing]
    cli::cli_abort(c(
      "Axes {.val {offending}} point along the same line.",
      "i" = "{.val {directions[offending]}} {?is/are} all {clashing}.",
      "i" = "Two axes of one frame cannot be parallel."
    ))
  }
  invisible(TRUE)
}


#' Is this a named map keyed by axis role?
#'
#' @param x Value to test.
#' @param arg Name of the argument it came from.
#' @param example A well-formed value, shown when `x` is not one.
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_named_axis_map <- function(x, arg, example, call = rlang::caller_env()) {
  nms <- names(x)
  if (length(x) == 0L || is.null(nms) || any(nms == "" | is.na(nms))) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must name an axis role for every value.",
        "i" = "For example {.code {example}}."
      ),
      call = call
    )
  }

  unknown <- setdiff(nms, list_linear_axis_roles())
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "{.val {unknown}} {?is/are} not {?an/} axis{?/es} that points anywhere.",
        "i" = "Only {.val {list_linear_axis_roles()}} have a direction and an extent."
      ),
      call = call
    )
  }
  invisible(TRUE)
}


#' Reflect a spatial axis around a reference value
#'
#' `reference - data[[axis]]`, which is what turning an axis over amounts to.
#'
#' @param data A data frame (typically an aniframe) containing `axis`.
#' @param axis Name of the column to reflect.
#' @param reference A single finite value to reflect around.
#'
#' @return The data with `axis` replaced by `reference - data[[axis]]`.
#' @keywords internal
reflect_axis <- function(data, axis, reference) {
  if (!is.character(axis) || length(axis) != 1) {
    cli::cli_abort("{.arg axis} must be a single column name.")
  }
  ensure_has_column(data, axis)
  if (
    !is.numeric(reference) ||
      length(reference) != 1 ||
      !is.finite(reference)
  ) {
    cli::cli_abort(
      "{.arg reference} must be a single finite numeric value."
    )
  }
  data[[axis]] <- reference - data[[axis]]
  data
}
