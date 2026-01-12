#' Convert a data frame to aniframe
#'
#' @param data A data frame with movement data.
#' @param metadata A list of metadata to attach to the aniframe.
#' @param variables_what Character vector of identity columns that together
#'   define a unique entity. Defaults to `c("individual", "keypoint")`.
#' @param variables_when Character vector of temporal columns that together
#'   define a unique timepoint. Defaults to `"time"`.
#' @param variables_where Character vector of spatial columns that together
#'   define position. If NULL, detected from data.
#'
#' @return An aniframe object
#' @export
as_aniframe <- function(
  data,
  metadata = list(),
  variables_what = NULL,
  variables_when = NULL,
  variables_where = NULL
) {
  defaults <- default_metadata()

  # Resolve variables: use provided or fall back to defaults
  variables_what <- variables_what %||% defaults$variables_what
  variables_when <- variables_when %||% defaults$variables_when

  # For spatial variables: detect from data if not specified
  if (is.null(variables_where)) {
    variables_where <- detect_variables_where(data)
    if (is.null(variables_where)) {
      cli::cli_abort(
        c(
          "No spatial variables found in data.",
          "i" = "Expected columns like {.val x}, {.val y}, {.val z}, {.val rho}, {.val phi}, or {.val theta}.",
          "i" = "Alternatively, specify {.arg variables_where} explicitly."
        )
      )
    }
  }

  # Validate required columns exist
  validate_aniframe_cols(data, variables_when, variables_where)

  # Standardize column types
  data <- standardise_aniframe_cols(
    data,
    variables_what,
    variables_when,
    variables_where
  )

  # Infer coordinate system from spatial variables
  coord_system <- infer_coordinate_system(variables_where)

  # Relocate columns: what, when, where, confidence, rest
  present_what <- variables_what[variables_what %in% names(data)]
  present_when <- variables_when[variables_when %in% names(data)]
  present_where <- variables_where[variables_where %in% names(data)]

  standard_cols <- c(present_what, present_when, present_where)
  if ("confidence" %in% names(data)) {
    standard_cols <- c(standard_cols, "confidence")
  }
  other_cols <- setdiff(names(data), standard_cols)
  data <- data[, c(standard_cols, other_cols)]

  # Order by identity first, then temporal (keeps trajectories contiguous)
  data <- dplyr::arrange(
    data,
    dplyr::across(dplyr::all_of(present_what)),
    dplyr::across(dplyr::all_of(present_when))
  )

  # Group by identity + temporal context (all what + when except time)
  grouping_vars <- c(present_what, setdiff(present_when, "time"))
  grouping_vars <- grouping_vars[grouping_vars %in% names(data)]

  if (length(grouping_vars) > 0) {
    data <- dplyr::group_by(
      data,
      dplyr::across(dplyr::all_of(grouping_vars))
    ) |>
      suppressWarnings()
  }

  # Build aniframe
  data <- new_aniframe(data)
  data <- set_metadata(data, metadata = metadata)
  data <- set_metadata(
    data,
    variables_what = variables_what,
    variables_when = variables_when,
    variables_where = variables_where,
    coordinate_system = factor(coord_system)
  )

  data
}


#' Standardize column types for aniframe
#'
#' Converts character identity and temporal variables to factors.
#' Spatial variables are converted to numeric.
#'
#' @param data Data frame to standardise.
#' @param variables_what Identity variable names.
#' @param variables_when Temporal variable names.
#' @param variables_where Spatial variable names.
#'
#' @return Data frame with standardised column types.
#' @keywords internal
standardise_aniframe_cols <- function(
  data,
  variables_what,
  variables_when,
  variables_where
) {
  # Convert character what/when variables to factors
  # Integers and other types remain unchanged to preserve ordering
  categorical_vars <- c(variables_what, variables_when)
  for (col in categorical_vars) {
    if (col %in% names(data) && is.character(data[[col]])) {
      data[[col]] <- factor(data[[col]])
    }
  }

  # Convert spatial variables to numeric
  for (col in variables_where) {
    if (col %in% names(data)) {
      data[[col]] <- as.numeric(data[[col]])
    }
  }

  data
}


#' Validate required columns for aniframe
#'
#' @param data Data frame to validate.
#' @param variables_when Temporal variables.
#' @param variables_where Spatial variables.
#'
#' @keywords internal
validate_aniframe_cols <- function(data, variables_when, variables_where) {
  # At least one temporal variable must exist
  present_when <- variables_when[variables_when %in% names(data)]
  if (length(present_when) == 0) {
    cli::cli_abort(
      c(
        "No temporal variables found in data.",
        "i" = "Expected at least one of: {.val {variables_when}}."
      )
    )
  }

  # All spatial variables must exist
  missing_where <- setdiff(variables_where, names(data))
  if (length(missing_where) > 0) {
    cli::cli_abort(
      c(
        "Missing spatial variable{?s}: {.val {missing_where}}.",
        "i" = "Position columns must be present in data."
      )
    )
  }

  invisible(TRUE)
}


#' Infer coordinate system from spatial variables
#'
#' @param variables_where Character vector of spatial variable names.
#' @return Character string naming the coordinate system.
#' @keywords internal
infer_coordinate_system <- function(variables_where) {
  vars <- sort(variables_where)

  # Map sorted variable combinations to coordinate systems
  coord_map <- list(
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

  key <- paste(vars, collapse = ",")

  if (key %in% names(coord_map)) {
    return(coord_map[[key]])
  }

  cli::cli_warn(
    c(
      "Could not infer coordinate system from spatial variables: {.val {variables_where}}.",
      "i" = "Setting coordinate system to {.val unknown}."
    )
  )
  "unknown"
}

#' Detect spatial variables from data
#'
#' @param data Data frame to check.
#' @return Character vector of detected spatial variable names, or NULL if none found.
#' @keywords internal
detect_variables_where <- function(data) {
  cartesian <- c("x", "y", "z")
  polar_spherical <- c("rho", "phi", "theta")

  present_cartesian <- cartesian[cartesian %in% names(data)]
  present_polar <- polar_spherical[polar_spherical %in% names(data)]

  # Prefer cartesian if any present
  if (length(present_cartesian) > 0) {
    return(present_cartesian)
  }

  if (length(present_polar) > 0) {
    return(present_polar)
  }

  NULL
}
