#' Convert a data frame to aniframe
#'
#' @param data A data frame with movement data.
#' @param metadata A list of metadata to attach to the aniframe.
#' @param variables_what Character vector of identity columns that together
#'   define a unique entity, and which the frame is grouped by. If `NULL`
#'   (the default), detected from the data: whichever of `model`,
#'   `individual`, `subject`, `track` and `keypoint` are present, in that
#'   order (coarse to fine). An aniframe needs
#'   at least one identity variable, so if none of them is found, a
#'   `keypoint` column is added with the value `"centroid"`. Pass
#'   `character(0)` to declare no identity variables at all — a
#'   deliberate opt-out, which leaves the frame ungrouped. Every column
#'   named here must exist in `data`.
#' @param variables_when Character vector of temporal columns that together
#'   define a unique timepoint. If `NULL` (the default), detected from the
#'   data: whichever of `observation`, `session`, `trial` and `time` are
#'   present. `time` is always required.
#' @param variables_where Character vector of spatial columns that together
#'   define position. If `NULL` (the default), detected from the data.
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

  # Resolve variables_when: detect from data if not specified
  if (is.null(variables_when)) {
    # Recognised temporal variable names (time is always required)
    recognised_when <- c("observation", "session", "trial", "time")

    # Only include recognised when variables that are present in data
    variables_when <- recognised_when[recognised_when %in% names(data)]
  }

  # Resolve variables_what: detect from data if not specified
  if (is.null(variables_what)) {
    data <- ensure_identity(data)

    # Only include recognised what variables that are present in data
    variables_what <- recognised_variables_what()[
      recognised_variables_what() %in% names(data)
    ]
  }

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
  ensure_aniframe_cols(data, variables_what, variables_when, variables_where)

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

  # Fall back y_height to max(y) when not supplied and y is present.
  # Never overwrite a value that's already set — only `set_y_height()` /
  # `set_origin()` should mutate it post-construction.
  if ("y" %in% present_where) {
    current_y_height <- get_metadata(data, "y_height")
    if (length(current_y_height) == 0 || is.na(current_y_height)) {
      max_y <- suppressWarnings(max(data$y, na.rm = TRUE))
      if (is.finite(max_y)) {
        data <- set_metadata(data, y_height = max_y)
      }
    }
  }

  data
}


#' Standardize column types for aniframe
#'
#' Converts character identity and temporal variables to factors.
#' Converts numeric identity and temporal variables (except time) to integers.
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
  # What and when variables (except time) should be categorical or integer
  categorical_vars <- c(variables_what, setdiff(variables_when, "time"))
  for (col in categorical_vars) {
    if (col %in% names(data)) {
      if (is.character(data[[col]])) {
        data[[col]] <- factor(data[[col]])
      } else if (is.numeric(data[[col]])) {
        data[[col]] <- as.integer(data[[col]])
      }
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


#' Ensure the data carries at least one identity variable
#'
#' An aniframe needs **at least one identity (`what`) variable** — the
#' columns that together say which entity a row belongs to, and which the
#' frame is grouped by. When auto-detection finds none of the recognised
#' names in the data, one is added so that rule holds.
#'
#' The column added is `keypoint = "centroid"`. It is not a claim about
#' the data: it does not mean the frame holds pose or skeleton data, only
#' that it has a single unnamed entity. A more neutral default
#' (`individual = "all"`) was considered and rejected in #77 — the name
#' stays as it is.
#'
#' This applies only to the auto-detection path. An explicit
#' `variables_what = character(0)` is a deliberate declaration of "no
#' identity variables" and is left alone.
#'
#' @param data Data frame to check.
#'
#' @return `data`, with an identity column added if it had none.
#' @keywords internal
ensure_identity <- function(data) {
  has_identity <- any(recognised_variables_what() %in% names(data))

  if (!has_identity) {
    data$keypoint <- "centroid"
  }

  data
}


#' Validate required columns for aniframe
#'
#' @param data Data frame to validate.
#' @param variables_what Identity variables.
#' @param variables_when Temporal variables.
#' @param variables_where Spatial variables.
#'
#' @keywords internal
ensure_aniframe_cols <- function(
  data,
  variables_what,
  variables_when,
  variables_where
) {
  # All identity variables must exist. Declaring a column that isn't
  # there leaves the metadata describing a frame it doesn't have.
  missing_what <- setdiff(variables_what, names(data))
  if (length(missing_what) > 0) {
    cli::cli_abort(
      c(
        "Identity variable{?s} not found in data: {.val {missing_what}}.",
        "i" = "Columns specified in {.arg variables_what} must be present."
      )
    )
  }

  # time column is always required
  if (!"time" %in% names(data)) {
    cli::cli_abort(
      c(
        "Column {.val time} is required but not found in data.",
        "i" = "The {.val time} column must always be present."
      )
    )
  }

  # Check other temporal variables if specified
  other_when <- setdiff(variables_when, "time")
  missing_when <- setdiff(other_when, names(data))
  if (length(missing_when) > 0) {
    cli::cli_abort(
      c(
        "Temporal variable{?s} not found in data: {.val {missing_when}}.",
        "i" = "Columns specified in {.arg variables_when} must be present."
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
#' Polar-family detection runs first so that cylindrical data (`rho`, `phi`,
#' `z`) and spherical data (`rho`, `phi`, `theta`) are not mis-classified as
#' Cartesian on account of their `z` column. The `rho` + `phi` pair is the
#' signature of a polar-family system; `z` then distinguishes cylindrical
#' from polar, and `theta` distinguishes spherical.
#'
#' @param data Data frame to check.
#' @return Character vector of detected spatial variable names, or NULL if none found.
#' @keywords internal
detect_variables_where <- function(data) {
  has_rho <- "rho" %in% names(data)
  has_phi <- "phi" %in% names(data)
  has_theta <- "theta" %in% names(data)
  has_z <- "z" %in% names(data)

  if (has_rho && has_phi) {
    if (has_theta) {
      return(c("rho", "phi", "theta")) # spherical
    } else if (has_z) {
      return(c("rho", "phi", "z")) # cylindrical
    } else {
      return(c("rho", "phi")) # polar
    }
  }

  cartesian <- c("x", "y", "z")
  present_cartesian <- cartesian[cartesian %in% names(data)]
  if (length(present_cartesian) > 0) {
    return(present_cartesian)
  }

  NULL
}
