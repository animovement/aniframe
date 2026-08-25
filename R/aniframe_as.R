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
#' @examples
#' df <- data.frame(
#'   time = 1:3, individual = 'a', keypoint = 'centroid',
#'   x = c(0, 1, 2), y = c(0, 1, 0)
#' )
#' as_aniframe(df)
#' @export
as_aniframe <- function(
  data,
  metadata = list(),
  variables_what = NULL,
  variables_when = NULL,
  variables_where = NULL
) {
  defaults <- default_metadata()

  # A frame that already declares a role keeps it. Casting an aniframe
  # that has been given a custom identity -- `id`, say -- used to re-run
  # detection, find no recognised name, inject `keypoint = "centroid"`
  # and overwrite the declaration with it (#96). Declarations whose
  # columns have since been dropped fall through to detection, so a cast
  # still repairs a frame rather than erroring on it.
  variables_when <- variables_when %||%
    declared_if_present(data, "variables_when")
  variables_what <- variables_what %||%
    declared_if_present(data, "variables_what")
  variables_where <- variables_where %||%
    declared_if_present(data, "variables_where")

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

  # Attach class and metadata first, then let the shared restructure do
  # the rest: validate, standardise types, relocate, arrange, regroup,
  # and derive `coordinate_system`. Construction and re-declaration go
  # through the same code so they cannot drift apart (#82).
  data <- new_aniframe(data)
  data <- set_metadata(data, metadata = metadata)
  data <- restructure_aniframe(
    data,
    variables_what,
    variables_when,
    variables_where
  )

  # Fall back y_height to max(y) when not supplied and y is present.
  # Never overwrite a value that's already set — only `set_y_height()` /
  # `set_origin()` should mutate it post-construction.
  if ("y" %in% variables_where) {
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
  # All declared variables must exist. Declaring a column that isn't
  # there leaves the metadata describing a frame it doesn't have.
  ensure_declared_cols_exist(data, variables_what, "what")

  # time column is always required
  if (!"time" %in% names(data)) {
    cli::cli_abort(
      c(
        "Column {.val time} is required but not found in data.",
        "i" = "The {.val time} column must always be present."
      )
    )
  }

  ensure_declared_cols_exist(data, setdiff(variables_when, "time"), "when")
  ensure_declared_cols_exist(data, variables_where, "where")

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


#' A role the data already declares, when its columns are still there
#'
#' Casting an object that is already an aniframe should not re-derive
#' what it has been told. It does fall back to detection when the
#' declared columns are gone, so a cast still repairs a frame whose
#' metadata has drifted rather than erroring on it.
#'
#' @param data Data frame, possibly carrying metadata.
#' @param field One of the `variables_*` metadata fields.
#'
#' @return The declared column names, or `NULL` to detect instead.
#' @keywords internal
declared_if_present <- function(data, field) {
  if (!check_metadata_exists(data)) {
    return(NULL)
  }

  declared <- as.character(get_metadata(data, field))
  declared <- declared[!is.na(declared)]

  # An empty declaration is a deliberate opt-out (`variables_what =
  # character(0)`), so it is kept as it is rather than re-detected.
  if (length(declared) == 0) {
    return(if (identical(field, "variables_what")) character(0) else NULL)
  }

  if (all(declared %in% names(data))) declared else NULL
}
