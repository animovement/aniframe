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
#'   present, minus the index. These are the temporal *context* — which
#'   session, which trial — and, together with `variables_what`, they are
#'   what the frame is grouped by. The index itself is declared separately
#'   and is never one of them.
#' @param index Length-one character vector naming the column the frame is
#'   indexed by — the position of each row within its temporal context.
#'   It is never a grouping variable. If
#'   `NULL` (the default), the frame's existing declaration is kept, or
#'   `"time"` for a frame that has none. The column must exist and be
#'   numeric; it may be called anything.
#' @param variables_where The spatial columns that together define
#'   position. Either a plain character vector of column names, in which
#'   case the name is taken to be the axis role, or a vector named by axis
#'   role — `c(x = "u", y = "v")` — which lets the columns be called
#'   anything. The roles themselves are a closed set (`x`, `y`, `z`,
#'   `rho`, `phi`, `theta`), so that transformations between coordinate
#'   systems stay well defined; an unrecognised role is rejected by name.
#'   If `NULL` (the default), detected from the data.
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
  variables_where = NULL,
  index = NULL
) {
  defaults <- list_default_metadata()

  # An explicit index wins; otherwise keep what the frame already declares,
  # falling back to "time" for a frame — or a serialised object — with no
  # declaration at all.
  if (!is.null(index)) {
    ensure_index_name(index)
  }
  index <- index %||%
    (if (is_aniframe(data) || is_anievent(data)) {
      resolve_index(get_metadata(data))
    } else {
      NULL
    }) %||%
    "time"

  # A frame that already declares a role keeps it. Casting an aniframe
  # that has been given a custom identity -- `id`, say -- used to re-run
  # detection, find no recognised name, inject `keypoint = "centroid"`
  # and overwrite the declaration with it (#96). Declarations whose
  # columns have since been dropped fall through to detection, so a cast
  # still repairs a frame rather than erroring on it.
  variables_when <- variables_when %||%
    get_declared_if_present(data, "variables_when")
  variables_what <- variables_what %||%
    get_declared_if_present(data, "variables_what")
  variables_where <- variables_where %||%
    get_declared_if_present(data, "variables_where")

  # Resolve variables_when: detect from data if not specified
  if (is.null(variables_when)) {
    # Recognised temporal variable names (time is always required)
    recognised_when <- c("observation", "session", "trial", "time")

    # Only include recognised when variables that are present in data
    variables_when <- recognised_when[recognised_when %in% names(data)]
  }

  # `variables_when` is the temporal *context* — which session, which
  # trial. The index is the position within that context and is declared
  # separately, so detection's `time` drops out here.
  variables_when <- setdiff(variables_when, index)

  # Resolve variables_what: detect from data if not specified
  if (is.null(variables_what)) {
    data <- add_default_identity(data)

    # Only include recognised what variables that are present in data
    variables_what <- list_recognised_variables_what()[
      list_recognised_variables_what() %in% names(data)
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

  # `index` is a declaration, so `set_metadata()` refuses it — it goes on
  # directly, before the restructure that reads it back.
  md <- get_metadata(data)
  md[["variables_index"]] <- index
  data <- attach_metadata(data, md)

  data <- restructure_aniframe(
    data,
    variables_what,
    variables_when,
    variables_where
  )

  # Fall back y_height to max of the y axis when not supplied and that
  # axis is present. Never overwrite a value that's already set — only
  # `set_y_height()` / `set_origin()` should mutate it post-construction.
  # Found by role, so a frame whose y axis is called something else is
  # handled too.
  axes <- get_axes(data)
  if ("y" %in% names(axes)) {
    current_y_height <- get_metadata(data, "y_height")
    if (length(current_y_height) == 0 || is.na(current_y_height)) {
      max_y <- suppressWarnings(max(data[[axes[["y"]]]], na.rm = TRUE))
      if (is.finite(max_y)) {
        data <- set_metadata(data, y_height = max_y)
      }
    }
  }

  data
}


#' Add a default identity variable when the data has none
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
#' @param data Data frame to complete.
#'
#' @return `data`, with an identity column added if it had none.
#' @keywords internal
add_default_identity <- function(data) {
  has_identity <- any(list_recognised_variables_what() %in% names(data))

  if (!has_identity) {
    data$keypoint <- "centroid"
  }

  data
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
get_declared_if_present <- function(data, field) {
  if (!has_metadata(data)) {
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
