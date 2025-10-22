#' Convert a data frame to aniframe
#'
#' @param data A data frame with appropriate columns
#' @param metadata A list of metadata to attach to the aniframe
#' @return An aniframe object
#' @export
as_aniframe <- function(data, metadata = list()) {
  # Validate first
  validate_cols(data)

  # Standardize column types
  data <- standardise_aniframe_cols(data)

  # Relocate columns to standard order
  standard_cols <- c(
    "session",
    "trial",
    "individual",
    "keypoint",
    "time"
  )
  coord_system <- matching_position_system(data)
  if (coord_system == "cartesian"){
    standard_cols <- c(standard_cols, "x", "y", "z", "confidence")
  } else if (coord_system == "polar"){
    standard_cols <- c(standard_cols, "rho", "phi", "confidence")
  } else if (coord_system == "cylindrical"){
    standard_cols <- c(standard_cols, "rho", "phi", "z", "confidence")
  } else if (coord_system == "spherical"){
    standard_cols <- c(standard_cols, "rho", "phi", "theta", "confidence")
  }
  present_standard <- standard_cols[standard_cols %in% names(data)]
  other_cols <- setdiff(names(data), present_standard)
  data <- data[, c(present_standard, other_cols)]

  # Group by relevant columns
  potential_groups <- c("session", "trial", "individual", "keypoint")
  groupings <- potential_groups[potential_groups %in% names(data)]

  if (length(groupings) > 0) {
    data <- dplyr::group_by(data, dplyr::across(dplyr::all_of(groupings))) |>
      suppressWarnings()
  }

  # Order by time
  data <- dplyr::arrange(data, .data$time, .by_group = TRUE)

  # Convert to aniframe
  data <- new_aniframe(data)
  data <- set_metadata(data, metadata = metadata)

  # Set coordinate system according to columns present
  data <- set_metadata(data, coordinate_system = factor(matching_position_system(data)))
  data
}

#' Standardize column types for aniframe
#' @keywords internal
standardise_aniframe_cols <- function(data) {
  # Handle x column
  if ("x" %in% names(data)) {
    data$x <- as.numeric(data$x)
  }

  # Handle y column
  if ("y" %in% names(data)) {
    data$y <- as.numeric(data$y)
  }

  # Handle z column
  if ("z" %in% names(data)) {
    data$z <- as.numeric(data$z)
  }

  # Handle rho column
  if ("rho" %in% names(data)) {
    data$rho <- as.numeric(data$rho)
  }

  # Handle phi column
  if ("phi" %in% names(data)) {
    data$phi <- as.numeric(data$phi)
  }

  # Handle theta column
  if ("theta" %in% names(data)) {
    data$theta <- as.numeric(data$theta)
  }

  # Handle keypoint column
  if (!"keypoint" %in% names(data)) {
    data$keypoint <- factor(NA)
  } else {
    data$keypoint <- factor(data$keypoint)
  }

  # Handle individual column
  if (!"individual" %in% names(data)) {
    data$individual <- factor(NA)
  } else {
    data$individual <- factor(data$individual)
  }

  # Handle confidence column
  if (!"confidence" %in% names(data)) {
    data$confidence <- as.numeric(NA)
  } else {
    data$confidence <- as.numeric(data$confidence)
  }

  # Convert trial to integer or factor if present
  if ("trial" %in% names(data)) {
    if (is.numeric(data$trial)) {
      data$trial <- as.integer(data$trial)
    } else if (is.character(data$trial)) {
      data$trial <- factor(data$trial)
    }
  }

  # Convert session to integer or factor if present
  if ("session" %in% names(data)) {
    if (is.numeric(data$session)) {
      data$session <- as.integer(data$session)
    } else if (is.character(data$session)) {
      data$session <- factor(data$session)
    }
  }

  data
}
