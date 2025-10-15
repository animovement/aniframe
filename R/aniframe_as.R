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
    "time",
    "x",
    "y",
    "z",
    "confidence"
  )
  present_standard <- standard_cols[standard_cols %in% names(data)]
  other_cols <- setdiff(names(data), present_standard)
  data <- data[, c(present_standard, other_cols)]

  # Group by relevant columns
  potential_groups <- c("session", "trial", "individual", "keypoint")
  groupings <- potential_groups[potential_groups %in% names(data)]

  if (length(groupings) > 0) {
    data <- dplyr::group_by(data, dplyr::across(dplyr::all_of(groupings)))
  }

  # Order by time
  data <- dplyr::arrange(data, .data$time, .by_group = TRUE)

  # Convert to aniframe
  data <- new_aniframe(data)
  data <- set_metadata(data, metadata = metadata)
  data
}

#' Standardize column types for aniframe
#' @keywords internal
standardise_aniframe_cols <- function(data) {
  # Handle x column
  if (!"x" %in% names(data)) {
    data$x <- as.numeric(NA)
  } else {
    data$x <- as.numeric(data$x)
  }

  # Handle y column
  if (!"y" %in% names(data)) {
    data$y <- as.numeric(NA)
  } else {
    data$y <- as.numeric(data$y)
  }

  # Handle z column
  if (!"z" %in% names(data)) {
    data$z <- as.numeric(NA)
  } else {
    data$z <- as.numeric(data$z)
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
