#' #' Adjust time values to reflect a new sampling rate
#' #'
#' #' This function modifies time values in a dataset to match a new sampling rate and
#' #' updates the corresponding metadata. It handles both integer and non-integer
#' #' time values, ensuring time series start from zero when appropriate.
#' #'
#' #' @param data A data frame or tibble containing the time series data
#' #' @param sampling_rate The new target sampling rate to convert to
#' #' @param old_sampling_rate The original sampling rate of the data (defaults to 1)
#' #'
#' #' @return A modified data frame with adjusted time values and updated metadata
#' #'
#' #' @export
#' #'
#' #' @details The function calculates a scaling factor based on the ratio of old to
#' #' new sampling rates. For integer time values, it ensures they start from zero. All
#' #' time values are then scaled proportionally to maintain relative temporal
#' #' relationships.
#' #'
#' #' @examples
#' #' data <- data.frame(time = 0:10, value = rnorm(11))
#' #' result <- set_sampling_rate(data, sampling_rate = 60, old_sampling_rate = 30)
#' set_sampling_rate <- function(data, sampling_rate, old_sampling_rate = 1) {
#'   ensure_metadata_exists(data)
#'   has_sampling_rate <- !is.null(attributes(data)$metadata$sampling_rate)
#'   if (has_sampling_rate && !is.na(attributes(data)$metadata$sampling_rate)) {
#'     old_sampling_rate <- attributes(data)$metadata$sampling_rate
#'   }
#'
#'   scaling_factor <- old_sampling_rate / sampling_rate
#'
#'   # Ensure frame numbers start at zero
#'   if (is.integer(data$time)) {
#'     data <- data |>
#'       dplyr::mutate(time = .data$time - min(.data$time, na.rm = TRUE))
#'   }
#'   data <- data |>
#'     dplyr::mutate(time = .data$time * scaling_factor)
#'
#'   attributes(data)$metadata$sampling_rate <- sampling_rate
#'
#'   return(data)
#' }
