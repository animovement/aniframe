#' Rotate coordinates in Cartesian space (2D or 3D)
#'
#' Automatically detects whether data are 2D or 3D and applies
#' the corresponding rotation method.
#'
#' @param data movement data frame with columns: time, individual, keypoint, x, y, z (optional)
#' @param alignment_points character vector of length 2 specifying the keypoints used for alignment
#' @param align_perpendicular logical; if TRUE, alignment_points are rotated to be
#'        perpendicular to the 0-degree axis (y-axis). If FALSE (default), they are
#'        aligned with the x-axis.
#'
#' @return movement data frame with rotated coordinates
#' @export
rotate_coords <- function(
    data,
    alignment_points,
    align_perpendicular = FALSE
) {
  ensure_is_aniframe(data)
  ensure_is_cartesian(data)

  if (length(alignment_points) != 2) {
    cli::cli_abort("alignment_points must contain exactly 2 keypoint names")
  }
  if (!all(alignment_points %in% unique(data$keypoint))) {
    cli::cli_abort("Some specified keypoints not found in data")
  }

  has_z <- "z" %in% names(data) && !all(is.na(data$z))

  if (has_z) {
    rotate_coords_3d(data, alignment_points, align_perpendicular)
  } else {
    rotate_coords_2d(data, alignment_points, align_perpendicular)
  }
}

#' @keywords internal
rotate_coords_2d <- function(data, alignment_points, align_perpendicular = FALSE) {
  individuals <- unique(data$individual)
  out_data <- data.frame()

  for (ind in individuals) {
    ind_data <- dplyr::filter(data, .data$individual == ind)

    p1 <- ind_data |>
      dplyr::filter(.data$keypoint == alignment_points[1]) |>
      dplyr::select(dplyr::all_of(c("time", "x", "y"))) |>
      dplyr::rename(x1 = "x", y1 = "y") |>
      suppressMessages()

    p2 <- ind_data |>
      dplyr::filter(.data$keypoint == alignment_points[2]) |>
      dplyr::select(dplyr::all_of(c("time", "x", "y"))) |>
      dplyr::rename(x2 = "x", y2 = "y") |>
      suppressMessages()

    angles <- dplyr::left_join(p1, p2, by = "time") |>
      dplyr::mutate(
        vec_x = .data$x2 - .data$x1,
        vec_y = .data$y2 - .data$y1,
        current_angle = atan2(.data$vec_y, .data$vec_x),
        target_angle = if (align_perpendicular) pi / 2 else 0,
        rotation_angle = .data$target_angle - .data$current_angle
      ) |>
      dplyr::select(dplyr::all_of(c("time", "rotation_angle")))

    ind_rotated <- ind_data |>
      dplyr::left_join(angles, by = "time") |>
      dplyr::mutate(
        x_new = .data$x * cos(.data$rotation_angle) - .data$y * sin(.data$rotation_angle),
        y_new = .data$x * sin(.data$rotation_angle) + .data$y * cos(.data$rotation_angle)
      ) |>
      dplyr::select(!dplyr::all_of(c("rotation_angle", "x", "y"))) |>
      dplyr::rename(x = "x_new", y = "y_new") |>
      suppressMessages()

    out_data <- dplyr::bind_rows(out_data, ind_rotated)
  }

  as_aniframe(out_data)
}

#' @keywords internal
rotate_coords_3d <- function(data, alignment_points, align_perpendicular = FALSE) {
  cli::cli_abort("3D rotation is not supported yet")

  # as_aniframe(out_data)
}
