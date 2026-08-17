#' Convert NaN to NA in numeric columns
#'
#' Replaces all `NaN` values with `NA` in numeric columns of a data frame.
#'
#' @param data A data frame.
#' @return A data frame with `NaN` values replaced by `NA` in numeric columns.
#' @export
convert_nan_to_na <- function(data) {
  dplyr::mutate(
    data,
    dplyr::across(dplyr::where(is.numeric), function(x) {
      ifelse(is.nan(x), NA, x)
    })
  )
}

#' Convert radians to degrees
#'
#' @param x Numeric vector of angles (radians).
#' @return Numeric vector of angles expressed in degrees.
#' @export
rad_to_deg <- function(x) {
  (x * 180) / pi
}

#' Convert degrees to radians
#'
#' @param x Numeric vector of angles (degrees).
#' @return Numeric vector of angles expressed in radians.
#' @export
deg_to_rad <- function(x) {
  (x * pi) / 180
}

#' Identity variable names recognised across the animovement classes
#'
#' The identity (`what`) columns auto-detection looks for, shared by
#' [as_aniframe()] and [as_anievent()]. The order is coarse to fine — a
#' `subject` or `individual` has `track`s, a track has `keypoint`s — and
#' it carries through to column order and grouping. Only the names
#' present in the data are used, and any other column can be declared
#' explicitly via `variables_what`.
#'
#' `subject` and `individual` name the same kind of thing; both are
#' recognised because behavioural coding tools (BORIS and its kin) speak
#' of subjects where tracking tools speak of individuals.
#'
#' @return Character vector of column names.
#' @keywords internal
recognised_variables_what <- function() {
  c("model", "individual", "subject", "track", "keypoint")
}

#' Classes owned by dplyr, tibble and base R
#'
#' The tail of the class vector that belongs to dplyr rather than to
#' animovement. `NextMethod()` returns these already set correctly, so
#' they are never restored from the input — doing so would, for instance,
#' re-group the result of an [dplyr::ungroup()].
#'
#' @return Character vector of class names.
#' @keywords internal
base_frame_classes <- function() {
  c("grouped_df", "rowwise_df", "tbl_df", "tbl", "data.frame")
}

#' Re-clothe a dispatched result with its animovement classes and metadata
#'
#' After a generic strips a result down to a plain tibble (via
#' `NextMethod()`), restore the animovement classes the input carried and
#' re-attach its metadata.
#'
#' dplyr rebuilds only the classes it knows how to reconstruct, so by the
#' time `NextMethod()` returns, the whole animovement family is gone —
#' `aniframe` / `anievent` and any subclass a downstream package has built
#' on top of them. Restoring the *incoming* stack rather than asserting a
#' fixed one is what lets such a subclass (e.g. `animetric`'s
#' `aniframe_kin`) survive a pipeline without registering methods of its
#' own.
#'
#' Order is preserved, so a subclass stays ahead of its parent and keeps
#' dispatch priority over it.
#'
#' @param x The bare result returned by `NextMethod()`.
#' @param cls Class vector of the original input, captured before dispatch.
#' @param md Metadata captured before dispatch via [get_metadata()].
#'
#' @return `x` with the animovement classes and metadata restored.
#' @keywords internal
preserve_animovement_class <- function(x, cls, md) {
  # Lay the incoming animovement classes down in their original order,
  # then whatever dplyr set on the result. Re-adding only the *missing*
  # ones would append them at the front instead, putting `aniframe` ahead
  # of its own subclasses in the methods that strip it before dispatch.
  animovement_cls <- setdiff(cls, base_frame_classes())
  class(x) <- c(animovement_cls, setdiff(class(x), animovement_cls))
  set_metadata(x, metadata = md)
}
