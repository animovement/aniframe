# Tests for aniframe validator

library(testthat)
library(tibble)

# ---- Test validate_cols() ----

test_that("validate_cols throws no error when both time and position are present", {
  df <- dplyr::tibble(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10)
  )

  df <- new_aniframe(df)

  expect_no_error(validate_cols(df))
})

test_that("validate_cols throws an error when time is not present", {
  df <- dplyr::tibble(
    x = rnorm(10),
    y = rnorm(10)
  )

  df <- new_aniframe(df)

  expect_error(validate_cols(df))
})

test_that("validate_cols throws an error when position is not present", {
  df <- dplyr::tibble(
    time = 1:10
  )

  df <- new_aniframe(df)

  expect_error(validate_cols(df))
})
