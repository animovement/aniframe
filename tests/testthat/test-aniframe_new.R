# Tests for ani_df constructor and creation functions

library(testthat)

# ---- Test ani_df() constructor ----

test_that("ani_df creates object with required columns", {
  df <- dplyr::tibble(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10)
  )

  df <- new_aniframe(df)

  expect_s3_class(df, "aniframe")
  expect_s3_class(df, "tbl_df")
})
