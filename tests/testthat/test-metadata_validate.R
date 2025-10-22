library(testthat)

# ------------------------------------------------------------------
# Tests for ensure_metadata_exists()
# ------------------------------------------------------------------

test_that("Tests for ensure_metadata_exists()", {
  x <- example_aniframe()
  expect_no_error(ensure_metadata_exists(x))
})

test_that("Tests for ensure_metadata_exists()", {
  x <- data.frame()
  expect_error(ensure_metadata_exists(x))
})

# ------------------------------------------------------------------
# Tests for ensure_is_list()
# ------------------------------------------------------------------

test_that("Tests for ensure_is_list()", {
  x <- example_aniframe()
  expect_no_error(ensure_is_list(get_metadata(x)))
})

test_that("Tests for ensure_is_list()", {
  x <- data.frame()
  expect_error(ensure_is_list(x))
})

# ------------------------------------------------------------------
# Tests for ensure_all_metadata_fields_present()
# ------------------------------------------------------------------

test_that("Tests for ensure_all_metadata_fields_present()", {
  x <- example_aniframe()
  expect_no_error(ensure_all_metadata_fields_present(get_metadata(x)))
})

test_that("Tests for ensure_all_metadata_fields_present()", {
  x <- example_aniframe()
  md <- get_metadata(x)
  expect_error(ensure_all_metadata_fields_present(md[-1]))
})
