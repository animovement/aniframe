library(testthat)

# ------------------------------------------------------------------
# Tests for check_class()
# ------------------------------------------------------------------

test_that("check_class returns TRUE when class matches", {
  x <- data.frame(a = 1:3)
  expect_true(check_class(x, "data.frame"))
})

test_that("check_class returns TRUE when class matches one of multiple classes", {
  x <- structure(list(), class = c("my_class", "list"))
  expect_true(check_class(x, "my_class"))
  expect_true(check_class(x, "list"))
})

test_that("check_class returns FALSE when class does not match", {
  x <- data.frame(a = 1:3)
  expect_false(check_class(x, "matrix"))
})

test_that("check_class works with numeric objects", {
  x <- 42
  expect_true(check_class(x, "numeric"))
  expect_false(check_class(x, "character"))
})

test_that("check_class works with character objects", {
  x <- "hello"
  expect_true(check_class(x, "character"))
  expect_false(check_class(x, "numeric"))
})

test_that("check_class works with list objects", {
  x <- list(a = 1, b = 2)
  expect_true(check_class(x, "list"))
  expect_false(check_class(x, "data.frame"))
})

test_that("check_class works with custom S3 classes", {
  x <- structure(list(), class = "my_custom_class")
  expect_true(check_class(x, "my_custom_class"))
  expect_false(check_class(x, "another_class"))
})

# ------------------------------------------------------------------
# Tests for ensure_class()
# ------------------------------------------------------------------

test_that("ensure_class does not error when class matches", {
  x <- data.frame(a = 1:3)
  expect_no_error(ensure_class(x, "data.frame"))
})

test_that("ensure_class does not error when class is one of multiple classes", {
  x <- structure(list(), class = c("my_class", "list"))
  expect_no_error(ensure_class(x, "my_class"))
  expect_no_error(ensure_class(x, "list"))
})

test_that("ensure_class errors when class does not match", {
  x <- data.frame(a = 1:3)
  expect_error(
    ensure_class(x, "matrix")
  )
})

test_that("ensure_class error message includes expected and actual class", {
  x <- data.frame(a = 1:3)
  expect_error(
    ensure_class(x, "matrix"),
    "Expected an object of class matrix"
  )
  expect_error(
    ensure_class(x, "matrix"),
    "data.frame"
  )
})

test_that("ensure_class works with numeric objects", {
  x <- 42
  expect_no_error(ensure_class(x, "numeric"))
  expect_error(ensure_class(x, "character"))
})

test_that("ensure_class works with character objects", {
  x <- "hello"
  expect_no_error(ensure_class(x, "character"))
  expect_error(ensure_class(x, "numeric"))
})

test_that("ensure_class works with custom S3 classes", {
  x <- structure(list(), class = "my_custom_class")
  expect_no_error(ensure_class(x, "my_custom_class"))
  expect_error(ensure_class(x, "another_class"))
})

test_that("ensure_class returns invisibly when successful", {
  x <- data.frame(a = 1:3)
  result <- ensure_class(x, "data.frame")
  expect_null(result)
})

