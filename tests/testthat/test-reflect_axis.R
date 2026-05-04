# Test outline for reflect_axis():
#
# Behaviour:
#   - flips a column around the given reference (h - y)
#   - leaves other columns untouched
#   - is its own inverse (applying twice returns original values)
#
# Input validation:
#   - errors when the named axis column is missing from data
#   - errors when reference is NA, Inf, non-numeric, or has length != 1
#   - errors when axis is not a single character string

test_that("reflect_axis flips a column around the given reference", {
  data <- dplyr::tibble(x = 1:5, y = c(0, 25, 50, 75, 100))

  result <- reflect_axis(data, axis = "y", reference = 100)

  expect_equal(result$y, c(100, 75, 50, 25, 0))
  expect_equal(result$x, 1:5) # untouched
})

test_that("reflect_axis is its own inverse", {
  data <- dplyr::tibble(x = 1:5, y = c(0, 25, 50, 75, 100))
  ref <- 100

  twice <- reflect_axis(reflect_axis(data, "y", ref), "y", ref)

  expect_equal(twice$y, data$y)
})

test_that("reflect_axis errors on missing column", {
  data <- dplyr::tibble(x = 1:3)
  expect_error(reflect_axis(data, axis = "y", reference = 10), "not found")
})

test_that("reflect_axis errors on invalid reference", {
  data <- dplyr::tibble(y = 1:3)
  expect_error(reflect_axis(data, "y", reference = NA_real_), "finite")
  expect_error(reflect_axis(data, "y", reference = Inf), "finite")
  expect_error(reflect_axis(data, "y", reference = c(1, 2)), "finite")
  expect_error(reflect_axis(data, "y", reference = "ten"), "finite")
})

test_that("reflect_axis errors on invalid axis argument", {
  data <- dplyr::tibble(y = 1:3)
  expect_error(reflect_axis(data, axis = c("x", "y"), 10), "single column name")
  expect_error(reflect_axis(data, axis = 1L, 10), "single column name")
})
