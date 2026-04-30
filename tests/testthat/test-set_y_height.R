# Test outline for set_y_height():
#
# Behaviour:
#   - stores the supplied value in the y_height metadata field
#   - preserves the aniframe class
#
# Input validation:
#   - rejects non-positive, non-finite, non-numeric, or non-scalar values
#   - errors when input is not an aniframe
#
# Consistency with data:
#   - warns when y_height is below max(y) (would produce negative y after reflect)
#   - does not warn when y_height is at or above max(y)

test_that("set_y_height stores the value in metadata", {
  data <- example_aniframe()

  result <- set_y_height(data, y_height = 1080)

  expect_equal(get_metadata(result, "y_height"), 1080)
})

test_that("set_y_height preserves aniframe class", {
  data <- example_aniframe()
  result <- set_y_height(data, y_height = 1080)
  expect_s3_class(result, "aniframe")
})

test_that("set_y_height rejects invalid values", {
  data <- example_aniframe()

  expect_error(set_y_height(data, y_height = -1), "positive")
  expect_error(set_y_height(data, y_height = 0), "positive")
  expect_error(set_y_height(data, y_height = NA_real_), "finite")
  expect_error(set_y_height(data, y_height = Inf), "finite")
  expect_error(set_y_height(data, y_height = c(100, 200)), "single")
  expect_error(set_y_height(data, y_height = "tall"), "numeric")
})

test_that("set_y_height warns when y_height is below max(y)", {
  data <- aniframe(
    individual = 1L,
    time = 1:3,
    x = c(1, 2, 3),
    y = c(10, 50, 100)
  )

  expect_warning(set_y_height(data, y_height = 50), "less than")
})

test_that("set_y_height does not warn when y_height is at or above max(y)", {
  data <- aniframe(
    individual = 1L,
    time = 1:3,
    x = c(1, 2, 3),
    y = c(10, 50, 100)
  )

  expect_no_warning(set_y_height(data, y_height = 100))
  expect_no_warning(set_y_height(data, y_height = 1080))
})

test_that("set_y_height errors when input is not an aniframe", {
  expect_error(set_y_height(data.frame(y = 1:3), y_height = 100))
})
