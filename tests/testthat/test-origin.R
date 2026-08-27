# Test outline for set_origin():
#
# Behaviour:
#   - flips y via reflect_axis(y_height) when origin changes
#     (e.g. bottom_left -> top_left)
#   - is a no-op when supplied origin matches the current one
#   - round-trips: bottom_left -> top_left -> bottom_left recovers original y
#   - leaves y_height unchanged (only the origin field is updated)
#   - preserves the aniframe class
#
# Input validation:
#   - errors when origin is not one of c("bottom_left", "top_left")
#   - errors when y_height is NA and a flip is required
#   - errors when input is not an aniframe

test_that("set_origin flips y when origin changes from bottom_left to top_left", {
  data <- aniframe(
    individual = 1L,
    time = 1:4,
    x = c(0, 1, 2, 3),
    y = c(0, 25, 50, 100)
  )
  data <- set_y_height(data, y_height = 100)

  result <- set_origin(data, origin = "top_left")

  expect_equal(result$y, c(100, 75, 50, 0))
  expect_equal(as.character(get_metadata(result, "origin")), "top_left")
  # y_height is unchanged by set_origin
  expect_equal(get_metadata(result, "y_height"), 100)
})

test_that("set_origin is a no-op when origin matches the current value", {
  data <- aniframe(
    individual = 1L,
    time = 1:3,
    x = c(0, 1, 2),
    y = c(10, 20, 30)
  )
  data <- set_y_height(data, y_height = 100)

  result <- set_origin(data, origin = "bottom_left")

  expect_equal(result$y, data$y)
  expect_equal(as.character(get_metadata(result, "origin")), "bottom_left")
})

test_that("set_origin round-trips back to original y values", {
  data <- aniframe(
    individual = 1L,
    time = 1:4,
    x = c(0, 1, 2, 3),
    y = c(0, 25, 50, 100)
  )
  data <- set_y_height(data, y_height = 100)
  original_y <- data$y

  flipped <- set_origin(data, origin = "top_left")
  back <- set_origin(flipped, origin = "bottom_left")

  expect_equal(back$y, original_y)
  expect_equal(as.character(get_metadata(back, "origin")), "bottom_left")
})

test_that("set_origin errors when y_height is NA", {
  data <- aniframe(
    individual = 1L,
    time = 1:3,
    x = c(0, 1, 2),
    y = c(10, 20, 30)
  )
  # Force y_height to NA, overriding the as_aniframe fallback
  data <- set_metadata(data, y_height = as.numeric(NA))

  expect_error(set_origin(data, origin = "top_left"), "y_height")
})

test_that("set_origin errors on invalid origin value", {
  data <- example_aniframe()
  data <- set_y_height(data, y_height = 100)

  # Non-permitted level: surfaced by set_metadata's factor-level check
  expect_error(set_origin(data, origin = "middle"), "can only be")
  # Wrong shape: caught up-front by set_origin
  expect_error(
    set_origin(data, origin = c("top_left", "bottom_left")),
    "single character string"
  )
})

test_that("set_origin preserves aniframe class", {
  data <- aniframe(
    individual = 1L,
    time = 1:3,
    x = c(0, 1, 2),
    y = c(10, 20, 30)
  )
  data <- set_y_height(data, y_height = 100)

  result <- set_origin(data, origin = "top_left")

  expect_s3_class(result, "aniframe")
})

test_that("set_origin errors when input is not an aniframe", {
  expect_error(set_origin(data.frame(y = 1:3), origin = "top_left"))
})


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
