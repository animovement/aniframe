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
