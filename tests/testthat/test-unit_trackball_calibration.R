test_that("get_trackball_calibration_factor calculates correct calibration factor", {
  # Create simple test data - ball moved 100 units in x direction
  test_data <- data.frame(
    time = seq(1, 5),
    x = c(0, 25, 50, 75, 100),
    y = c(0, 0, 0, 0, 0)
  ) |>
    as_aniframe()

  # For a 10mm diameter ball rotated 1 time:
  # Real distance = pi * 10 * 1 = ~31.42mm
  # Measured distance = 100 units
  # Calibration factor = 31.42 / 100 = ~0.3142
  result <- get_trackball_calibration_factor(
    test_data,
    ball_diameter = 10,
    ball_rotations = 1
  )

  expect_equal(result, pi * 10 / 100, tolerance = 1e-10)
})

test_that("get_trackball_calibration_factor uses the axis with maximum travel", {
  # Y axis has more travel than X
  test_data <- data.frame(
    time = seq(1, 5),
    x = c(0, 10, 20, 30, 40),
    y = c(0, 25, 50, 75, 100)
  ) |>
    as_aniframe()

  result <- get_trackball_calibration_factor(
    test_data,
    ball_diameter = 20,
    ball_rotations = 2
  )

  # Should use y-axis travel (100 units)
  expected <- (pi * 20 * 2) / 100
  expect_equal(result, expected)
})

test_that("get_trackball_calibration_factor handles negative movement", {
  # Ball moved backwards in x direction
  test_data <- data.frame(
    time = seq(1, 5),
    x = c(100, 75, 50, 25, 0),
    y = c(0, 0, 0, 0, 0)
  ) |>
    as_aniframe()

  result <- get_trackball_calibration_factor(
    test_data,
    ball_diameter = 10,
    ball_rotations = 1
  )

  # Should handle negative travel correctly (using abs)
  expect_equal(result, pi * 10 / 100, tolerance = 1e-10)
})

test_that("get_trackball_calibration_factor handles multiple rotations", {
  test_data <- data.frame(
    time = seq(1, 5),
    x = c(0, 50, 100, 150, 200),
    y = c(0, 0, 0, 0, 0)
  ) |>
    as_aniframe()

  # 5 complete rotations
  result <- get_trackball_calibration_factor(
    test_data,
    ball_diameter = 50,
    ball_rotations = 5
  )

  expected <- (pi * 50 * 5) / 200
  expect_equal(result, expected)
})

test_that("get_trackball_calibration_factor handles fractional rotations", {
  test_data <- data.frame(
    time = seq(1, 5),
    x = c(0, 25, 50, 75, 100),
    y = c(0, 0, 0, 0, 0)
  ) |>
    as_aniframe()

  result <- get_trackball_calibration_factor(
    test_data,
    ball_diameter = 10,
    ball_rotations = 1.5
  )

  expected <- (pi * 10 * 1.5) / 100
  expect_equal(result, expected)
})

test_that("get_trackball_calibration_factor uses only first and last points", {
  # Intermediate points should not affect calculation
  test_data <- data.frame(
    time = seq(1, 5),
    x = c(0, 200, -50, 300, 100), # Zigzag path
    y = c(0, 0, 0, 0, 0)
  ) |>
    as_aniframe()

  result <- get_trackball_calibration_factor(
    test_data,
    ball_diameter = 10,
    ball_rotations = 1
  )

  # Should only use first (0) and last (100)
  expected <- (pi * 10) / 100
  expect_equal(result, expected)
})

test_that("get_trackball_calibration_factor handles equal x and y travel", {
  # Both axes travel the same distance
  test_data <- data.frame(
    time = seq(1, 5),
    x = c(0, 25, 50, 75, 100),
    y = c(0, 25, 50, 75, 100)
  ) |>
    as_aniframe()

  result <- get_trackball_calibration_factor(
    test_data,
    ball_diameter = 10,
    ball_rotations = 1
  )

  # which.max will return the first index (x) when equal
  expected <- (pi * 10) / 100
  expect_equal(result, expected)
})

test_that("get_trackball_calibration_factor errors on non-aniframe data", {
  test_data <- data.frame(
    time = seq(1, 3),
    x = c(0, 50, 100),
    y = c(0, 0, 0)
  )

  # Should error because ensure_is_aniframe will fail
  expect_error(get_trackball_calibration_factor(
    test_data,
    ball_diameter = 10,
    ball_rotations = 1
  ))
})

test_that("get_trackball_calibration_factor handles single row data", {
  test_data <- data.frame(
    time = 1,
    x = 100,
    y = 50
  ) |>
    as_aniframe()

  # First and last are the same, so travel = 0
  # This will cause division by zero - consider if this needs handling
  expect_equal(
    get_trackball_calibration_factor(
      test_data,
      ball_diameter = 10,
      ball_rotations = 1
    ),
    Inf
  )
})
