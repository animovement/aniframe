# tests/testthat/test-angle-helpers.R

test_that("deg_to_rad() and rad_to_deg() are inverses of each other", {
  degrees <- c(0, 90, 180, 270, 360, -90)
  radians <- deg_to_rad(degrees)

  expect_equal(rad_to_deg(radians), degrees, tolerance = 1e-10)
  expect_equal(deg_to_rad(rad_to_deg(radians)), radians, tolerance = 1e-10)
})

test_that("deg_to_rad() converts correctly for key values", {
  expect_equal(deg_to_rad(0), 0)
  expect_equal(deg_to_rad(180), pi)
  expect_equal(deg_to_rad(90), pi / 2)
  expect_equal(deg_to_rad(270), 3 * pi / 2)
  expect_equal(deg_to_rad(360), 2 * pi, tolerance = 1e-10)
})

test_that("rad_to_deg() converts correctly for key values", {
  expect_equal(rad_to_deg(0), 0)
  expect_equal(rad_to_deg(pi / 2), 90)
  expect_equal(rad_to_deg(pi), 180)
  expect_equal(rad_to_deg(3 * pi / 2), 270)
  expect_equal(rad_to_deg(2 * pi), 360, tolerance = 1e-10)
})

test_that("constrain_angles_radians() wraps angles to [0, 2pi)", {
  expect_equal(constrain_angles_radians(0), 0)
  expect_equal(constrain_angles_radians(2 * pi), 0)
  expect_equal(constrain_angles_radians(-pi / 2), 3 * pi / 2)
  expect_equal(constrain_angles_radians(3 * pi), pi)
  expect_equal(constrain_angles_radians(4 * pi), 0)
  expect_equal(constrain_angles_radians(5 * pi / 2), pi / 2)
})

test_that("constrain_angles_radians() is vectorised", {
  input <- c(-pi / 2, 0, pi / 2, pi, 3 * pi / 2, 2 * pi)
  expected <- c(3 * pi / 2, 0, pi / 2, pi, 3 * pi / 2, 0)
  expect_equal(constrain_angles_radians(input), expected)
})

test_that("calculate_angular_difference() returns expected signed differences", {
  # Simple same-angle case
  expect_equal(calculate_angular_difference(0, 0), 0)

  # Small positive/negative differences
  expect_equal(calculate_angular_difference(pi / 4, 0), pi / 4)
  expect_equal(calculate_angular_difference(0, pi / 4), -pi / 4)

  # Wrap-around at 2*pi boundary
  expect_equal(calculate_angular_difference(0, 2 * pi - 0.1), 0.1, tolerance = 1e-10)
  expect_equal(calculate_angular_difference(2 * pi - 0.1, 0), -0.1, tolerance = 1e-10)

  # Large differences should wrap correctly
  expect_equal(calculate_angular_difference(0, 3 * pi / 2), pi / 2, tolerance = 1e-10)
  expect_equal(calculate_angular_difference(3 * pi / 2, 0), -pi / 2, tolerance = 1e-10)

  # Difference exactly equal to pi stays as pi, not wrapped
  expect_equal(calculate_angular_difference(pi, 0), pi)
  expect_equal(calculate_angular_difference(0, pi), -pi)
})

test_that("calculate_angular_difference() is vectorised", {
  from <- c(0, pi / 2, pi)
  to   <- c(pi / 2, pi, 0)
  result <- calculate_angular_difference(from, to)
  expected <- c(-pi / 2, -pi / 2, pi)
  expect_equal(result, expected)
})
