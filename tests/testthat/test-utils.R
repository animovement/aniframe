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
