# tests/testthat/test-coordinate-helpers.R

test_that("is_cartesian() correctly identifies Cartesian coordinate systems", {
  expect_true(is_cartesian(data.frame(x = 1:3, y = 2:4)))
  expect_true(is_cartesian(data.frame(z = 1)))
  expect_false(is_cartesian(data.frame(a = 1, b = 2)))
})

test_that("ensure_is_cartesian() aborts when data is not Cartesian", {
  expect_silent(ensure_is_cartesian(data.frame(x = 1)))
  expect_error(
    ensure_is_cartesian(data.frame(a = 1, b = 2)),
    "This data frame is not in a Cartesian coordinate system"
  )
})

test_that("is_polar() correctly identifies polar coordinate systems", {
  expect_true(is_polar(data.frame(rho = 1:3, phi = 2:4)))
  expect_false(is_polar(data.frame(rho = 1, theta = 2)))
  expect_false(is_polar(data.frame(x = 1, y = 2)))
})

test_that("ensure_is_polar() aborts when data is not polar", {
  expect_silent(ensure_is_polar(data.frame(rho = 1, phi = 2)))
  expect_error(
    ensure_is_polar(data.frame(x = 1, y = 2)),
    "This data frame is not in a polar coordinate system"
  )
})

test_that("is_cylindrical() correctly identifies cylindrical coordinate systems", {
  expect_true(is_cylindrical(data.frame(rho = 1, phi = 2, z = 3)))
  expect_false(is_cylindrical(data.frame(rho = 1, phi = 2)))
  expect_false(is_cylindrical(data.frame(x = 1, y = 2, z = 3)))
})

test_that("ensure_is_cylindrical() aborts when data is not cylindrical", {
  expect_silent(ensure_is_cylindrical(data.frame(rho = 1, phi = 2, z = 3)))
  expect_error(
    ensure_is_cylindrical(data.frame(rho = 1, phi = 2)),
    "This data frame is not in a cylindrical coordinate system"
  )
})

test_that("is_spherical() correctly identifies spherical coordinate systems", {
  expect_true(is_spherical(data.frame(rho = 1, phi = 2, theta = 3)))
  expect_false(is_spherical(data.frame(rho = 1, phi = 2)))
  expect_false(is_spherical(data.frame(x = 1, y = 2, z = 3)))
})

test_that("ensure_is_spherical() aborts when data is not spherical", {
  expect_silent(ensure_is_spherical(data.frame(rho = 1, phi = 2, theta = 3)))
  expect_error(
    ensure_is_spherical(data.frame(rho = 1, phi = 2)),
    "This data frame is not in a spherical coordinate system"
  )
})

# ------------------------------------------------------------------
# 1D / 2D / 3D Cartesian
# ------------------------------------------------------------------

test_that("is_cartesian_1d() correctly identifies 1D Cartesian data", {
  expect_true(is_cartesian_1d(data.frame(x = 1:3)))
  expect_true(is_cartesian_1d(data.frame(z = 1:3)))
  # Forbidden polar columns reject
  expect_false(is_cartesian_1d(data.frame(x = 1, rho = 2)))
  # More than one cartesian axis -> not 1D
  expect_false(is_cartesian_1d(data.frame(x = 1, y = 2)))
})

test_that("ensure_is_cartesian_1d() aborts when data isn't 1D Cartesian", {
  expect_silent(ensure_is_cartesian_1d(data.frame(x = 1)))
  expect_error(
    ensure_is_cartesian_1d(data.frame(x = 1, y = 2)),
    "1D Cartesian"
  )
})

test_that("is_cartesian_2d() correctly identifies 2D Cartesian data", {
  expect_true(is_cartesian_2d(data.frame(x = 1, y = 2)))
  # z present but all NA is allowed
  expect_true(is_cartesian_2d(data.frame(x = 1, y = 2, z = NA)))
  # z present with non-NA values rejects
  expect_false(is_cartesian_2d(data.frame(x = 1, y = 2, z = 3)))
  expect_false(is_cartesian_2d(data.frame(x = 1)))
})

test_that("ensure_is_cartesian_2d() aborts when data isn't 2D Cartesian", {
  expect_silent(ensure_is_cartesian_2d(data.frame(x = 1, y = 2)))
  expect_error(
    ensure_is_cartesian_2d(data.frame(x = 1, y = 2, z = 3)),
    "2D Cartesian"
  )
})

test_that("is_cartesian_3d() correctly identifies 3D Cartesian data", {
  expect_true(is_cartesian_3d(data.frame(x = 1, y = 2, z = 3)))
  # Missing one of x/y/z rejects
  expect_false(is_cartesian_3d(data.frame(x = 1, y = 2)))
  # All three present but z all NA rejects
  expect_false(is_cartesian_3d(data.frame(x = 1, y = 2, z = NA)))
})

test_that("ensure_is_cartesian_3d() aborts when data isn't 3D Cartesian", {
  expect_silent(ensure_is_cartesian_3d(data.frame(x = 1, y = 2, z = 3)))
  expect_error(
    ensure_is_cartesian_3d(data.frame(x = 1, y = 2)),
    "3D Cartesian"
  )
})
