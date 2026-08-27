# Axis roles (#109) ----

test_that("an unnamed declaration reads the column name as the role", {
  # The historical form. Every existing frame and every reader's output
  # arrives this way, so it has to keep working untouched.
  af <- aniframe(individual = "a", time = 1:3, x = c(1, 2, 3), y = c(0, 1, 0))

  expect_equal(get_axes(af), c(x = "x", y = "y"))
  expect_equal(
    as.character(get_metadata(af, "coordinate_system")),
    "cartesian_2d"
  )
})

test_that("get_variables_where() still returns bare column names", {
  # The accessor strips names, so callers that want columns are unaffected
  # by roles being stored.
  af <- aniframe(individual = "a", time = 1:3, x = c(1, 2, 3), y = c(0, 1, 0))

  expect_equal(get_variables_where(af), c("x", "y"))
  expect_null(names(get_variables_where(af)))
})

test_that("axes can be carried by columns named anything", {
  df <- data.frame(time = 1:3, individual = "a", u = c(1, 2, 3), v = c(0, 1, 0))

  af <- as_aniframe(df, variables_where = c(x = "u", y = "v"))

  expect_equal(get_axes(af), c(x = "u", y = "v"))
  expect_equal(get_variables_where(af), c("u", "v"))
  # Previously this degraded to "unknown" and every spatial function
  # refused the frame.
  expect_equal(
    as.character(get_metadata(af, "coordinate_system")),
    "cartesian_2d"
  )
})

test_that("a renamed polar frame is recognised as polar", {
  df <- data.frame(
    time = 1:3,
    individual = "a",
    r = c(100, 200, 300),
    ang = c(0, 1, 2)
  )

  af <- as_aniframe(df, variables_where = c(rho = "r", phi = "ang"))

  expect_equal(as.character(get_metadata(af, "coordinate_system")), "polar")
  expect_equal(get_axes(af)[["rho"]], "r")
})

test_that("y_height is taken from the y axis, whatever the column is called", {
  df <- data.frame(time = 1:3, individual = "a", u = c(1, 2, 3), v = c(0, 5, 0))

  af <- as_aniframe(df, variables_where = c(x = "u", y = "v"))

  expect_equal(get_metadata(af, "y_height"), 5)
})

test_that("an unrecognised role is rejected by name", {
  df <- data.frame(time = 1:3, individual = "a", u = c(1, 2, 3), v = c(0, 1, 0))

  # Named by the offending role at the point of declaration, rather than
  # silently degrading to "unknown" and failing later somewhere else.
  expect_error(
    as_aniframe(df, variables_where = c(banana = "u", y = "v")),
    "not recognised"
  )
})

test_that("roles that do not form a coordinate system are rejected", {
  df <- data.frame(time = 1:3, individual = "a", u = c(1, 2, 3), v = c(0, 1, 0))

  # x and theta belong to different systems; the set is closed so that
  # transformations between systems stay well defined.
  expect_error(
    as_aniframe(df, variables_where = c(x = "u", theta = "v")),
    "do not form a coordinate system"
  )
})

test_that("a duplicated role is rejected", {
  df <- data.frame(time = 1:3, individual = "a", u = c(1, 2, 3), v = c(0, 1, 0))

  expect_error(
    as_aniframe(df, variables_where = c(x = "u", x = "v")),
    "declared more than once"
  )
})

test_that("an unnamed declaration that matches nothing still warns rather than aborting", {
  # The lenient path is preserved for bare column names, because readers
  # and existing frames depend on it.
  df <- data.frame(time = 1:3, individual = "a", u = c(1, 2, 3), v = c(0, 1, 0))

  expect_warning(
    af <- as_aniframe(df, variables_where = c("u", "v")),
    "Could not infer coordinate system"
  )
  expect_equal(as.character(get_metadata(af, "coordinate_system")), "unknown")
  # Roles are only stored once they mean something, so `unknown` keeps the
  # bare vector it was declared with.
  expect_null(names(get_metadata(af, "variables_where")))
  expect_equal(get_axes(af), stats::setNames(character(), character()))
})

test_that("set_variables_where() accepts a role mapping", {
  df <- data.frame(time = 1:3, individual = "a", u = c(1, 2, 3), v = c(0, 1, 0))
  af <- suppressWarnings(as_aniframe(df, variables_where = c("u", "v")))

  result <- set_variables_where(af, c(x = "u", y = "v"))

  expect_equal(get_axes(result), c(x = "u", y = "v"))
  expect_equal(
    as.character(get_metadata(result, "coordinate_system")),
    "cartesian_2d"
  )
})

test_that("axis_role_sets() and infer_coordinate_system() agree", {
  # The inference reads its map from the same place the validator does, so
  # a role set can never be accepted and then fail to infer.
  for (key in names(axis_role_sets())) {
    roles <- strsplit(key, ",", fixed = TRUE)[[1]]
    axes <- stats::setNames(roles, roles)
    expect_equal(infer_coordinate_system(axes), axis_role_sets()[[key]])
  }
})


# Length-unit conversion resolves roles to columns ----

test_that("set_unit_space() converts the length axes of a renamed frame", {
  af <- as_aniframe(
    data.frame(time = 1:3, individual = "a", u = c(1, 2, 3), v = c(0, 1, 0)),
    variables_where = c(x = "u", y = "v")
  )

  result <- expect_no_warning(
    set_unit_space(af, "mm", calibration_factor = 10)
  )

  expect_equal(result$u, c(10, 20, 30))
  expect_equal(result$v, c(0, 10, 0))
  expect_equal(as.character(get_metadata(result, "unit_space")), "mm")
})

test_that("set_unit_space() converts rho but not phi on a renamed polar frame", {
  af <- as_aniframe(
    data.frame(time = 1:3, individual = "a", r = c(1, 2, 3), a = c(0, 1, 2)),
    variables_where = c(rho = "r", phi = "a")
  )

  result <- expect_no_warning(
    set_unit_space(af, "mm", calibration_factor = 10)
  )

  expect_equal(result$r, c(10, 20, 30))
  expect_equal(result$a, c(0, 1, 2))
})
