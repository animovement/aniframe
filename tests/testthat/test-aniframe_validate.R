# Tests for validate_aniframe() and the spatial guards (#79)
#
# The metadata and the frame can drift apart under ordinary dplyr work:
# select() drops a column without touching the metadata naming it, and
# assignment can retype one. Neither is_aniframe() nor the
# is_cartesian*() family notices, since those test for column names only.

make_flat_af <- function() {
  aniframe(
    time = 1:5,
    x = as.numeric(1:5),
    y = as.numeric(1:5),
    variables_what = character(0)
  )
}

# ---- validate_aniframe() -----------------------------------------------

test_that("a well-formed aniframe validates silently and returns invisibly", {
  af <- make_flat_af()

  expect_silent(validate_aniframe(af))
  expect_invisible(validate_aniframe(af))
  expect_identical(validate_aniframe(af), af)
})

test_that("validate_aniframe rejects a non-aniframe", {
  expect_error(
    validate_aniframe(data.frame(time = 1:5, x = 1:5, y = 1:5)),
    "not an aniframe"
  )
})

test_that("dropping a declared spatial column is caught", {
  # The reprex from #79: select() leaves variables_where promising `x`.
  dropped <- dplyr::select(make_flat_af(), -x)

  expect_true(is_aniframe(dropped))
  expect_equal(get_metadata(dropped, "variables_where"), c("x", "y"))
  expect_error(validate_aniframe(dropped), "x")
})

test_that("a declared identity column that is missing is caught", {
  af <- drift_metadata(make_flat_af(), variables_what = "individual")

  expect_error(validate_aniframe(af), "variables_what")
  expect_error(validate_aniframe(af), "individual")
})

test_that("a declared temporal column that is missing is caught", {
  af <- drift_metadata(make_flat_af(), variables_when = c("session", "time"))

  expect_error(validate_aniframe(af), "variables_when")
  expect_error(validate_aniframe(af), "session")
})

test_that("a declared event column that is missing is caught", {
  af <- set_metadata(
    make_flat_af(),
    variables_event = list(state = "behaviour", point = character())
  )

  expect_error(validate_aniframe(af), "variables_event")
  expect_error(validate_aniframe(af), "behaviour")
})

test_that("multiple missing columns are all named", {
  af <- drift_metadata(
    make_flat_af(),
    variables_what = c("individual", "track")
  )

  expect_error(validate_aniframe(af), "individual")
  expect_error(validate_aniframe(af), "track")
})

test_that("a missing time column is caught", {
  # `variables_when` has to be updated alongside the rename, or the
  # declared-columns check fires first and this branch is never reached.
  no_time <- drift_metadata(
    dplyr::rename(make_flat_af(), moment = time),
    variables_when = "moment"
  )

  expect_error(validate_aniframe(no_time), "required but not found")
})

test_that("a non-numeric time column is caught", {
  chr_time <- make_flat_af()
  chr_time$time <- letters[1:5]

  expect_error(validate_aniframe(chr_time), "must be numeric")
})

test_that("coordinate_system drift warns rather than errors", {
  # From #82: setting variables_where alone leaves the derived field stale.
  af <- make_flat_af()
  drifted <- drift_metadata(
    dplyr::mutate(af, z = 0),
    variables_where = c("x", "y", "z")
  )

  expect_equal(
    as.character(get_metadata(drifted, "coordinate_system")),
    "cartesian_2d"
  )
  expect_warning(validate_aniframe(drifted), "coordinate_system")
  expect_warning(validate_aniframe(drifted), "cartesian_3d")
})

# ---- is_spatial() / ensure_is_spatial() --------------------------------

test_that("is_spatial tracks the metadata rather than the column names", {
  af <- make_flat_af()
  expect_true(is_spatial(af))

  # `y` alone still satisfies is_cartesian_1d(), but variables_where
  # promises both columns — that is the divergence is_spatial() catches.
  dropped <- dplyr::select(af, -x)
  expect_true(is_cartesian_1d(dropped))
  expect_false(is_spatial(dropped))

  chr <- af
  chr$x <- letters[1:5]
  expect_true(is_cartesian(chr))
  expect_false(is_spatial(chr))
})

test_that("is_spatial is FALSE when nothing is declared", {
  af <- drift_metadata(make_flat_af(), variables_where = character(0))
  expect_false(is_spatial(af))
})

test_that("ensure_is_spatial names the missing column", {
  dropped <- dplyr::select(make_flat_af(), -x)

  expect_error(ensure_is_spatial(dropped), "Missing spatial column")
  expect_error(ensure_is_spatial(dropped), "x")
})

test_that("ensure_is_spatial names the non-numeric column and its class", {
  chr <- make_flat_af()
  chr$x <- letters[1:5]

  expect_error(ensure_is_spatial(chr), "must be numeric")
  expect_error(ensure_is_spatial(chr), "character")
})

test_that("ensure_is_spatial errors when no spatial variables are declared", {
  af <- drift_metadata(make_flat_af(), variables_where = character(0))
  expect_error(ensure_is_spatial(af), "No spatial variables")
})

test_that("ensure_is_spatial rejects a non-aniframe", {
  expect_error(
    ensure_is_spatial(data.frame(x = 1, y = 1)),
    "not an aniframe"
  )
})

test_that("ensure_is_spatial returns the input invisibly so it can be piped", {
  af <- make_flat_af()

  expect_invisible(ensure_is_spatial(af))
  expect_identical(ensure_is_spatial(af), af)
})

test_that("pluralisation reads correctly for one and several columns", {
  one <- dplyr::select(make_flat_af(), -x)
  expect_error(ensure_is_spatial(one), "column:")

  both <- drift_metadata(
    dplyr::select(make_flat_af(), -x, -y),
    variables_where = c("x", "y")
  )
  expect_error(ensure_is_spatial(both), "columns:")
})
