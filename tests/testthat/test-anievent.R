# Tests for the anievent class
#
# Construction:
#   - anievent() builds an object with the expected class chain
#   - as_anievent() coerces a data.frame
#   - as_anievent() on an existing anievent is a no-op
#   - column type standardisation (variable -> character, value -> factor,
#     start/stop -> numeric, individual character -> factor)
#   - metadata defaults: variables_what = "individual",
#     variables_when = c("start", "stop"), variables_where = character()
#   - optional modifiers list-column is preserved
#
# Validation (validate_anievent):
#   - rejects missing required columns
#   - rejects wrong column types
#   - rejects negative intervals (stop < start)
#   - rejects malformed modifiers (non-list cell, unnamed entries)
#
# Predicates:
#   - is_anievent / ensure_is_anievent

# ---- Construction --------------------------------------------------------

test_that("anievent() builds an object with the expected class chain", {
  ae <- anievent(
    individual = c(1L, 1L, 1L),
    variable = c("behaviour", "behaviour", "call"),
    value = c("REM", "wake", "alarm"),
    start = c(3, 14, 4.5),
    stop = c(9, 19, 4.5)
  )

  expect_s3_class(ae, "anievent")
  expect_s3_class(ae, "tbl_df")
  expect_false(inherits(ae, "aniframe"))
})

test_that("as_anievent() coerces a plain data.frame", {
  df <- data.frame(
    individual = 1L,
    variable = "behaviour",
    value = "REM",
    start = 3,
    stop = 9,
    stringsAsFactors = FALSE
  )

  ae <- as_anievent(df)
  expect_s3_class(ae, "anievent")
})

test_that("as_anievent() on an existing anievent is a no-op", {
  ae <- anievent(
    individual = 1L,
    variable = "behaviour",
    value = "REM",
    start = 3,
    stop = 9
  )

  expect_identical(as_anievent(ae), ae)
})

test_that("anievent standardises column types", {
  ae <- anievent(
    individual = c("a", "b"),
    variable = factor(c("behaviour", "call")),
    value = c("REM", "alarm"),
    start = c(3L, 4L),
    stop = c(9L, 4L)
  )

  expect_s3_class(ae$individual, "factor")
  expect_type(ae$variable, "character")
  expect_s3_class(ae$value, "factor")
  expect_type(ae$start, "double")
  expect_type(ae$stop, "double")
})

test_that("anievent metadata gets anievent-flavoured defaults", {
  ae <- anievent(
    individual = 1L,
    variable = "behaviour",
    value = "REM",
    start = 3,
    stop = 9
  )

  md <- get_metadata(ae)
  expect_equal(md$variables_what, "individual")
  expect_equal(md$variables_when, c("start", "stop"))
  expect_length(md$variables_where, 0)
})

test_that("anievent auto-detects recognised identity columns", {
  ae <- anievent(
    subject = c("a", "b"),
    variable = c("behaviour", "behaviour"),
    value = c("REM", "wake"),
    start = c(3, 14),
    stop = c(9, 19)
  )

  expect_equal(get_metadata(ae, "variables_what"), "subject")
})

test_that("anievent accepts an explicit non-default identity column", {
  ae <- anievent(
    rat = c("a", "b"),
    variable = c("behaviour", "behaviour"),
    value = c("REM", "wake"),
    start = c(3, 14),
    stop = c(9, 19),
    variables_what = "rat"
  )

  expect_equal(get_metadata(ae, "variables_what"), "rat")
  expect_s3_class(ae$rat, "factor")
})

test_that("anievent works with no identity column", {
  ae <- anievent(
    variable = c("behaviour", "behaviour"),
    value = c("REM", "wake"),
    start = c(3, 14),
    stop = c(9, 19)
  )

  expect_s3_class(ae, "anievent")
  expect_length(get_metadata(ae, "variables_what"), 0)
})

test_that("optional modifiers list-column is preserved", {
  ae <- anievent(
    individual = c(1L, 1L),
    variable = c("behaviour", "behaviour"),
    value = c("REM", "REM"),
    start = c(3, 14),
    stop = c(9, 19),
    modifiers = list(
      c("limb", "whisker"),
      "tail"
    )
  )

  expect_true("modifiers" %in% names(ae))
  expect_type(ae$modifiers, "list")
  expect_equal(ae$modifiers[[1]], c("limb", "whisker"))
})

# ---- Validation ---------------------------------------------------------

test_that("validate_anievent rejects missing required columns", {
  ae <- anievent(
    individual = 1L,
    variable = "behaviour",
    value = "REM",
    start = 3,
    stop = 9
  )
  ae$value <- NULL

  expect_error(validate_anievent(ae), "Missing required")
})

test_that("validate_anievent rejects wrong column types", {
  ae <- anievent(
    individual = 1L,
    variable = "behaviour",
    value = "REM",
    start = 3,
    stop = 9
  )
  ae$value <- as.character(ae$value)

  expect_error(validate_anievent(ae), "must be a factor")
})

test_that("validate_anievent rejects negative intervals", {
  ae <- anievent(
    individual = 1L,
    variable = "behaviour",
    value = "REM",
    start = 9,
    stop = 3
  )

  expect_error(validate_anievent(ae), "greater than or equal")
})

test_that("validate_anievent rejects malformed modifiers", {
  ae <- anievent(
    individual = c(1L, 1L),
    variable = c("behaviour", "behaviour"),
    value = c("REM", "REM"),
    start = c(3, 14),
    stop = c(9, 19),
    modifiers = list(
      1:3,
      character()
    )
  )

  expect_error(validate_anievent(ae), "character vector")
})

test_that("validate_anievent returns the input invisibly on success", {
  ae <- anievent(
    individual = 1L,
    variable = "behaviour",
    value = "REM",
    start = 3,
    stop = 9
  )

  expect_identical(validate_anievent(ae), ae)
})

# ---- Predicates ---------------------------------------------------------

test_that("is_anievent / ensure_is_anievent work as expected", {
  ae <- anievent(
    individual = 1L,
    variable = "behaviour",
    value = "REM",
    start = 3,
    stop = 9
  )

  expect_true(is_anievent(ae))
  expect_false(is_anievent(data.frame()))
  expect_no_error(ensure_is_anievent(ae))
  expect_error(ensure_is_anievent(data.frame()), "not an anievent")
})
