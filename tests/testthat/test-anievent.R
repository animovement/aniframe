# Tests for the anievent class
#
# Construction:
#   - anievent() builds an object with the expected class chain
#   - as_anievent() coerces a data.frame
#   - as_anievent() on an existing anievent is a no-op
#   - column type standardisation (channel -> character, value -> factor,
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
    channel = c("behaviour", "behaviour", "call"),
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
    channel = "behaviour",
    value = "REM",
    start = 3,
    stop = 9,
    stringsAsFactors = FALSE
  )

  ae <- as_anievent(df)
  expect_s3_class(ae, "anievent")
})

test_that("anievent() accepts a single data.frame as its only argument", {
  df <- dplyr::tibble(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 3,
    stop = 9
  )

  ae <- anievent(df)
  expect_s3_class(ae, "anievent")
})

test_that("as_anievent() on an existing anievent is a no-op", {
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 3,
    stop = 9
  )

  expect_identical(as_anievent(ae), ae)
})

test_that("anievent standardises column types", {
  ae <- anievent(
    individual = c("a", "b"),
    channel = factor(c("behaviour", "call")),
    value = c("REM", "alarm"),
    start = c(3L, 4L),
    stop = c(9L, 4L)
  )

  expect_s3_class(ae$individual, "factor")
  expect_type(ae$channel, "character")
  expect_s3_class(ae$value, "factor")
  expect_type(ae$start, "double")
  expect_type(ae$stop, "double")
})

test_that("anievent metadata gets anievent-flavoured defaults", {
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
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
    channel = c("behaviour", "behaviour"),
    value = c("REM", "wake"),
    start = c(3, 14),
    stop = c(9, 19)
  )

  expect_equal(get_metadata(ae, "variables_what"), "subject")
})

test_that("anievent accepts an explicit non-default identity column", {
  ae <- anievent(
    rat = c("a", "b"),
    channel = c("behaviour", "behaviour"),
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
    channel = c("behaviour", "behaviour"),
    value = c("REM", "wake"),
    start = c(3, 14),
    stop = c(9, 19)
  )

  expect_s3_class(ae, "anievent")
  expect_length(get_metadata(ae, "variables_what"), 0)
})

test_that("anievent auto-detects observation / session / trial into variables_when", {
  ae <- anievent(
    individual = c(1L, 1L, 1L, 1L),
    observation = c("clip_a", "clip_a", "clip_b", "clip_b"),
    trial = c(1L, 1L, 2L, 2L),
    channel = c("behaviour", "behaviour", "behaviour", "behaviour"),
    value = c("REM", "wake", "REM", "wake"),
    start = c(3, 14, 1, 7),
    stop = c(9, 19, 5, 12)
  )

  expect_equal(
    get_metadata(ae, "variables_when"),
    c("observation", "trial", "start", "stop")
  )
})

test_that("anievent coerces auto-detected grouping columns to factor / integer", {
  ae <- anievent(
    individual = 1L,
    observation = c("clip_a", "clip_a"),
    trial = c(1L, 2L),
    channel = c("behaviour", "behaviour"),
    value = c("REM", "wake"),
    start = c(3, 14),
    stop = c(9, 19)
  )

  expect_s3_class(ae$observation, "factor")
  expect_type(ae$trial, "integer")
})

test_that("anievent column ordering mirrors aniframe (what, when incl start/stop, payload)", {
  ae <- anievent(
    individual = 1L,
    observation = "clip_a",
    trial = 1L,
    modifiers = list(character()),
    channel = "behaviour",
    value = "REM",
    start = 3,
    stop = 9
  )

  expect_equal(
    names(ae),
    c(
      "individual",
      "observation",
      "trial",
      "start",
      "stop",
      "channel",
      "value",
      "modifiers"
    )
  )
})

test_that("optional modifiers list-column is preserved", {
  ae <- anievent(
    individual = c(1L, 1L),
    channel = c("behaviour", "behaviour"),
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
    channel = "behaviour",
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
    channel = "behaviour",
    value = "REM",
    start = 3,
    stop = 9
  )
  bad_channel <- ae
  bad_channel$channel <- factor(bad_channel$channel)
  expect_error(validate_anievent(bad_channel), "must be character")

  bad_value <- ae
  bad_value$value <- as.character(bad_value$value)
  expect_error(validate_anievent(bad_value), "must be a factor")

  bad_start <- ae
  bad_start$start <- as.character(bad_start$start)
  expect_error(validate_anievent(bad_start), "start must be numeric")

  bad_stop <- ae
  bad_stop$stop <- as.character(bad_stop$stop)
  expect_error(validate_anievent(bad_stop), "stop must be numeric")
})

test_that("validate_anievent rejects overlapping bouts in the same channel for the same subject", {
  ae <- anievent(
    individual = c(1L, 1L),
    channel = c("behaviour", "behaviour"),
    value = c("REM", "wake"),
    start = c(3, 5),
    stop = c(8, 10)
  )

  expect_error(validate_anievent(ae), "overlap")
})

test_that("validate_anievent accepts overlapping bouts on different channels", {
  ae <- anievent(
    individual = c(1L, 1L),
    channel = c("behaviour", "call"),
    value = c("REM", "alarm"),
    start = c(3, 5),
    stop = c(8, 5)
  )

  expect_no_error(validate_anievent(ae))
})

test_that("validate_anievent accepts overlapping bouts in the same channel across subjects", {
  ae <- anievent(
    individual = c(1L, 2L),
    channel = c("behaviour", "behaviour"),
    value = c("REM", "REM"),
    start = c(3, 4),
    stop = c(8, 9)
  )

  expect_no_error(validate_anievent(ae))
})

test_that("validate_anievent rejects negative intervals", {
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 9,
    stop = 3
  )

  expect_error(validate_anievent(ae), "greater than or equal")
})

test_that("validate_anievent rejects malformed modifiers", {
  ae <- anievent(
    individual = c(1L, 1L),
    channel = c("behaviour", "behaviour"),
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

test_that("validate_anievent rejects a modifiers column that isn't a list", {
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 3,
    stop = 9
  )
  ae$modifiers <- "not a list"

  expect_error(validate_anievent(ae), "must be a list-column")
})

test_that("validate_anievent accepts well-formed modifiers", {
  ae <- anievent(
    individual = c(1L, 1L, 1L),
    channel = c("behaviour", "behaviour", "call"),
    value = c("REM", "REM", "alarm"),
    start = c(3, 14, 4.5),
    stop = c(9, 19, 4.5),
    modifiers = list(
      c("limb", "whisker"),
      "tail",
      character()
    )
  )

  expect_no_error(validate_anievent(ae))
})

test_that("validate_anievent returns the input invisibly on success", {
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
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
    channel = "behaviour",
    value = "REM",
    start = 3,
    stop = 9
  )

  expect_true(is_anievent(ae))
  expect_false(is_anievent(data.frame()))
  expect_no_error(ensure_is_anievent(ae))
  expect_error(ensure_is_anievent(data.frame()), "not an anievent")
})
