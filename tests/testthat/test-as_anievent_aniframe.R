# Tests for as_anievent.aniframe (RLE conversion from aniframe -> anievent)
#
# Construction:
#   - state column run-length-encoded into bouts (start = first frame time,
#     stop = last frame time)
#   - point column emits one row per non-NA frame with start == stop
#   - both state and point columns coexist in one call
#   - NA values break runs (gap is not part of any bout)
#   - per-(individual, observation) grouping isolates bouts
#
# Metadata propagation:
#   - unit_time inherited from the host aniframe
#   - sampling_rate inherited
#   - variables_when picks up grouping cols + start/stop
#
# Errors:
#   - host with no variables_event declared
#   - declared column missing from host data

make_state_aniframe <- function() {
  af <- aniframe(
    individual = rep(1L, 8),
    time = 1:8,
    x = rnorm(8),
    y = rnorm(8),
    behaviour = factor(
      c("REM", "REM", "REM", "wake", "wake", "REM", "REM", NA),
      levels = c("REM", "wake")
    )
  )
  set_metadata(
    af,
    variables_event = list(state = "behaviour", point = character())
  )
}

test_that("state column is run-length-encoded into bouts", {
  af <- make_state_aniframe()
  ae <- as_anievent(af)

  expect_s3_class(ae, "anievent")
  expect_equal(nrow(ae), 3) # REM(1-3), wake(4-5), REM(6-7); the trailing NA closes
  expect_equal(ae$start, c(1, 4, 6))
  expect_equal(ae$stop, c(3, 5, 7))
  expect_equal(as.character(ae$value), c("REM", "wake", "REM"))
  expect_true(all(ae$channel == "behaviour"))
})

test_that("point column emits one row per non-NA frame with start == stop", {
  af <- aniframe(
    individual = rep(1L, 5),
    time = 1:5,
    x = rnorm(5),
    y = rnorm(5),
    call = factor(c(NA, "alarm", NA, "alarm", NA), levels = "alarm")
  )
  af <- set_metadata(
    af,
    variables_event = list(state = character(), point = "call")
  )

  ae <- as_anievent(af)
  expect_equal(nrow(ae), 2)
  expect_equal(ae$start, c(2, 4))
  expect_equal(ae$start, ae$stop)
  expect_true(all(ae$channel == "call"))
})

test_that("state and point columns coexist in one conversion", {
  af <- aniframe(
    individual = rep(1L, 5),
    time = 1:5,
    x = rnorm(5),
    y = rnorm(5),
    behaviour = factor(c("REM", "REM", "wake", "wake", "wake")),
    call = factor(c(NA, "alarm", NA, NA, NA), levels = "alarm")
  )
  af <- set_metadata(
    af,
    variables_event = list(state = "behaviour", point = "call")
  )

  ae <- as_anievent(af)
  expect_setequal(unique(ae$channel), c("behaviour", "call"))
  expect_equal(sum(ae$channel == "behaviour"), 2) # REM(1-2), wake(3-5)
  expect_equal(sum(ae$channel == "call"), 1) # one alarm at t=2
})

test_that("per-individual grouping isolates bouts", {
  af <- aniframe(
    individual = rep(c(1L, 2L), each = 4),
    time = c(1:4, 1:4),
    x = rnorm(8),
    y = rnorm(8),
    behaviour = factor(c("REM", "REM", "wake", "wake", "wake", "wake", "REM", "REM"))
  )
  af <- set_metadata(
    af,
    variables_event = list(state = "behaviour", point = character())
  )

  ae <- as_anievent(af)
  expect_equal(nrow(ae), 4) # 2 bouts per individual
  expect_equal(sum(ae$individual == 1), 2)
  expect_equal(sum(ae$individual == 2), 2)
})

test_that("observation grouping isolates bouts across clips", {
  af <- aniframe(
    individual = rep(1L, 8),
    observation = c(rep("clip_a", 4), rep("clip_b", 4)),
    time = c(1:4, 1:4),
    x = rnorm(8),
    y = rnorm(8),
    behaviour = factor(c("REM", "REM", "wake", "wake", "REM", "REM", "REM", "wake"))
  )
  af <- set_metadata(
    af,
    variables_event = list(state = "behaviour", point = character())
  )

  ae <- as_anievent(af)
  expect_equal(nrow(ae), 4)
  expect_true("observation" %in% names(ae))
  expect_true("observation" %in% get_metadata(ae, "variables_when"))
})

test_that("metadata is inherited from the host aniframe", {
  af <- make_state_aniframe()
  af <- set_metadata(af, unit_time = "s", sampling_rate = 30)

  ae <- as_anievent(af)
  expect_equal(as.character(get_metadata(ae, "unit_time")), "s")
  expect_equal(get_metadata(ae, "sampling_rate"), 30)
})

test_that("as_anievent.aniframe errors when no event columns are declared", {
  af <- aniframe(individual = 1L, time = 1:3, x = 1:3, y = 1:3)
  expect_error(as_anievent(af), "no event columns declared")
})

test_that("as_anievent.aniframe errors when a declared column is missing", {
  af <- aniframe(individual = 1L, time = 1:3, x = 1:3, y = 1:3)
  af <- set_metadata(
    af,
    variables_event = list(state = "behaviour", point = character())
  )

  expect_error(as_anievent(af), "not present in the data")
})

test_that("as_anievent.aniframe handles an aniframe with no identity columns", {
  af <- as_aniframe(
    dplyr::tibble(
      time = 1:5,
      x = 1:5,
      y = 1:5,
      behaviour = factor(c("REM", "REM", "wake", "wake", "wake"))
    )
  )
  af <- set_metadata(af, variables_what = character())
  af <- set_metadata(
    af,
    variables_event = list(state = "behaviour", point = character())
  )

  ae <- as_anievent(af)
  expect_equal(nrow(ae), 2)
  expect_equal(ae$start, c(1, 3))
  expect_equal(ae$stop, c(2, 5))
})

test_that("as_anievent.aniframe returns an empty anievent when all event rows are NA", {
  af <- aniframe(
    individual = rep(1L, 3),
    time = 1:3,
    x = rnorm(3),
    y = rnorm(3),
    behaviour = factor(c(NA, NA, NA), levels = "REM"),
    call = factor(c(NA, NA, NA), levels = "alarm")
  )
  af <- set_metadata(
    af,
    variables_event = list(state = "behaviour", point = "call")
  )

  ae <- as_anievent(af)
  expect_s3_class(ae, "anievent")
  expect_equal(nrow(ae), 0)
})
