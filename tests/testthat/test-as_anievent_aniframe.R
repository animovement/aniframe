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
    behaviour = factor(c(
      "REM",
      "REM",
      "wake",
      "wake",
      "wake",
      "wake",
      "REM",
      "REM"
    ))
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
    behaviour = factor(c(
      "REM",
      "REM",
      "wake",
      "wake",
      "REM",
      "REM",
      "REM",
      "wake"
    ))
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

test_that("as_anievent.aniframe picks up <channel>_modifiers list-columns", {
  af <- aniframe(
    individual = rep(1L, 5),
    time = 1:5,
    x = rnorm(5),
    y = rnorm(5),
    behaviour = factor(c("REM", "REM", "wake", "wake", "wake")),
    behaviour_modifiers = I(list(
      c("limb", "whisker"),
      c("limb", "whisker"),
      "tail",
      "tail",
      "tail"
    ))
  )
  af <- set_metadata(
    af,
    variables_event = list(state = "behaviour", point = character())
  )

  ae <- as_anievent(af)
  expect_true("modifiers" %in% names(ae))
  expect_equal(ae$modifiers[[1]], c("limb", "whisker"))
  expect_equal(ae$modifiers[[2]], "tail")
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

test_that("redundant identity columns (e.g. keypoint when behaviour is constant across keypoints) are dropped from bouts", {
  # behaviour is constant across keypoint within (individual, time)
  af <- aniframe(
    individual = rep(1L, 6),
    keypoint = rep(c("head", "tail"), each = 3),
    time = rep(1:3, 2),
    x = rnorm(6),
    y = rnorm(6),
    behaviour = factor(
      c("REM", "REM", "wake", "REM", "REM", "wake"),
      levels = c("REM", "wake")
    )
  )
  af <- set_metadata(
    af,
    variables_event = list(state = "behaviour", point = character())
  )

  ae <- as_anievent(af)

  # Should be 2 bouts (REM 1-2, wake 3-3), not 4 (duplicated per keypoint)
  expect_equal(nrow(ae), 2)
  expect_false("keypoint" %in% names(ae))
  expect_false("keypoint" %in% get_metadata(ae, "variables_what"))
  expect_equal(ae$start, c(1, 3))
  expect_equal(ae$stop, c(2, 3))
})

test_that("non-redundant identity columns (e.g. behaviour varying by keypoint) are kept", {
  af <- aniframe(
    individual = rep(1L, 6),
    keypoint = rep(c("head", "tail"), each = 3),
    time = rep(1:3, 2),
    x = rnorm(6),
    y = rnorm(6),
    # head: always REM; tail: always wake -> varies by keypoint
    behaviour = factor(
      c("REM", "REM", "REM", "wake", "wake", "wake"),
      levels = c("REM", "wake")
    )
  )
  af <- set_metadata(
    af,
    variables_event = list(state = "behaviour", point = character())
  )

  ae <- as_anievent(af)
  expect_true("keypoint" %in% names(ae))
  expect_true("keypoint" %in% get_metadata(ae, "variables_what"))
  expect_equal(nrow(ae), 2) # one bout per keypoint
})

test_that("scope is collapsed to nothing when an event is constant for everyone at each time", {
  af <- aniframe(
    individual = c(1L, 1L, 2L, 2L),
    time = c(1:2, 1:2),
    x = rnorm(4),
    y = rnorm(4),
    epoch = factor(c("A", "B", "A", "B")) # same value for both individuals at each time
  )
  af <- set_metadata(
    af,
    variables_event = list(state = "epoch", point = character())
  )

  ae <- as_anievent(af)
  expect_false("individual" %in% names(ae))
  expect_length(get_metadata(ae, "variables_what"), 0)
  expect_equal(nrow(ae), 2) # one A bout, one B bout
})

test_that("channels with disagreeing scopes error with a helpful message", {
  af <- aniframe(
    individual = rep(1L, 6),
    keypoint = rep(c("head", "tail"), each = 3),
    time = rep(1:3, 2),
    x = rnorm(6),
    y = rnorm(6),
    # behaviour: individual-scope (constant across keypoint)
    behaviour = factor(rep(c("REM", "REM", "wake"), 2)),
    # limb_extended: keypoint-scope (varies by keypoint)
    limb_extended = factor(rep(c("yes", "no"), each = 3))
  )
  af <- set_metadata(
    af,
    variables_event = list(
      state = c("behaviour", "limb_extended"),
      point = character()
    )
  )

  expect_error(as_anievent(af), "disagree on their identity scope")
})

test_that("point channel keeps identity columns when the scope detection requires it", {
  af <- aniframe(
    individual = rep(c(1L, 2L), each = 4),
    time = rep(1:4, 2),
    x = rnorm(8),
    y = rnorm(8),
    # individual 1 has alarm only at t=2; individual 2 has alarm only at t=3
    call = factor(
      c(NA, "alarm", NA, NA, NA, NA, "alarm", NA),
      levels = "alarm"
    )
  )
  af <- set_metadata(
    af,
    variables_event = list(state = character(), point = "call")
  )

  ae <- as_anievent(af)
  expect_true("individual" %in% names(ae))
  expect_equal(nrow(ae), 2)
  expect_equal(sort(ae$individual), c(1L, 2L))
})

test_that("empty-rows path keeps identity columns when forced via variables_what", {
  # All-NA event with an explicit variables_what override -> empty bout df
  # is produced with the (forced) identity column present.
  af <- aniframe(
    individual = c(1L, 2L),
    time = c(1, 2),
    x = c(1, 2),
    y = c(1, 2),
    call = factor(c(NA, NA), levels = "alarm")
  )
  af <- set_metadata(
    af,
    variables_event = list(state = character(), point = "call")
  )

  ae <- as_anievent(af, variables_what = "individual")
  expect_equal(nrow(ae), 0)
  expect_true("individual" %in% names(ae))
})

test_that("explicit variables_what overrides scope detection", {
  af <- aniframe(
    individual = rep(1L, 6),
    keypoint = rep(c("head", "tail"), each = 3),
    time = rep(1:3, 2),
    x = rnorm(6),
    y = rnorm(6),
    behaviour = factor(rep(c("REM", "REM", "wake"), 2))
  )
  af <- set_metadata(
    af,
    variables_event = list(state = "behaviour", point = character())
  )

  # Force keypoint to be kept even though it's redundant
  ae <- as_anievent(af, variables_what = c("individual", "keypoint"))
  expect_true("keypoint" %in% names(ae))
  expect_equal(nrow(ae), 4) # duplicate per keypoint
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
