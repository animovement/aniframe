# Tests for add_events()
#
# Happy path:
#   - state channel: a per-frame factor column, NA outside any bout
#   - point channel: only the matching frame gets a value
#   - multiple channels added in one call
#   - per-individual matching: events for individual 1 don't bleed into individual 2
#   - per-observation matching
#   - variables_event metadata on the result registers each channel as state/point
#
# Auto-detect type:
#   - all bouts with start == stop -> point
#   - any bout with stop > start -> state
#
# Unit reconciliation:
#   - SI <-> SI conversion happens transparently
#   - frame <-> SI requires sampling_rate; errors without it
#   - identical units pass through unchanged
#
# Errors:
#   - channel-name collides with an existing host column
#   - events have overlapping bouts in the same channel (caught by validate_anievent)
#   - non-aniframe data, non-anievent events

test_that("state channel fills frames within bouts and NA outside", {
  af <- aniframe(individual = 1L, time = 1:10, x = 1:10, y = 1:10)
  ae <- anievent(
    individual = 1L,
    channel = c("behaviour", "behaviour"),
    value = c("REM", "wake"),
    start = c(1, 5),
    stop = c(4, 10)
  )

  result <- add_events(af, ae)
  expect_true("behaviour" %in% names(result))
  expect_equal(
    as.character(result$behaviour),
    c(rep("REM", 4), rep("wake", 6))
  )
})

test_that("point channel only fills the matching frame", {
  af <- aniframe(individual = 1L, time = 1:5, x = 1:5, y = 1:5)
  ae <- anievent(
    individual = 1L,
    channel = "call",
    value = "alarm",
    start = 3,
    stop = 3
  )

  result <- add_events(af, ae)
  expect_equal(as.character(result$call), c(NA, NA, "alarm", NA, NA))
})

test_that("multiple channels are added in one call", {
  af <- aniframe(individual = 1L, time = 1:5, x = 1:5, y = 1:5)
  ae <- anievent(
    individual = 1L,
    channel = c("behaviour", "call"),
    value = c("REM", "alarm"),
    start = c(1, 3),
    stop = c(4, 3)
  )

  result <- add_events(af, ae)
  expect_true(all(c("behaviour", "call") %in% names(result)))
})

test_that("auto-detects state vs point and registers in variables_event", {
  af <- aniframe(individual = 1L, time = 1:5, x = 1:5, y = 1:5)
  ae <- anievent(
    individual = 1L,
    channel = c("behaviour", "call"),
    value = c("REM", "alarm"),
    start = c(1, 3),
    stop = c(4, 3)
  )

  result <- add_events(af, ae)
  ve <- get_metadata(result, "variables_event")
  expect_equal(ve$state, "behaviour")
  expect_equal(ve$point, "call")
})

test_that("per-individual matching keeps events on the right subject", {
  af <- aniframe(
    individual = rep(c(1L, 2L), each = 5),
    time = rep(1:5, 2),
    x = rnorm(10),
    y = rnorm(10)
  )
  ae <- anievent(
    individual = c(1L, 2L),
    channel = c("behaviour", "behaviour"),
    value = c("REM", "wake"),
    start = c(1, 1),
    stop = c(3, 3)
  )

  result <- add_events(af, ae)
  expect_equal(
    as.character(result$behaviour[result$individual == 1])[1:3],
    rep("REM", 3)
  )
  expect_equal(
    as.character(result$behaviour[result$individual == 2])[1:3],
    rep("wake", 3)
  )
})

test_that("per-observation matching isolates clips", {
  af <- aniframe(
    individual = rep(1L, 8),
    observation = c(rep("clip_a", 4), rep("clip_b", 4)),
    time = c(1:4, 1:4),
    x = rnorm(8),
    y = rnorm(8)
  )
  ae <- anievent(
    individual = c(1L, 1L),
    observation = c("clip_a", "clip_b"),
    channel = c("behaviour", "behaviour"),
    value = c("REM", "wake"),
    start = c(1, 1),
    stop = c(2, 2)
  )

  result <- add_events(af, ae)
  expect_equal(
    as.character(result$behaviour[result$observation == "clip_a"]),
    c("REM", "REM", NA, NA)
  )
  expect_equal(
    as.character(result$behaviour[result$observation == "clip_b"]),
    c("wake", "wake", NA, NA)
  )
})

test_that("SI <-> SI unit conversion happens automatically", {
  af <- aniframe(individual = 1L, time = 1:5, x = 1:5, y = 1:5)
  af <- set_metadata(af, unit_time = "s")
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 1000,
    stop = 4000
  )
  ae <- set_metadata(ae, unit_time = "ms")

  result <- add_events(af, ae)
  expect_equal(as.character(result$behaviour), c(rep("REM", 4), NA))
})

test_that("frame -> SI conversion uses sampling_rate from events", {
  af <- aniframe(individual = 1L, time = 1:5, x = 1:5, y = 1:5)
  af <- set_metadata(af, unit_time = "s")
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 30,
    stop = 120
  )
  ae <- set_metadata(ae, sampling_rate = 30)

  result <- add_events(af, ae)
  expect_equal(as.character(result$behaviour), c(rep("REM", 4), NA))
})

test_that("frame <-> SI without sampling_rate errors", {
  af <- aniframe(individual = 1L, time = 1:5, x = 1:5, y = 1:5)
  af <- set_metadata(af, unit_time = "s")
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 1,
    stop = 4
  )
  # ae stays in default "frame" with no sampling_rate

  expect_error(add_events(af, ae), "Cannot reconcile")
})

test_that("SI -> frame conversion uses sampling_rate to invert", {
  af <- aniframe(individual = 1L, time = 1:200, x = rnorm(200), y = rnorm(200))
  # host stays in frame, has sampling_rate
  af <- set_metadata(af, sampling_rate = 30)
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 1,
    stop = 3
  )
  ae <- set_metadata(ae, unit_time = "s")

  result <- add_events(af, ae)
  # 1s to 3s at 30 fps = frames 30-90
  filled <- which(!is.na(result$behaviour))
  expect_equal(filled[1], 30)
  expect_equal(filled[length(filled)], 90)
})

test_that("collision between channel name and existing host column errors", {
  af <- aniframe(individual = 1L, time = 1:5, x = 1:5, y = 1:5)
  ae <- anievent(
    individual = 1L,
    channel = "x", # collides with spatial column on host
    value = "REM",
    start = 1,
    stop = 3
  )

  expect_error(add_events(af, ae), "collide")
})

test_that("overlapping bouts in the same channel for the same subject error", {
  af <- aniframe(individual = 1L, time = 1:10, x = 1:10, y = 1:10)
  ae <- dplyr::tibble(
    individual = c(1L, 1L),
    channel = c("behaviour", "behaviour"),
    value = factor(c("REM", "wake")),
    start = c(1, 3),
    stop = c(5, 7)
  ) |>
    as_anievent()

  expect_error(add_events(af, ae), "overlap")
})

test_that("non-aniframe / non-anievent inputs error", {
  af <- aniframe(individual = 1L, time = 1:5, x = 1:5, y = 1:5)
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 1,
    stop = 3
  )

  expect_error(add_events(data.frame(), ae), "not an aniframe")
  expect_error(add_events(af, data.frame()), "not an anievent")
})

test_that("identical unit_time bypasses conversion", {
  af <- aniframe(individual = 1L, time = 1:5, x = 1:5, y = 1:5)
  af <- set_metadata(af, unit_time = "s")
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 1,
    stop = 3
  )
  ae <- set_metadata(ae, unit_time = "s")

  result <- add_events(af, ae)
  expect_equal(as.character(result$behaviour), c("REM", "REM", "REM", NA, NA))
})

test_that("frame -> non-second SI host conversion works", {
  af <- aniframe(
    individual = 1L,
    time = c(0, 50, 100, 150, 200),
    x = 1:5,
    y = 1:5
  )
  af <- set_metadata(af, unit_time = "ms")
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 1,
    stop = 5
  )
  ae <- set_metadata(ae, sampling_rate = 20) # 20 fps -> 50ms per frame

  result <- add_events(af, ae)
  # frame 1 = 50ms, frame 5 = 250ms; host at 50, 100, 150, 200 -> all REM
  filled <- which(!is.na(result$behaviour))
  expect_equal(filled, 2:5)
})

test_that("non-second SI events -> frame host conversion works", {
  af <- aniframe(individual = 1L, time = 1:200, x = rnorm(200), y = rnorm(200))
  af <- set_metadata(af, sampling_rate = 30)
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 1000,
    stop = 3000
  )
  ae <- set_metadata(ae, unit_time = "ms")

  result <- add_events(af, ae)
  filled <- which(!is.na(result$behaviour))
  expect_equal(filled[1], 30)
  expect_equal(filled[length(filled)], 90)
})

test_that("frame <-> unknown unit reconciliation is a no-op on values", {
  af <- aniframe(individual = 1L, time = 1:5, x = 1:5, y = 1:5)
  # host stays in default "frame"
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 1,
    stop = 3
  )
  ae <- set_metadata(ae, unit_time = "unknown")

  result <- add_events(af, ae)
  expect_equal(as.character(result$behaviour), c("REM", "REM", "REM", NA, NA))
})

test_that("add_events works when host has no identity columns", {
  af <- as_aniframe(dplyr::tibble(time = 1:5, x = 1:5, y = 1:5))
  af <- set_metadata(af, variables_what = character())
  ae <- anievent(
    channel = c("behaviour", "behaviour"),
    value = c("REM", "wake"),
    start = c(1, 4),
    stop = c(3, 5)
  )

  result <- add_events(af, ae)
  expect_equal(
    as.character(result$behaviour),
    c("REM", "REM", "REM", "wake", "wake")
  )
})

test_that("modifiers on the events broadcast to <channel>_modifiers on the host", {
  af <- aniframe(individual = 1L, time = 1:10, x = 1:10, y = 1:10)
  ae <- anievent(
    individual = c(1L, 1L),
    channel = c("behaviour", "behaviour"),
    value = c("REM", "wake"),
    start = c(1, 5),
    stop = c(4, 10),
    modifiers = list(
      c("limb", "whisker"),
      "tail"
    )
  )

  result <- add_events(af, ae)
  expect_true("behaviour_modifiers" %in% names(result))
  expect_type(result$behaviour_modifiers, "list")

  # Frames 1-4: REM bout -> c("limb", "whisker")
  for (i in 1:4) {
    expect_equal(result$behaviour_modifiers[[i]], c("limb", "whisker"))
  }
  # Frames 5-10: wake bout -> "tail"
  for (i in 5:10) {
    expect_equal(result$behaviour_modifiers[[i]], "tail")
  }
})

test_that("channels without any non-empty modifiers don't add a <channel>_modifiers column", {
  af <- aniframe(individual = 1L, time = 1:5, x = 1:5, y = 1:5)
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 1,
    stop = 4,
    modifiers = list(character()) # explicitly empty
  )

  result <- add_events(af, ae)
  expect_false("behaviour_modifiers" %in% names(result))
})

test_that("mixed-channel modifiers add a column only for the channel that has them", {
  af <- aniframe(individual = 1L, time = 1:10, x = 1:10, y = 1:10)
  ae <- anievent(
    individual = c(1L, 1L),
    channel = c("behaviour", "call"),
    value = c("REM", "alarm"),
    start = c(1, 3),
    stop = c(4, 3),
    modifiers = list(
      c("limb", "whisker"), # behaviour has modifiers
      character() # call doesn't
    )
  )

  result <- add_events(af, ae)
  expect_true("behaviour_modifiers" %in% names(result))
  expect_false("call_modifiers" %in% names(result))
})

test_that("frames outside any bout get an empty character() in <channel>_modifiers", {
  af <- aniframe(individual = 1L, time = 1:6, x = 1:6, y = 1:6)
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 2,
    stop = 4,
    modifiers = list(c("limb"))
  )

  result <- add_events(af, ae)
  expect_equal(result$behaviour_modifiers[[1]], character())
  expect_equal(result$behaviour_modifiers[[5]], character())
  expect_equal(result$behaviour_modifiers[[6]], character())
})

test_that("anievent -> aniframe -> anievent round-trips modifiers", {
  af <- aniframe(individual = 1L, time = 1:10, x = 1:10, y = 1:10)
  ae <- anievent(
    individual = c(1L, 1L, 1L),
    channel = c("behaviour", "behaviour", "call"),
    value = c("REM", "wake", "alarm"),
    start = c(1, 5, 3),
    stop = c(4, 10, 3),
    modifiers = list(
      c("limb", "whisker"),
      "tail",
      "high"
    )
  )

  af_back <- add_events(af, ae)
  ae_back <- as_anievent(af_back)

  expect_true("modifiers" %in% names(ae_back))
  rem_row <- which(ae_back$channel == "behaviour" & ae_back$value == "REM")
  expect_equal(ae_back$modifiers[[rem_row]], c("limb", "whisker"))
  wake_row <- which(ae_back$channel == "behaviour" & ae_back$value == "wake")
  expect_equal(ae_back$modifiers[[wake_row]], "tail")
  alarm_row <- which(ae_back$channel == "call")
  expect_equal(ae_back$modifiers[[alarm_row]], "high")
})

test_that("add_events accumulates into an existing variables_event", {
  af <- aniframe(individual = 1L, time = 1:10, x = 1:10, y = 1:10)
  af <- set_metadata(
    af,
    variables_event = list(state = "pre_existing", point = character())
  )
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 1,
    stop = 4
  )

  result <- add_events(af, ae)
  ve <- get_metadata(result, "variables_event")
  expect_setequal(ve$state, c("pre_existing", "behaviour"))
})
