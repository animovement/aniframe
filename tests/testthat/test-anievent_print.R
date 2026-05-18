# Tests for tbl_sum.anievent (the print header for anievent)
#
# Coverage:
#   - default header shows class name and identity row
#   - "Event channels" row lists unique values from the `channel` column
#   - "Sampling rate" row appears when set in metadata
#   - zero-row anievent omits the "Event channels" row

test_that("tbl_sum.anievent shows class name and identity row", {
  ae <- anievent(
    individual = c(1L, 1L, 2L),
    channel = c("behaviour", "call", "behaviour"),
    value = c("REM", "alarm", "wake"),
    start = c(3, 4.5, 14),
    stop = c(9, 4.5, 19)
  )

  header <- pillar::tbl_sum(ae)

  expect_true("anievent" %in% names(header))
  expect_true("Individuals" %in% names(header))
})

test_that("tbl_sum.anievent surfaces event channels from the channel column", {
  ae <- anievent(
    individual = c(1L, 1L),
    channel = c("behaviour", "call"),
    value = c("REM", "alarm"),
    start = c(3, 4.5),
    stop = c(9, 4.5)
  )

  header <- pillar::tbl_sum(ae)
  expect_true("Event channels" %in% names(header))
  expect_match(unname(header["Event channels"]), "behaviour")
  expect_match(unname(header["Event channels"]), "call")
})

test_that("tbl_sum.anievent surfaces sampling rate when set", {
  ae <- anievent(
    individual = 1L,
    channel = "behaviour",
    value = "REM",
    start = 3,
    stop = 9
  )
  ae <- set_metadata(ae, sampling_rate = 30)

  header <- pillar::tbl_sum(ae)
  expect_true("Sampling rate" %in% names(header))
  expect_equal(unname(header["Sampling rate"]), "30 Hz")
})

test_that("tbl_sum.anievent omits event channels row on a zero-row object", {
  ae <- anievent(
    individual = integer(),
    channel = character(),
    value = factor(character()),
    start = numeric(),
    stop = numeric()
  )

  header <- pillar::tbl_sum(ae)
  expect_false("Event channels" %in% names(header))
})
