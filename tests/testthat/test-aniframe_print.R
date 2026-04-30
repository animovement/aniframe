# Test outline for tbl_sum.aniframe():
#
# Behaviour:
#   - includes "Individuals" only when the individual column is present
#   - includes "Keypoints" only when the keypoint column is present
#   - includes "Sessions" / "Trials" only when those columns are present
#   - includes "Sampling rate" only when set in metadata
#
# Regression:
#   - does not warn "Unknown or uninitialised column" when `individual` is
#     absent (issue #51)

test_that("tbl_sum omits Individuals row when individual column is absent", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5
  )
  data <- as_aniframe(df)

  result <- pillar::tbl_sum(data)

  expect_false("Individuals" %in% names(result))
  expect_true("Keypoints" %in% names(result))
})

test_that("tbl_sum does not warn when individual column is absent", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5
  )
  data <- as_aniframe(df)

  expect_no_warning(pillar::tbl_sum(data))
  expect_no_warning(format(data))
})

test_that("tbl_sum includes Individuals when individual column is present", {
  data <- example_aniframe(n_individuals = 2, n_keypoints = 3)

  result <- pillar::tbl_sum(data)

  expect_true("Individuals" %in% names(result))
  expect_true("Keypoints" %in% names(result))
})

test_that("tbl_sum includes Sessions and Trials only when present", {
  data <- example_aniframe(n_sessions = 2, n_trials = 3)
  result <- pillar::tbl_sum(data)
  expect_true("Sessions" %in% names(result))
  expect_true("Trials" %in% names(result))

  data_no_session <- example_aniframe(n_sessions = 1, n_trials = 1) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::any_of(c("session", "trial"))) |>
    suppressWarnings()
  result_no_session <- pillar::tbl_sum(data_no_session)
  expect_false("Sessions" %in% names(result_no_session))
  expect_false("Trials" %in% names(result_no_session))
})

test_that("tbl_sum includes Sampling rate only when set in metadata", {
  data <- example_aniframe()
  result <- pillar::tbl_sum(data)
  expect_false("Sampling rate" %in% names(result))

  data_with_sr <- set_metadata(data, sampling_rate = 60)
  result_with_sr <- pillar::tbl_sum(data_with_sr)
  expect_true("Sampling rate" %in% names(result_with_sr))
  expect_equal(unname(result_with_sr["Sampling rate"]), "60 Hz")
})
