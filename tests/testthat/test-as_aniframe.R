test_that("as_aniframe validates required columns", {
  df <- data.frame(x = 1:5, y = 1:5)

  expect_error(
    as_aniframe(df),
    "time"  # Adjust based on your validate_cols error message
  )
})

test_that("as_aniframe works with minimal required columns", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5
  )

  result <- suppressMessages(as_aniframe(df))

  expect_s3_class(result, "aniframe")
  expect_true("keypoint" %in% names(result))
  expect_true("individual" %in% names(result))
})

test_that("as_aniframe works with only a z column", {
  df <- data.frame(
    time = 1:5,
    z = 1:5
  )

  result <- suppressMessages(as_aniframe(df))

  expect_s3_class(result, "aniframe")
  expect_true("keypoint" %in% names(result))
  expect_true("individual" %in% names(result))
})

test_that("as_aniframe creates keypoint column with default value", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5
  )

  result <- suppressMessages(as_aniframe(df))
  expect_true(is.na(result$keypoint[1]))
  expect_s3_class(result$keypoint, "factor")
})

test_that("as_aniframe converts existing keypoint to factor", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5,
    keypoint = c("nose", "nose", "tail", "tail", "nose")
  )

  result <- suppressMessages(as_aniframe(df))

  expect_s3_class(result$keypoint, "factor")
  expect_equal(levels(result$keypoint), c("nose", "tail"))
})

test_that("as_aniframe creates individual column with NA", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5
  )

  result <- suppressMessages(as_aniframe(df))
  expect_true(all(is.na(result$individual)))
  expect_s3_class(result$individual, "factor")
})

test_that("as_aniframe converts existing individual to factor", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5,
    individual = c("A", "A", "B", "B", "A")
  )

  result <- suppressMessages(as_aniframe(df))

  expect_s3_class(result$individual, "factor")
  expect_equal(levels(result$individual), c("A", "B"))
})

test_that("as_aniframe converts numeric trial to integer", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5,
    trial = c(1, 1, 2, 2, 3)
  )

  result <- suppressMessages(as_aniframe(df))

  expect_type(result$trial, "integer")
})

test_that("as_aniframe converts character trial to factor", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5,
    trial = c("trial1", "trial1", "trial2", "trial2", "trial1")
  )

  result <- suppressMessages(as_aniframe(df))

  expect_s3_class(result$trial, "factor")
})

test_that("as_aniframe converts numeric session to integer", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5,
    session = c(1, 1, 2, 2, 1)
  )

  result <- suppressMessages(as_aniframe(df))

  expect_type(result$session, "integer")
})

test_that("as_aniframe converts character session to factor", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5,
    session = c("morning", "morning", "evening", "evening", "morning")
  )

  result <- suppressMessages(as_aniframe(df))

  expect_s3_class(result$session, "factor")
})

test_that("as_aniframe relocates columns to standard order", {
  df <- data.frame(
    confidence = rep(0.9, 5),
    x = 1:5,
    time = 1:5,
    y = 1:5,
    z = 1:5
  )

  result <- suppressMessages(as_aniframe(df))

  standard_cols <- c("session", "trial", "individual", "keypoint", "time",
                     "x", "y", "z", "confidence")
  present_standard <- standard_cols[standard_cols %in% names(result)]

  expect_equal(names(result)[1:length(present_standard)], present_standard)
})

test_that("as_aniframe preserves non-standard columns", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5,
    custom_col = letters[1:5]
  )

  result <- suppressMessages(as_aniframe(df))

  expect_true("custom_col" %in% names(result))
  expect_equal(result$custom_col, letters[1:5])
})

test_that("as_aniframe groups by appropriate columns", {
  df <- data.frame(
    time = 1:6,
    x = 1:6,
    y = 1:6,
    trial = c(1, 1, 1, 2, 2, 2),
    individual = c("A", "A", "A", "B", "B", "B")
  )

  result <- suppressMessages(as_aniframe(df))

  expect_s3_class(result, "grouped_df")
  expect_true(all(c("trial", "individual", "keypoint") %in% dplyr::group_vars(result)))
})

test_that("as_aniframe attaches metadata", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5
  )

  md <- list(sampling_rate = 30, source = "test")
  result <- suppressMessages(as_aniframe(df, metadata = md))

  result_md <- get_metadata(result)
  expect_equal(result_md$sampling_rate, 30)
  expect_equal(result_md$source, "test")
})

test_that("as_aniframe works with all standard columns", {
  df <- data.frame(
    session = c(1, 1, 1, 2, 2, 2),
    trial = c(1, 1, 2, 1, 1, 2),
    individual = c("A", "B", "A", "A", "B", "A"),
    keypoint = rep("nose", 6),
    time = 1:6,
    x = 1:6,
    y = 1:6,
    z = 1:6,
    confidence = rep(0.95, 6)
  )

  result <- as_aniframe(df)

  expect_s3_class(result, "aniframe")
  expect_equal(names(result)[1:9],
               c("session", "trial", "individual", "keypoint", "time",
                 "x", "y", "z", "confidence"))
})

test_that("as_aniframe arranges by groups", {
  df <- data.frame(
    time = c(3, 1, 2, 6, 4, 5),
    x = 1:6,
    y = 1:6,
    trial = c(1, 1, 1, 2, 2, 2)
  )

  result <- suppressMessages(as_aniframe(df))

  # Should be arranged by trial, then by time within trial
  expect_equal(result$trial, c(1, 1, 1, 2, 2, 2))
  expect_equal(result$time, c(1, 2, 3, 4, 5, 6))
})
