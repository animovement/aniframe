# Test outline for as_aniframe():
#
# Validation and minimal requirements:
#   - errors when no temporal variables found
#   - errors when spatial variables missing
#   - works with minimal required columns (time, x, y)
#   - works with custom spatial variables
#
# Type standardisation:
#   - converts character identity variables to factor
#   - converts character temporal variables to factor
#   - keeps integer temporal variables as integer
#   - converts spatial variables to numeric
#
# Column ordering and preservation:
#   - relocates columns to standard order (what, when, where)
#
#   - preserves non-standard columns
#
# Grouping and arrangement:
#   - groups by identity and temporal context (excluding time)
#   - arranges by identity first, then temporal
#
# Metadata:
#   - attaches metadata
#   - stores variables in metadata
#
# Custom variables:
#   - respects custom variables_what
#   - respects custom variables_when
#   - respects custom variables_where

test_that("as_aniframe errors when time column missing", {
  df <- data.frame(
    frame = 1:5,
    x = 1:5,
    y = 1:5
  )

  expect_error(
    as_aniframe(df),
    "time.*is required"
  )
})

test_that("as_aniframe errors when other temporal variables missing", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5
  )

  expect_error(
    as_aniframe(df, variables_when = c("trial", "time")),
    "Temporal variable.*not found.*trial"
  )
})

test_that("as_aniframe works with additional temporal variables", {
  df <- data.frame(
    trial = c(1L, 1L, 2L, 2L, 2L),
    time = c(1, 2, 1, 2, 3),
    x = 1:5,
    y = 1:5
  )

  result <- as_aniframe(df, variables_when = c("trial", "time"))

  expect_s3_class(result, "aniframe")
  expect_true("trial" %in% dplyr::group_vars(result))
  expect_false("time" %in% dplyr::group_vars(result))
})

test_that("as_aniframe works with minimal required columns", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5
  )

  result <- as_aniframe(df)

  expect_s3_class(result, "aniframe")
  expect_equal(names(result), c("time", "x", "y"))
})

test_that("as_aniframe works with custom spatial variables", {
  df <- data.frame(
    time = 1:5,
    z = 1:5
  )

  result <- as_aniframe(df, variables_where = "z")

  expect_s3_class(result, "aniframe")
  expect_equal(names(result), c("time", "z"))
})

test_that("as_aniframe converts character identity variables to factor", {
  df <- data.frame(
    individual = c("A", "A", "B", "B", "A"),
    time = 1:5,
    x = 1:5,
    y = 1:5
  )

  result <- as_aniframe(df, variables_what = "individual")

  expect_s3_class(result$individual, "factor")
  expect_equal(levels(result$individual), c("A", "B"))
})

test_that("as_aniframe converts character temporal variables to factor", {
  df <- data.frame(
    trial = c("trial1", "trial1", "trial2", "trial2", "trial1"),
    time = 1:5,
    x = 1:5,
    y = 1:5
  )

  result <- as_aniframe(df, variables_when = c("trial", "time"))

  expect_s3_class(result$trial, "factor")
})

test_that("as_aniframe keeps integer temporal variables as integer", {
  df <- data.frame(
    trial = c(1L, 1L, 2L, 2L, 3L),
    time = 1:5,
    x = 1:5,
    y = 1:5
  )

  result <- as_aniframe(df, variables_when = c("trial", "time"))

  expect_type(result$trial, "integer")
})

test_that("as_aniframe converts spatial variables to numeric", {
  df <- data.frame(
    time = 1:5,
    x = c("1", "2", "3", "4", "5"),
    y = 1:5
  )

  result <- as_aniframe(df)

  expect_type(result$x, "double")
})

test_that("as_aniframe relocates columns to standard order", {
  df <- data.frame(
    confidence = rep(0.9, 5),
    x = 1:5,
    time = 1:5,
    y = 1:5,
    individual = "A"
  )

  result <- as_aniframe(df, variables_what = "individual")

  expect_equal(names(result)[1:4], c("individual", "time", "x", "y"))
  expect_true("confidence" %in% names(result))
})

test_that("as_aniframe preserves non-standard columns", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5,
    custom_col = letters[1:5]
  )

  result <- as_aniframe(df)

  expect_true("custom_col" %in% names(result))
  expect_equal(result$custom_col, letters[1:5])
})

test_that("as_aniframe groups by identity and temporal context", {
  df <- data.frame(
    individual = c("A", "A", "A", "B", "B", "B"),
    trial = c(1L, 1L, 1L, 2L, 2L, 2L),
    time = 1:6,
    x = 1:6,
    y = 1:6
  )

  result <- as_aniframe(
    df,
    variables_what = "individual",
    variables_when = c("trial", "time")
  )

  expect_s3_class(result, "grouped_df")
  group_vars <- dplyr::group_vars(result)
  expect_true("individual" %in% group_vars)
  expect_true("trial" %in% group_vars)
  expect_false("time" %in% group_vars)
})

test_that("as_aniframe arranges by identity then temporal", {
  df <- data.frame(
    individual = c("B", "A", "B", "A", "B", "A"),
    time = c(3, 1, 2, 3, 1, 2),
    x = 1:6,
    y = 1:6
  )

  result <- as_aniframe(df, variables_what = "individual")

  # Should be arranged by individual, then by time within individual
  expect_equal(as.character(result$individual), c("A", "A", "A", "B", "B", "B"))
  expect_equal(result$time, c(1, 2, 3, 1, 2, 3))
})

test_that("as_aniframe attaches metadata", {
  df <- data.frame(
    time = 1:5,
    x = 1:5,
    y = 1:5
  )

  md <- list(sampling_rate = 30, source = "test")
  result <- as_aniframe(df, metadata = md)

  result_md <- get_metadata(result)
  expect_equal(result_md$sampling_rate, 30)
  expect_equal(result_md$source, "test")
})

test_that("as_aniframe stores variables in metadata", {
  df <- data.frame(
    individual = "A",
    trial = 1L,
    time = 1:5,
    x = 1:5,
    y = 1:5
  )

  result <- as_aniframe(
    df,
    variables_what = "individual",
    variables_when = c("trial", "time"),
    variables_where = c("x", "y")
  )

  result_md <- get_metadata(result)
  expect_equal(result_md$variables_what, "individual")
  expect_equal(result_md$variables_when, c("trial", "time"))
  expect_equal(result_md$variables_where, c("x", "y"))
})

test_that("as_aniframe respects custom variables_what", {
  df <- data.frame(
    track = c(1, 1, 2, 2, 3, 3),
    time = rep(1:2, 3),
    x = 1:6,
    y = 1:6
  )

  result <- as_aniframe(df, variables_what = "track")

  expect_equal(names(result)[1], "track")
  expect_true("track" %in% dplyr::group_vars(result))
})

test_that("as_aniframe respects custom variables_when with time", {
  df <- data.frame(
    session = c(1L, 1L, 2L, 2L),
    time = 1:4,
    x = 1:4,
    y = 1:4
  )

  result <- as_aniframe(df, variables_when = c("session", "time"))

  expect_s3_class(result, "aniframe")
  expect_equal(get_metadata(result)$variables_when, c("session", "time"))
})

# TODO: We need to handle the coordinate system before including this test

# test_that("as_aniframe respects custom variables_where", {
#   df <- data.frame(
#     time = 1:5,
#     lon = 1:5,
#     lat = 1:5
#   )

#   result <- as_aniframe(df, variables_where = c("lon", "lat"))

#   expect_s3_class(result, "aniframe")
#   expect_true(all(c("lon", "lat") %in% names(result)))
#   expect_type(result$lon, "double")
#   expect_type(result$lat, "double")
# })

test_that("as_aniframe works with full tidy movement data", {
  df <- data.frame(
    individual = c("A", "A", "B", "B", "A", "A", "B", "B"),
    keypoint = rep(c("head", "tail"), 4),
    session = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    trial = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
    time = rep(1:2, 4),
    x = 1:8,
    y = 1:8,
    confidence = rep(0.95, 8)
  )

  result <- as_aniframe(
    df,
    variables_what = c("individual", "keypoint"),
    variables_when = c("session", "trial", "time"),
    variables_where = c("x", "y")
  )

  expect_s3_class(result, "aniframe")
  expect_equal(
    names(result)[1:8],
    c(
      "individual",
      "keypoint",
      "session",
      "trial",
      "time",
      "x",
      "y",
      "confidence"
    )
  )

  # Check grouping
  group_vars <- dplyr::group_vars(result)
  expect_true(all(
    c("individual", "keypoint", "session", "trial") %in% group_vars
  ))
  expect_false("time" %in% group_vars)
})

test_that("as_aniframe infers coordinate system from spatial variables", {
  df_2d <- data.frame(time = 1:5, x = 1:5, y = 1:5)
  df_3d <- data.frame(time = 1:5, x = 1:5, y = 1:5, z = 1:5)
  df_polar <- data.frame(time = 1:5, rho = 1:5, phi = 1:5)

  result_2d <- as_aniframe(df_2d)
  result_3d <- as_aniframe(df_3d, variables_where = c("x", "y", "z"))
  result_polar <- as_aniframe(df_polar, variables_where = c("rho", "phi"))

  expect_equal(
    as.character(get_metadata(result_2d)$coordinate_system),
    "cartesian_2d"
  )
  expect_equal(
    as.character(get_metadata(result_3d)$coordinate_system),
    "cartesian_3d"
  )
  expect_equal(
    as.character(get_metadata(result_polar)$coordinate_system),
    "polar"
  )
})

# test_that("as_aniframe warns for unknown coordinate system", {
#   df <- data.frame(
#     time = 1:5,
#     lon = 1:5,
#     lat = 1:5
#   )

#   expect_warning(
#     as_aniframe(df, variables_where = c("lon", "lat")),
#     "Could not infer coordinate system"
#   )
# })

test_that("as_aniframe errors when no spatial variables found", {
  df <- data.frame(
    time = 1:5,
    value = 1:5
  )

  expect_error(
    as_aniframe(df),
    "No spatial variables found"
  )
})

test_that("as_aniframe errors when specified spatial variables missing", {
  df <- data.frame(
    time = 1:5,
    x = 1:5
  )

  expect_error(
    as_aniframe(df, variables_where = c("x", "y", "z")),
    "Missing spatial variable"
  )
})

test_that("as_aniframe warns for unknown coordinate system", {
  df <- data.frame(
    time = 1:5,
    lon = 1:5,
    lat = 1:5
  )

  expect_warning(
    as_aniframe(df, variables_where = c("lon", "lat")),
    "Could not infer coordinate system"
  )
})

test_that("as_aniframe detects polar coordinates", {
  df <- data.frame(
    time = 1:5,
    rho = 1:5,
    phi = seq(0, pi, length.out = 5)
  )

  result <- as_aniframe(df)

  expect_s3_class(result, "aniframe")
  expect_equal(get_metadata(result)$variables_where, c("rho", "phi"))
  expect_equal(as.character(get_metadata(result)$coordinate_system), "polar")
})

test_that("detect_variables_where returns NULL when no spatial columns", {
  df <- data.frame(
    time = 1:5,
    value = 1:5
  )

  result <- detect_variables_where(df)

  expect_null(result)
})
