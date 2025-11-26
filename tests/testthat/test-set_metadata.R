test_that("set_metadata works with named arguments", {
  data <- dplyr::tibble()

  result <- set_metadata(data, sampling_rate = 60, source = "deeplabcut")

  md <- get_metadata(result)
  expect_equal(md$sampling_rate, 60)
  expect_equal(md$source, "deeplabcut")
})

test_that("set_metadata works with metadata list", {
  data <- dplyr::tibble()

  md_list <- list(sampling_rate = 120, source = "sleap")
  result <- set_metadata(data, metadata = md_list)

  md <- get_metadata(result)
  expect_equal(md$sampling_rate, 120)
  expect_equal(md$source, "sleap")
})

test_that("set_metadata converts character to factor for factor fields", {
  data <- dplyr::tibble()

  result <- set_metadata(
    data,
    unit_space = "m",
    reference_frame = "egocentric"
  )

  md <- get_metadata(result)
  expect_s3_class(md$unit_space, "factor")
  expect_equal(as.character(md$unit_space), "m")
  expect_s3_class(md$reference_frame, "factor")
  expect_equal(as.character(md$reference_frame), "egocentric")
})

test_that("set_metadata converts character to factor in metadata list", {
  data <- dplyr::tibble()

  md_list <- list(
    unit_space = "cm",
    coordinate_system = "cartesian_3d"
  )
  result <- set_metadata(data, metadata = md_list)

  md <- get_metadata(result)
  expect_s3_class(md$unit_space, "factor")
  expect_equal(as.character(md$unit_space), "cm")
  expect_s3_class(md$coordinate_system, "factor")
  expect_equal(as.character(md$coordinate_system), "cartesian_3d")
})

test_that("set_metadata errors on invalid factor levels with character", {
  data <- dplyr::tibble()

  expect_error(
    set_metadata(data, unit_space = "invalid_unit"),
    "can only be"
  )

  expect_error(
    set_metadata(data, reference_frame = "not_a_frame"),
    "can only be"
  )
})

test_that("set_metadata errors on invalid factor levels in metadata list", {
  data <- dplyr::tibble()

  expect_error(
    set_metadata(data, metadata = list(unit_space = "invalid")),
    "can only be"
  )
})

test_that("set_metadata preserves factors with correct levels", {
  data <- dplyr::tibble()

  # Provide factor directly
  result <- set_metadata(
    data,
    unit_space = factor("mm", levels = levels(default_metadata()$unit_space))
  )

  md <- get_metadata(result)
  expect_s3_class(md$unit_space, "factor")
  expect_equal(as.character(md$unit_space), "mm")
  expect_equal(levels(md$unit_space), levels(default_metadata()$unit_space))
})

test_that("set_metadata errors when both ... and metadata are provided", {
  data <- dplyr::tibble()

  expect_error(
    set_metadata(data, sampling_rate = 30, metadata = list(source = "test")),
    "Metadata input can only be provided as either name-value pairs"
  )
})

test_that("set_metadata initializes default metadata if none exists", {
  data <- dplyr::tibble()
  # Remove metadata if it exists
  attr(data, "metadata") <- NULL

  result <- set_metadata(data, sampling_rate = 30)

  md <- get_metadata(result)
  expect_true(all(names(default_metadata()) %in% names(md)))
  expect_equal(md$sampling_rate, 30)
})

test_that("set_metadata merges with existing metadata", {
  data <- dplyr::tibble()
  data <- set_metadata(data, sampling_rate = 30, source = "original")

  result <- set_metadata(data, sampling_rate = 60)

  md <- get_metadata(result)
  expect_equal(md$sampling_rate, 60)
  expect_equal(md$source, "original") # Should be preserved
})

test_that("set_metadata overwrites existing values", {
  data <- dplyr::tibble()
  data <- set_metadata(data, sampling_rate = 30)

  result <- set_metadata(data, sampling_rate = 120)

  md <- get_metadata(result)
  expect_equal(md$sampling_rate, 120)
})

test_that("set_metadata works with empty arguments", {
  data <- dplyr::tibble()

  result <- set_metadata(data)

  # Should still have metadata (default or existing)
  expect_true(check_metadata_exists(result))
})

test_that("set_metadata preserves aniframe class", {
  data <- example_aniframe()

  result <- set_metadata(data, sampling_rate = 30)

  expect_s3_class(result, "aniframe")
})

test_that("set_metadata handles multiple fields at once", {
  data <- dplyr::tibble()

  result <- set_metadata(
    data,
    sampling_rate = 60,
    source = "test_source",
    filename = "test.csv",
    reference_frame = "egocentric"
  )

  md <- get_metadata(result)
  expect_equal(md$sampling_rate, 60)
  expect_equal(md$source, "test_source")
  expect_equal(md$filename, "test.csv")
  expect_equal(as.character(md$reference_frame), "egocentric")
})

test_that("set_metadata handles mixed character and non-character fields", {
  data <- dplyr::tibble()

  result <- set_metadata(
    data,
    sampling_rate = 90,
    source = "mixed_test",
    unit_space = "mm",
    unit_time = "s"
  )

  md <- get_metadata(result)
  expect_equal(md$sampling_rate, 90)
  expect_equal(md$source, "mixed_test")
  expect_s3_class(md$unit_space, "factor")
  expect_equal(as.character(md$unit_space), "mm")
  expect_s3_class(md$unit_time, "factor")
  expect_equal(as.character(md$unit_time), "s")
})

test_that("set_metadata validates metadata", {
  data <- dplyr::tibble()

  # This assumes validate_metadata() catches invalid metadata
  # Adjust based on your actual validation rules
  expect_error(
    set_metadata(data, sampling_rate = "not_a_number")
  )
})
