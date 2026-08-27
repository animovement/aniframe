test_that("get_metadata() returns the full list when fields is NULL", {
  data <- example_aniframe()
  md <- get_metadata(data)
  expect_s3_class(md, "aniframe_metadata")
  expect_true("variables_what" %in% names(md))
  expect_true("connections" %in% names(md))
})

test_that("get_metadata() returns a single value when fields is a length-1 vector", {
  data <- example_aniframe() |>
    set_metadata(sampling_rate = 30)
  expect_equal(get_metadata(data, "sampling_rate"), 30)
})

test_that("get_metadata() returns a sub-list when fields has length > 1", {
  data <- example_aniframe() |>
    set_metadata(sampling_rate = 30, source = "test")

  sub <- get_metadata(data, c("sampling_rate", "source"))

  expect_s3_class(sub, "aniframe_metadata")
  expect_named(sub, c("sampling_rate", "source"), ignore.order = TRUE)
  expect_equal(sub$sampling_rate, 30)
  expect_equal(sub$source, "test")
})
