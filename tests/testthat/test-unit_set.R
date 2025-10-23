# Test set_unit_space ----

test_that("set_unit_space converts between standard units correctly", {
  # Create test data in millimeters
  data <- dplyr::tibble(
    x = c(10, 20, 30),
    y = c(15, 25, 35),
    z = c(5, 10, 15),
    time = c(1, 2, 3)
  ) |>
    as_aniframe() |>
    set_metadata(unit_space = "mm")

  # Convert mm to cm
  result <- set_unit_space(data, to_unit = "cm")

  expect_equal(result$x, c(1, 2, 3))
  expect_equal(result$y, c(1.5, 2.5, 3.5))
  expect_equal(result$z, c(0.5, 1, 1.5))
  expect_equal(get_metadata(result, "unit_space") |> as.character(), "cm")
})

test_that("set_unit_space converts mm to m correctly", {
  data <- dplyr::tibble(
    x = c(1000, 2000, 3000),
    y = c(500, 1000, 1500),
    time = c(1, 2, 3)
  ) |>
    as_aniframe() |>
    set_metadata(unit_space = "mm")

  result <- set_unit_space(data, to_unit = "m")

  expect_equal(result$x, c(1, 2, 3))
  expect_equal(result$y, c(0.5, 1, 1.5))
  expect_equal(get_metadata(result, "unit_space") |> as.character(), "m")
})

test_that("set_unit_space converts cm to mm correctly", {
  data <- dplyr::tibble(
    x = c(1, 2, 3),
    y = c(1.5, 2.5, 3.5),
    time = c(1, 2, 3)
  ) |>
    as_aniframe() |>
    set_metadata(unit_space = "cm")

  result <- set_unit_space(data, to_unit = "mm")

  expect_equal(result$x, c(10, 20, 30))
  expect_equal(result$y, c(15, 25, 35))
  expect_equal(get_metadata(result, "unit_space") |> as.character(), "mm")
})

test_that("set_unit_space handles custom calibration factor", {
  data <- dplyr::tibble(
    x = c(100, 200, 300),
    y = c(150, 250, 350),
    time = c(1, 2, 3)
  ) |>
    as_aniframe() |>
    set_metadata(unit_space = "px")

  # 1 pixel = 0.5 mm
  result <- set_unit_space(data, to_unit = "mm", calibration_factor = 0.5)

  expect_equal(result$x, c(50, 100, 150))
  expect_equal(result$y, c(75, 125, 175))
  expect_equal(get_metadata(result, "unit_space") |> as.character(), "mm")
})

test_that("set_unit_space warns when calibration_factor is 1 for px/unknown", {
  data <- dplyr::tibble(
    x = c(100, 200, 300),
    y = c(150, 250, 350),
    time = c(1, 2, 3)
  ) |>
    as_aniframe() |>
    set_metadata(unit_space = "px")

  expect_message(
    set_unit_space(data, to_unit = "mm", calibration_factor = 1),
    "calibration_factor is not set"
  )
})

test_that("set_unit_space errors on invalid to_unit", {
  data <- dplyr::tibble(
    x = c(10, 20, 30),
    time = c(1, 2, 3)
  ) |>
    as_aniframe() |>
    set_metadata(unit_space = "mm")

  expect_error(
    set_unit_space(data, to_unit = "invalid_unit"),
    "Space unit can only be"
  )
})

test_that("set_unit_space handles missing spatial columns gracefully", {
  # Data with only x column
  data <- dplyr::tibble(
    x = c(10, 20, 30),
    time = c(1, 2, 3)
  ) |>
    as_aniframe() |>
    set_metadata(unit_space = "mm")

  result <- set_unit_space(data, to_unit = "cm")

  expect_equal(result$x, c(1, 2, 3))
  expect_false("y" %in% names(result))
  expect_false("z" %in% names(result))
})

test_that("set_unit_space preserves non-spatial columns", {
  data <- dplyr::tibble(
    x = c(10, 20, 30),
    y = c(15, 25, 35),
    time = c(1, 2, 3),
    id = c("a", "b", "c"),
    value = c(100, 200, 300)
  ) |>
    as_aniframe() |>
    set_metadata(unit_space = "mm")

  result <- set_unit_space(data, to_unit = "cm")

  expect_equal(result$id, c("a", "b", "c"))
  expect_equal(result$value, c(100, 200, 300))
  expect_equal(result$time, c(1, 2, 3))
})

# Test set_unit_time ----

test_that("set_unit_time converts between standard units correctly", {
  # Create test data in seconds
  data <- dplyr::tibble(
    x = c(10, 20, 30),
    y = c(15, 25, 35),
    time = c(1, 2, 3)
  ) |>
    as_aniframe() |>
    set_metadata(unit_time = "s")

  # Convert s to ms
  result <- set_unit_time(data, to_unit = "ms")

  expect_equal(result$time, c(1000, 2000, 3000))
  expect_equal(get_metadata(result, "unit_time") |> as.character(), "ms")
})

test_that("set_unit_time converts ms to s correctly", {
  data <- dplyr::tibble(
    x = c(10, 20, 30),
    time = c(1000, 2000, 3000)
  ) |>
    as_aniframe() |>
    set_metadata(unit_time = "ms")

  result <- set_unit_time(data, to_unit = "s")

  expect_equal(result$time, c(1, 2, 3))
  expect_equal(get_metadata(result, "unit_time") |> as.character(), "s")
})

test_that("set_unit_time converts s to m correctly", {
  data <- dplyr::tibble(
    x = c(10, 20, 30),
    time = c(60, 120, 180)
  ) |>
    as_aniframe() |>
    set_metadata(unit_time = "s")

  result <- set_unit_time(data, to_unit = "m")

  expect_equal(result$time, c(1, 2, 3))
  expect_equal(get_metadata(result, "unit_time") |> as.character(), "m")
})

test_that("set_unit_time converts m to h correctly", {
  data <- dplyr::tibble(
    x = c(10, 20, 30),
    time = c(60, 120, 180)
  ) |>
    as_aniframe() |>
    set_metadata(unit_time = "m")

  result <- set_unit_time(data, to_unit = "h")

  expect_equal(result$time, c(1, 2, 3))
  expect_equal(get_metadata(result, "unit_time") |> as.character(), "h")
})

test_that("set_unit_time converts h to s correctly", {
  data <- dplyr::tibble(
    x = c(10, 20, 30),
    time = c(1, 2, 3)
  ) |>
    as_aniframe() |>
    set_metadata(unit_time = "h")

  result <- set_unit_time(data, to_unit = "s")

  expect_equal(result$time, c(3600, 7200, 10800))
  expect_equal(get_metadata(result, "unit_time") |> as.character(), "s")
})

test_that("set_unit_time handles custom calibration factor", {
  data <- dplyr::tibble(
    x = c(10, 20, 30),
    time = c(0, 1, 2)
  ) |>
    as_aniframe() |>
    set_metadata(unit_time = "frame")

  # 30 frames per second (1 frame = 1/30 seconds)
  result <- set_unit_time(data, to_unit = "s", calibration_factor = 1 / 30)

  expect_equal(result$time, c(0, 1 / 30, 2 / 30))
  expect_equal(get_metadata(result, "unit_time") |> as.character(), "s")
})

test_that("set_unit_time warns when calibration_factor is 1 for frame/unknown", {
  data <- dplyr::tibble(
    x = c(10, 20, 30),
    time = c(1, 2, 3)
  ) |>
    as_aniframe() |>
    set_metadata(unit_time = "frame")

  expect_message(
    set_unit_time(data, to_unit = "s", calibration_factor = 1),
    "calibration_factor is not set"
  )
})

test_that("set_unit_time errors on invalid to_unit", {
  data <- dplyr::tibble(
    x = c(10, 20, 30),
    time = c(1, 2, 3)
  ) |>
    as_aniframe() |>
    set_metadata(unit_time = "s")

  expect_error(
    set_unit_time(data, to_unit = "invalid_unit"),
    "Time unit can only be"
  )
})

test_that("set_unit_time preserves spatial and other columns", {
  data <- dplyr::tibble(
    x = c(10, 20, 30),
    y = c(15, 25, 35),
    z = c(5, 10, 15),
    time = c(1, 2, 3),
    id = c("a", "b", "c"),
    value = c(100, 200, 300)
  ) |>
    as_aniframe() |>
    set_metadata(unit_time = "s")

  result <- set_unit_time(data, to_unit = "ms")

  expect_equal(result$x, c(10, 20, 30))
  expect_equal(result$y, c(15, 25, 35))
  expect_equal(result$z, c(5, 10, 15))
  expect_equal(result$id, c("a", "b", "c"))
  expect_equal(result$value, c(100, 200, 300))
})

# Test conversion_factors_space ----

test_that("conversion_factors_space returns correct matrix structure", {
  result <- conversion_factors_space()

  expect_true(is.matrix(result))
  expect_equal(dim(result), c(3, 3))
  expect_equal(rownames(result), c("mm", "cm", "m"))
  expect_equal(colnames(result), c("mm", "cm", "m"))
})

test_that("conversion_factors_space has correct diagonal values", {
  result <- conversion_factors_space()

  expect_equal(diag(result) |> as.vector(), c(1, 1, 1))
})

test_that("conversion_factors_space has correct conversion values", {
  result <- conversion_factors_space()

  # mm to cm
  expect_equal(result["cm", "mm"], 1 / 10)
  # mm to m
  expect_equal(result["m", "mm"], 1 / 1000)
  # cm to mm
  expect_equal(result["mm", "cm"], 10)
  # cm to m
  expect_equal(result["m", "cm"], 1 / 100)
  # m to mm
  expect_equal(result["mm", "m"], 1000)
  # m to cm
  expect_equal(result["cm", "m"], 100)
})

# Test conversion_factors_time ----

test_that("conversion_factors_time returns correct matrix structure", {
  result <- conversion_factors_time()

  expect_true(is.matrix(result))
  expect_equal(dim(result), c(4, 4))
  expect_equal(rownames(result), c("ms", "s", "m", "h"))
  expect_equal(colnames(result), c("ms", "s", "m", "h"))
})

test_that("conversion_factors_time has correct diagonal values", {
  result <- conversion_factors_time()

  expect_equal(diag(result) |> as.vector(), c(1, 1, 1, 1))
})

test_that("conversion_factors_time has correct conversion values", {
  result <- conversion_factors_time()

  # ms to s
  expect_equal(result["s", "ms"], 1 / 1000)
  # ms to m
  expect_equal(result["m", "ms"], 1 / (1000 * 60))
  # ms to h
  expect_equal(result["h", "ms"], 1 / (1000 * 60 * 60))
  # s to ms
  expect_equal(result["ms", "s"], 1000)
  # s to m
  expect_equal(result["m", "s"], 1 / 60)
  # s to h
  expect_equal(result["h", "s"], 1 / (60 * 60))
  # m to s
  expect_equal(result["s", "m"], 60)
  # m to h
  expect_equal(result["h", "m"], 1 / 60)
  # h to m
  expect_equal(result["m", "h"], 60)
  # h to s
  expect_equal(result["s", "h"], 60 * 60)
})

# Test get_conversion_factor_space ----

test_that("get_conversion_factor_space returns correct values", {
  expect_equal(get_conversion_factor_space("mm", "cm"), 1 / 10)
  expect_equal(get_conversion_factor_space("cm", "mm"), 10)
  expect_equal(get_conversion_factor_space("mm", "m"), 1 / 1000)
  expect_equal(get_conversion_factor_space("m", "mm"), 1000)
  expect_equal(get_conversion_factor_space("cm", "m"), 1 / 100)
  expect_equal(get_conversion_factor_space("m", "cm"), 100)
})

test_that("get_conversion_factor_space returns 1 for same units", {
  expect_equal(get_conversion_factor_space("mm", "mm"), 1)
  expect_equal(get_conversion_factor_space("cm", "cm"), 1)
  expect_equal(get_conversion_factor_space("m", "m"), 1)
})

# Test get_conversion_factor_time ----

test_that("get_conversion_factor_time returns correct values", {
  expect_equal(get_conversion_factor_time("ms", "s"), 1 / 1000)
  expect_equal(get_conversion_factor_time("s", "ms"), 1000)
  expect_equal(get_conversion_factor_time("s", "m"), 1 / 60)
  expect_equal(get_conversion_factor_time("m", "s"), 60)
  expect_equal(get_conversion_factor_time("m", "h"), 1 / 60)
  expect_equal(get_conversion_factor_time("h", "m"), 60)
  expect_equal(get_conversion_factor_time("s", "h"), 1 / 3600)
  expect_equal(get_conversion_factor_time("h", "s"), 3600)
})

test_that("get_conversion_factor_time returns 1 for same units", {
  expect_equal(get_conversion_factor_time("ms", "ms"), 1)
  expect_equal(get_conversion_factor_time("s", "s"), 1)
  expect_equal(get_conversion_factor_time("m", "m"), 1)
  expect_equal(get_conversion_factor_time("h", "h"), 1)
})
