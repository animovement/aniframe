# Tests for the variables_event metadata field
#
# Default:
#   - present in default_metadata() with empty state and point character vectors
#
# Round-trip:
#   - set_metadata() stores the full named list under variables_event
#
# Validation:
#   - rejects overlap between $state and $point
#   - rejects non-character vectors in $state / $point
#   - accepts partial input (only $state or only $point); the missing
#     side fills in as character() — see #76
#   - treats NA / empty entries as "none"
#
# Backwards compatibility:
#   - metadata missing variables_event still passes ensure_valid_metadata()
#
# Print header:
#   - aniframe tbl_sum shows state/point variables when present

test_that("default_metadata() includes variables_event with empty state and point", {
  md <- default_metadata()

  expect_true("variables_event" %in% names(md))
  expect_type(md$variables_event, "list")
  expect_named(md$variables_event, c("state", "point"))
  expect_type(md$variables_event$state, "character")
  expect_type(md$variables_event$point, "character")
  expect_length(md$variables_event$state, 0)
  expect_length(md$variables_event$point, 0)
})

test_that("set_metadata round-trips variables_event", {
  data <- dplyr::tibble()

  result <- set_metadata(
    data,
    variables_event = list(
      state = c("sleep_state", "sleep_stage"),
      point = "call"
    )
  )

  ve <- get_metadata(result, "variables_event")
  expect_equal(ve$state, c("sleep_state", "sleep_stage"))
  expect_equal(ve$point, "call")
})

test_that("set_metadata rejects overlap between state and point", {
  data <- dplyr::tibble()

  expect_error(
    set_metadata(
      data,
      variables_event = list(state = "behaviour", point = "behaviour")
    ),
    "both a state and a point"
  )
})

test_that("set_metadata rejects non-character vectors in variables_event", {
  data <- dplyr::tibble()

  expect_error(
    set_metadata(
      data,
      variables_event = list(state = 1:3, point = character())
    ),
    "must be character vectors"
  )
})

test_that("set_metadata accepts variables_event with only state (#76)", {
  data <- dplyr::tibble()

  result <- set_metadata(
    data,
    variables_event = list(state = c("sleep_state", "sleep_stage"))
  )

  ve <- get_metadata(result, "variables_event")
  expect_equal(ve$state, c("sleep_state", "sleep_stage"))
  expect_equal(ve$point, character())
})

test_that("set_metadata accepts variables_event with only point (#76)", {
  data <- dplyr::tibble()

  result <- set_metadata(
    data,
    variables_event = list(point = "call")
  )

  ve <- get_metadata(result, "variables_event")
  expect_equal(ve$state, character())
  expect_equal(ve$point, "call")
})

test_that("set_metadata treats NA / empty variables_event entries as none (#76)", {
  data <- dplyr::tibble()

  result <- set_metadata(
    data,
    variables_event = list(state = "behaviour", point = NA)
  )

  ve <- get_metadata(result, "variables_event")
  expect_equal(ve$state, "behaviour")
  expect_equal(ve$point, character())
})

test_that("ensure_valid_metadata() tolerates metadata missing variables_event", {
  md <- default_metadata()
  md$variables_event <- NULL

  expect_no_error(ensure_valid_metadata(md))
})

test_that("ensure_valid_variables_event() returns invisibly on NULL", {
  expect_no_error(ensure_valid_variables_event(NULL))
})

test_that("tbl_sum.aniframe surfaces state and point variables in the header", {
  af <- aniframe(
    individual = rep(1L, 4),
    time = 1:4,
    x = rnorm(4),
    y = rnorm(4),
    behaviour = factor(c("REM", "REM", "wake", "wake")),
    call = factor(c(NA, "alarm", NA, NA))
  )
  af <- set_metadata(
    af,
    variables_event = list(state = "behaviour", point = "call")
  )

  header <- pillar::tbl_sum(af)
  expect_true("State event variables" %in% names(header))
  expect_equal(unname(header["State event variables"]), "behaviour")
  expect_true("Point event variables" %in% names(header))
  expect_equal(unname(header["Point event variables"]), "call")
})

test_that("tbl_sum.aniframe omits state/point rows when variables_event is empty", {
  af <- example_aniframe()
  header <- pillar::tbl_sum(af)

  expect_false("State event variables" %in% names(header))
  expect_false("Point event variables" %in% names(header))
})
