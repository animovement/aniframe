# The index (#109) ----

test_that("a frame with no declaration is indexed by time", {
  af <- aniframe(individual = "a", time = 1:3, x = c(1, 2, 3), y = c(0, 1, 0))

  expect_equal(get_index(af), "time")
  expect_equal(get_variables_when(af), "time")
})

test_that("a frame can be indexed by a column that is not called time", {
  df <- data.frame(
    frame = 1:3,
    individual = "a",
    x = c(1, 2, 3),
    y = c(0, 1, 0)
  )

  af <- as_aniframe(df, index = "frame")

  expect_equal(get_index(af), "frame")
  # The index is one of the temporal variables, never a separate declaration.
  expect_true("frame" %in% get_variables_when(af))
  # No column literally named `time` is required any more.
  expect_false("time" %in% names(af))
})

test_that("the index is numeric and the temporal context is not", {
  df <- data.frame(
    frame = c(1, 2, 1, 2),
    session = c("a", "a", "b", "b"),
    individual = "x",
    x = 1:4,
    y = 1:4
  )

  af <- as_aniframe(df, index = "frame")

  expect_true(is.numeric(af$frame))
  expect_s3_class(af$session, "factor")
})

test_that("the frame is grouped by identity and context, never by the index", {
  df <- data.frame(
    frame = c(1, 2, 1, 2),
    session = c("a", "a", "b", "b"),
    individual = "x",
    x = 1:4,
    y = 1:4
  )

  af <- as_aniframe(df, index = "frame")

  expect_setequal(dplyr::group_vars(af), c("individual", "session"))
  expect_false("frame" %in% dplyr::group_vars(af))
})

test_that("set_index() moves the index and regroups the frame", {
  af <- aniframe(
    individual = "a",
    time = 1:3,
    x = c(1, 2, 3),
    y = c(0, 1, 0)
  ) |>
    dplyr::mutate(tick = c(10, 20, 30))

  result <- set_index(af, "tick")

  expect_equal(get_index(result), "tick")
  # `time` is now ordinary temporal context, so it groups.
  expect_true("time" %in% dplyr::group_vars(result))
  expect_false("tick" %in% dplyr::group_vars(result))
})

test_that("set_index() rejects a column that cannot be an index", {
  af <- aniframe(individual = "a", time = 1:3, x = c(1, 2, 3), y = c(0, 1, 0))

  expect_error(set_index(af, "absent"), "not present")
  expect_error(set_index(af, "individual"), "must be numeric")
  expect_error(set_index(af, c("time", "x")), "single column name")
})

test_that("as_aniframe() aborts when the declared index is absent", {
  df <- data.frame(
    frame = 1:3,
    individual = "a",
    x = c(1, 2, 3),
    y = c(0, 1, 0)
  )

  expect_error(as_aniframe(df, index = "nope"), "not found in data")
})

test_that("variables_when must keep the index", {
  # Dropping it would leave the metadata pointing at a column the frame no
  # longer declares as temporal, which is the desynchronisation #82 closed.
  af <- as_aniframe(data.frame(
    time = 1:4,
    session = c("a", "a", "b", "b"),
    individual = "x",
    x = 1:4,
    y = 1:4
  ))

  expect_error(set_variables_when(af, "session"), "must include the index")
})

test_that("set_metadata() refuses the index and names its setter", {
  af <- aniframe(individual = "a", time = 1:3, x = c(1, 2, 3), y = c(0, 1, 0))

  expect_error(
    set_metadata(af, variables_when_index = "x"),
    "set_index"
  )
})

test_that("metadata serialised before the field existed reads back as time", {
  # `variables_when_index` is optional precisely so that objects written by
  # earlier versions still validate. They were built when a column named
  # `time` was mandatory, so that is what they are indexed by.
  af <- aniframe(individual = "a", time = 1:3, x = c(1, 2, 3), y = c(0, 1, 0))
  md <- get_metadata(af)
  md[["variables_when_index"]] <- NULL

  expect_true(check_all_metadata_fields_present(md))
  expect_equal(resolve_index(md), "time")
})
