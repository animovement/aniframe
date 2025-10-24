# ------------------------------------------------------------
# Helper: create a minimal aniframe object for the tests
# ------------------------------------------------------------
make_test_aniframe <- function(df, unit = "rad") {
  # Turn a plain data.frame into an aniframe and attach unit_angle metadata
  df %>%
    as_aniframe() %>%               # from your package
    set_metadata(unit_angle = unit) # store the current angular unit
}

# ------------------------------------------------------------
# Sample data – three numeric angle columns
# ------------------------------------------------------------
raw_df <- data.frame(
  time = c(0,1,2),
  x = c(1,1,1),
  y = c(2,2,2),
  head_left  = c(0, pi / 2, pi),
  head_right = c(pi / 4, pi / 3, pi / 6),
  speed      = c(1, 2, 3)           # non‑angle column, should stay untouched
)

# ------------------------------------------------------------
# Begin the test suite
# ------------------------------------------------------------
test_that("set_unit_angle converts rad → deg correctly", {
  anif <- make_test_aniframe(raw_df, unit = "rad")

  out <- set_unit_angle(anif,
                        cols = c("head_left", "head_right"),
                        to_unit = "deg")

  # Metadata should now report degrees
  expect_equal(get_metadata(out, "unit_angle") |> as.character(), "deg")

  # Angle columns are converted, other columns unchanged
  expect_equal(out$head_left,
               rad_to_deg(raw_df$head_left))
  expect_equal(out$head_right,
               rad_to_deg(raw_df$head_right))
  expect_equal(out$speed, raw_df$speed)
})

test_that("set_unit_angle converts deg → rad correctly", {
  # Start from a degree‑based aniframe
  deg_df <- data.frame(
    time = c(0,1,2),
    x = c(1,1,1),
    y = c(2,2,2),
    head_left  = rad_to_deg(raw_df$head_left),
    head_right = rad_to_deg(raw_df$head_right),
    speed      = raw_df$speed
  )
  anif <- make_test_aniframe(deg_df, unit = "deg")

  out <- set_unit_angle(anif,
                        cols = c("head_left", "head_right"),
                        to_unit = "rad")

  expect_equal(get_metadata(out, "unit_angle") |> as.character(), "rad")
  expect_equal(out$head_left,
               deg_to_rad(deg_df$head_left))
  expect_equal(out$head_right,
               deg_to_rad(deg_df$head_right))
  expect_equal(out$speed, deg_df$speed)
})

test_that("no conversion occurs when target unit already set", {
  anif <- make_test_aniframe(raw_df, unit = "rad")

  # Capture the informational message
  expect_message(
    out <- set_unit_angle(anif,
                          cols = c("head_left", "head_right"),
                          to_unit = "rad"),
    "Angular unit is already rad"
  )

  # Object should be identical (aside from possible class attributes)
  expect_identical(out, as_aniframe(raw_df) %>% set_metadata(unit_angle = "rad"))
})

test_that("invalid target unit triggers an error", {
  anif <- make_test_aniframe(raw_df, unit = "rad")

  expect_error(
    set_unit_angle(anif,
                   cols = c("head_left"),
                   to_unit = "turns"),
    "Angular unit can only be"
  )
})

test_that("missing columns raise an informative error", {
  anif <- make_test_aniframe(raw_df, unit = "rad")

  expect_error(
    set_unit_angle(anif,
                   cols = c("nonexistent"),
                   to_unit = "deg"),
    "All provided columns must be in the data."
  )
})

test_that("non‑numeric columns raise an informative error", {
  # Introduce a character column deliberately
  bad_df <- raw_df
  bad_df$head_left <- as.character(bad_df$head_left)

  anif <- make_test_aniframe(bad_df, unit = "rad")

  expect_error(
    set_unit_angle(anif,
                   cols = c("head_left"),
                   to_unit = "deg"),
    "All provided columns must be numeric."
  )
})
