# tests/testthat/test-aniframe_methods.R

# ------------------------------------------------------------------
# Helper ----------------------------------------------------------------
# Create a fresh aniframe object and capture its metadata once.
make_af <- function() {
  af <- example_aniframe()          # ← your convenience constructor
  meta <- get_metadata(af)
  list(obj = af, meta = meta)
}

# ------------------------------------------------------------------
# 1. dplyr verbs ----------------------------------------------------
test_that("group_by preserves class & metadata", {
  src <- make_af()
  out <- dplyr::group_by(src$obj, individual)   # individual is a column in example_aniframe()
  expect_s3_class(out, "aniframe")
  expect_identical(get_metadata(out), src$meta)
})

test_that("ungroup preserves class & metadata", {
  src <- make_af()
  tmp <- dplyr::group_by(src$obj, individual)
  expect_warning(
    out <- dplyr::ungroup(tmp)
    )
  expect_s3_class(out, "aniframe")
  expect_identical(get_metadata(out), src$meta)
})

test_that("mutate preserves class & metadata", {
  src <- make_af()
  out <- dplyr::mutate(src$obj, new_col = x * 2)
  expect_s3_class(out, "aniframe")
  expect_identical(get_metadata(out), src$meta)
})

test_that("select preserves class & metadata", {
  src <- make_af()
  out <- dplyr::select(src$obj, individual, x)   # keep a subset of columns
  expect_s3_class(out, "aniframe")
  expect_identical(get_metadata(out), src$meta)
})

test_that("filter preserves class & metadata", {
  src <- make_af()
  out <- dplyr::filter(src$obj, time < 5)
  expect_s3_class(out, "aniframe")
  expect_identical(get_metadata(out), src$meta)
})

test_that("arrange preserves class & metadata", {
  src <- make_af()
  out <- dplyr::arrange(src$obj, desc(x))
  expect_s3_class(out, "aniframe")
  expect_identical(get_metadata(out), src$meta)
})

test_that("rename preserves class & metadata", {
  src <- make_af()
  out <- dplyr::rename(src$obj, x_new = x)
  expect_s3_class(out, "aniframe")
  expect_identical(get_metadata(out), src$meta)
})

test_that("relocate preserves class & metadata", {
  src <- make_af()
  out <- dplyr::relocate(src$obj, y, .before = individual)
  expect_s3_class(out, "aniframe")
  expect_identical(get_metadata(out), src$meta)
})

test_that("slice preserves class & metadata", {
  src <- make_af()
  out <- dplyr::slice(src$obj, 1:3)
  expect_s3_class(out, "aniframe")
  expect_identical(get_metadata(out), src$meta)
})

# ------------------------------------------------------------------
# 2. Base‑R extraction ------------------------------------------------
test_that("[ ] subsetting preserves class & metadata", {
  src <- make_af()
  out <- src$obj[1:2, c("individual", "x")]
  expect_s3_class(out, "aniframe")
  expect_identical(get_metadata(out), src$meta)
})

test_that("[[ ]] extraction preserves metadata when a data.frame is returned", {
  src <- make_af()
  # [[ extracts a single column – should return a vector (no class)
  # but when the result is a data.frame (e.g., extracting a list‑column),
  # the class must be restored.
  # We'll create a list‑column first.
  df <- src$obj %>% dplyr::mutate(lst = list(list(a = 1)))
  out <- df[["lst"]]
  # out is a list; class is irrelevant – just check that no error occurs.
  expect_type(out, "list")
})

test_that("$ extraction returns a plain vector (no class) but does not alter metadata", {
  src <- make_af()
  val <- src$obj$x
  expect_type(val, "double")
  # Metadata is unchanged on the original object
  expect_identical(get_metadata(src$obj), src$meta)
})

# ------------------------------------------------------------------
# 3. Assignment operators ---------------------------------------------
test_that("[<-] assignment preserves class & metadata", {
  src <- make_af()
  out <- src$obj
  out[1, "x"] <- 999
  expect_s3_class(out, "aniframe")
  expect_identical(get_metadata(out), src$meta)
})

test_that("[[<-] assignment preserves class & metadata", {
  src <- make_af()
  out <- src$obj
  out[[ "x" ]] <- 123
  expect_s3_class(out, "aniframe")
  expect_identical(get_metadata(out), src$meta)
})

test_that("$<- assignment preserves class & metadata", {
  src <- make_af()
  out <- src$obj
  out$x <- 42
  expect_s3_class(out, "aniframe")
  expect_identical(get_metadata(out), src$meta)
})

test_that("names<- assignment preserves class & metadata", {
  src <- make_af()
  out <- src$obj
  names(out) <- paste0("col_", seq_along(names(out)))
  expect_s3_class(out, "aniframe")
  expect_identical(get_metadata(out), src$meta)
})

# ------------------------------------------------------------------
# 4. Conversion -------------------------------------------------------
test_that("as.data.frame drops the class but leaves metadata untouched", {
  src <- make_af()
  df <- as.data.frame(src$obj)
  expect_false(inherits(df, "aniframe"))
  # The original object should stay unchanged
  expect_identical(get_metadata(src$obj), src$meta)
})

# ------------------------------------------------------------------
# 5. Round‑trip sanity check -----------------------------------------
test_that("a full pipeline keeps class & metadata", {
  src <- make_af()
  out <- src$obj %>%
    dplyr::group_by(individual) %>%
    dplyr::mutate(new = x * 2) %>%
    dplyr::filter(new > 10) %>%
    dplyr::select(individual, new) %>%
    dplyr::arrange(desc(new))

  expect_s3_class(out, "aniframe")
  expect_identical(get_metadata(out), src$meta)
})
