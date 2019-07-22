context("recode")

test_that("basic recoding works", {
  cyl <- c(4, 4, 6, 8)
  expect_identical(vec_recode(cyl, keys(0:2, c(4L, 6L, 8L))), int(0, 0, 1, 2))
  expect_identical(vec_recode(cyl, keys(0:1, c(4L, 6L))), int(0, 0, 1, 8))
  expect_identical(vec_recode(cyl, keys(0:1, c(4L, 6L)), default = 1.5), dbl(0, 0, 1, 1.5))
})

test_that("can map to other type", {
  expect_identical(vec_recode(c(0, 1, 0), keys(c("zero", "one"), 0:1)), c("zero", "one", "zero"))
})

test_that("can supply missing values as part of spec", {
  x <- c("foo", "bar", NA, "foo")
  spec <- keys(c("FOO", "missing"), c("foo", NA))
  expect_identical(vec_recode(x, spec, default = "default"), c("FOO", "default", "missing", "FOO"))
})

test_that("can recode multiple values to multiple keys", {
  spec <- tibble::tribble(
    ~ .new, ~ .old,
      0L,      c(4, 6),
      1L,      8
  )
  expect_identical(vec_recode(c(4, 4, 6, 8), spec), int(0, 0, 0, 1))
})
