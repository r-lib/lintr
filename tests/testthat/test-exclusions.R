old_ops <- options(lintr.exclude = "#TeSt_NoLiNt",
                   lintr.exclude_start = "#TeSt_NoLiNt_StArT",
                   lintr.exclude_end = "#TeSt_NoLiNt_EnD")

context("parse_exclusions")
test_that("it returns an empty vector if there are no exclusions", {
  read_settings(NULL)
  t1 <- tempfile()
  on.exit(unlink(t1))
  writeLines(
    c("this",
      "is",
      "a",
      "test"), t1)
  expect_equal(parse_exclusions(t1), numeric(0))
})

test_that("it returns the line if one line is excluded", {
  read_settings(NULL)

  t1 <- tempfile()
  on.exit(unlink(t1))
  writeLines(
    c("this",
      "is #TeSt_NoLiNt",
      "a",
      "test"), t1)
  expect_equal(parse_exclusions(t1), c(2))

  t2 <- tempfile()
  on.exit(unlink(t2))
  writeLines(
    c("this",
      "is #TeSt_NoLiNt",
      "a",
      "test #TeSt_NoLiNt"), t2)
  expect_equal(parse_exclusions(t2), c(2, 4))
})

test_that("it returns all lines between start and end", {
  read_settings(NULL)

  t1 <- tempfile()
  on.exit(unlink(t1))
  writeLines(
    c("this #TeSt_NoLiNt_StArT",
      "is",
      "a #TeSt_NoLiNt_EnD",
      "test"), t1)
  expect_equal(parse_exclusions(t1), c(1, 2, 3))

  t2 <- tempfile()
  on.exit(unlink(t2))
  writeLines(
    c("this #TeSt_NoLiNt_StArT",
      "is",
      "a #TeSt_NoLiNt_EnD",
      "test",
      "of",
      "the #TeSt_NoLiNt_StArT",
      "emergency #TeSt_NoLiNt_EnD",
      "broadcast",
      "system"
    ), t2)
  expect_equal(parse_exclusions(t2), c(1, 2, 3, 6, 7))
})

test_that("it ignores exclude coverage lines within start and end", {
  read_settings(NULL)

    t1 <- tempfile()
  on.exit(unlink(t1))
  writeLines(
    c("this #TeSt_NoLiNt_StArT",
      "is #TeSt_NoLiNt",
      "a #TeSt_NoLiNt_EnD",
      "test"), t1)
  expect_equal(parse_exclusions(t1), c(1, 2, 3))
})

test_that("it throws an error if start and end are unpaired", {
  read_settings(NULL)

  t1 <- tempfile()
  on.exit(unlink(t1))
  writeLines(
    c("this #TeSt_NoLiNt_StArT",
      "is #TeSt_NoLiNt",
      "a",
      "test"), t1)
  expect_error(parse_exclusions(t1), "but only")
})

context("normalize_exclusions")
test_that("it merges two NULL or empty objects as an empty list", {
  expect_equal(normalize_exclusions(c(NULL, NULL)), list())
  expect_equal(normalize_exclusions(c(NULL, list())), list())
  expect_equal(normalize_exclusions(c(list(), NULL)), list())
  expect_equal(normalize_exclusions(c(list(), list())), list())
})

test_that("it returns the object if the other is NULL", {
  t1 <- list(a = 1:10)

  expect_equal(normalize_exclusions(c(t1, NULL)), t1)
  expect_equal(normalize_exclusions(c(NULL, t1)), t1)
})

test_that("it returns the union of two non-overlapping lists", {
  t1 <- list(a = 1:10)
  t2 <- list(a = 20:30)

  expect_equal(normalize_exclusions(c(t1, t2)), list(a = c(1:10, 20:30)))
})

test_that("it returns the union of two overlapping lists", {
  t1 <- list(a = 1:10)
  t2 <- list(a = 5:15)

  expect_equal(normalize_exclusions(c(t1, t2)), list(a = 1:15))
})

test_that("it adds names if needed", {
  t1 <- list(a = 1:10)
  t2 <- list(b = 5:15)

  expect_equal(normalize_exclusions(c(t1, t2)), list(a = 1:10, b = 5:15))
})

test_that("it handles full file exclusions", {

  expect_equal(normalize_exclusions(list("a")), list(a = Inf))

  expect_equal(normalize_exclusions(list("a", b = 1)), list(a = Inf, b = 1))
})

test_that("it handles redundant lines", {

  expect_equal(normalize_exclusions(list(a = c(1, 1, 1:10))), list(a = 1:10))

  expect_equal(normalize_exclusions(list(a = c(1, 1, 1:10), b = 1:10)), list(a = 1:10, b = 1:10))
})

test_that("it handles redundant files", {

  expect_equal(normalize_exclusions(list(a = c(1:10), a = c(10:20))), list(a = 1:20))
})

context("exclude")
test_that("it excludes properly", {
  read_settings(NULL)

  t1 <- lint("exclusions-test")

  expect_equal(length(t1), 2)

  t2 <- lint("exclusions-test", exclusions = list("exclusions-test" = 4))

  expect_equal(length(t2), 1)

  t3 <- lint("exclusions-test", exclusions = list("exclusions-test"))

  expect_equal(length(t3), 0)

  cache_dir <- file.path(tempdir(), "lintr_cache")
  clear_cache("exclusions-test", cache_dir)
  for (info in sprintf("caching: pass %s", 1:4)) {
    t4 <- lint("exclusions-test", cache = cache_dir)

    expect_equal(length(t4), 2, info = info)
  }
})

options(old_ops)
