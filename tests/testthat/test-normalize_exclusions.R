old_ops <- options(lintr.exclude = "#TeSt_NoLiNt",
                   lintr.exclude_start = "#TeSt_NoLiNt_StArT",
                   lintr.exclude_end = "#TeSt_NoLiNt_EnD")

a <- tempfile()
b <- tempfile()
c <- tempfile(tmpdir = ".")
file.create(a, b, c)
a <- normalizePath(a)
b <- normalizePath(b)
c <- normalizePath(c)

test_that("it merges two NULL or empty objects as an empty list", {
  expect_equal(normalize_exclusions(c(NULL, NULL)), list())
  expect_equal(normalize_exclusions(c(NULL, list())), list())
  expect_equal(normalize_exclusions(c(list(), NULL)), list())
  expect_equal(normalize_exclusions(c(list(), list())), list())
})

test_that("it returns the object if the other is NULL", {
  t1 <- list()
  t1[[a]] <- list(1L:10L)
  expect_equal(normalize_exclusions(c(t1, NULL)), t1)
  expect_equal(normalize_exclusions(c(NULL, t1)), t1)
})

test_that("it returns the union of two non-overlapping lists", {
  t1 <- list()
  t1[[a]] <- list(1L:10L)
  t2 <- list()
  t2[[a]] <- list(20L:30L)
  res <- list()
  res[[a]] <- list(c(1L:10L, 20L:30L))
  expect_equal(normalize_exclusions(c(t1, t2)), res)
})

test_that("it works with named lists", {
  t1 <- list()
  t1[[a]] <- list(1L:10L, my_linter = 1L:20L)
  t2 <- list()
  t2[[a]] <- list(11L:15L, my_linter = 21L:25L)
  res <- list()
  res[[a]] <- list(1L:15L, my_linter = 1L:25L)
  expect_equal(normalize_exclusions(c(t1, t2)), res)
})

test_that("it returns the union of two overlapping lists", {
  t1 <- list()
  t1[[a]] <- list(1L:10L)
  t2 <- list()
  t2[[a]] <- list(5L:15L)
  res <- list()
  res[[a]] <- list(1L:15L)
  expect_equal(normalize_exclusions(c(t1, t2)), res)
})

test_that("it adds names if needed", {
  t1 <- list()
  t1[[a]] <- list(1L:10L)
  t2 <- list()
  t2[[b]] <- list(5L:15L)
  res <- list()
  res[[a]] <- list(1L:10L)
  res[[b]] <- list(5L:15L)
  expect_equal(normalize_exclusions(c(t1, t2)), res)
})

test_that("it handles full file exclusions", {
  res <- list()
  res[[a]] <- list(Inf)
  expect_equal(normalize_exclusions(list(a)), res)

  t1 <- list()
  t1[[1L]] <- a
  t1[[b]] <- 1L
  res <- list()
  res[[a]] <- list(Inf)
  res[[b]] <- list(1L)
  expect_equal(normalize_exclusions(t1), res)
})

test_that("it handles redundant lines", {
  t1 <- list()
  t1[[a]] <- list(c(1L, 1L, 1L:10L))
  res <- list()
  res[[a]] <- list(1L:10L)
  expect_equal(normalize_exclusions(t1), res)

  t1 <- list()
  t1[[a]] <- list(c(1L, 1L, 1L:10L))
  t1[[b]] <- list(1L:10L)
  res <- list()
  res[[a]] <- list(1L:10L)
  res[[b]] <- list(1L:10L)
  expect_equal(normalize_exclusions(t1), res)
})

test_that("it handles redundant linters", {
  t1 <- list()
  t1[[a]] <- list(c(1L, 1L, 1L:10L), my_linter = c(1L, 1L, 1L, 2L), my_linter = 3L)
  res <- list()
  res[[a]] <- list(1L:10L, my_linter = 1L:3L)
  expect_equal(normalize_exclusions(t1), res)

  t1 <- list()
  t1[[a]] <- list(c(1L, 1L, 1L:10L), my_linter = c(1L, 1L, 1L, 2L))
  t1[[b]] <- list(1L:10L, my_linter = 1L:10L, my_linter = 11L:20L)
  res <- list()
  res[[a]] <- list(1L:10L, my_linter = 1L:2L)
  res[[b]] <- list(1L:10L, my_linter = 1L:20L)
  expect_equal(normalize_exclusions(t1), res)
})

test_that("it handles redundant files", {
  t1 <- list(1L:10L, 10L:20L)
  names(t1) <- c(a, a)
  res <- list()
  res[[a]] <- list(1L:20L)
  expect_equal(normalize_exclusions(t1), res)
})

test_that("it normalizes file paths, removing non-existing files", {
  t1 <- list()
  t1[[a]] <- 1L:10L
  t2 <- list()
  t2[["notafile"]] <- 5L:15L
  t3 <- list()
  t3[[c]] <- 5L:15L
  res <- list()
  res[[a]] <- list(1L:10L)
  res[[normalizePath(c)]] <- list(5L:15L)
  expect_equal(normalize_exclusions(c(t1, t2, t3)), res)

  res <- list()
  res[[a]] <- list(1L:10L)
  res[["notafile"]] <- list(5L:15L)
  res[[c]] <- list(5L:15L)
  expect_equal(normalize_exclusions(c(t1, t2, t3), normalize_path = FALSE), res)
})

test_that("it errors for invalid specifications", {
  msg_full_files <- "Full file exclusions must be character vectors of length 1."
  expect_error(normalize_exclusions(2L), msg_full_files)
  expect_error(normalize_exclusions(list("a.R", 2L)), msg_full_files)
  expect_error(normalize_exclusions(list("a.R" = Inf, 2L)), msg_full_files)

  msg_full_lines <- "Full line exclusions must be numeric or integer vectors."
  expect_error(normalize_exclusions(list("a.R" = "Inf")), msg_full_lines)
})

unlink(c(a, b, c))
options(old_ops)
