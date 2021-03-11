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
  t1[[a]] <- list(1:10)
  expect_equal(normalize_exclusions(c(t1, NULL)), t1)
  expect_equal(normalize_exclusions(c(NULL, t1)), t1)
})

test_that("it returns the union of two non-overlapping lists", {
  t1 <- list()
  t1[[a]] <- list(1:10)
  t2 <- list()
  t2[[a]] <- list(20:30)
  res <- list()
  res[[a]] <- list(c(1:10, 20:30))
  expect_equal(normalize_exclusions(c(t1, t2)), res)
})

test_that("it works with named lists", {
  t1 <- list()
  t1[[a]] <- list(1:10, my_linter = 1:20)
  t2 <- list()
  t2[[a]] <- list(11:15, my_linter = 21:25)
  res <- list()
  res[[a]] <- list(1:15, my_linter = 1:25)
  expect_equal(normalize_exclusions(c(t1, t2)), res)
})

test_that("it returns the union of two overlapping lists", {
  t1 <- list()
  t1[[a]] <- list(1:10)
  t2 <- list()
  t2[[a]] <- list(5:15)
  res <- list()
  res[[a]] <- list(1:15)
  expect_equal(normalize_exclusions(c(t1, t2)), res)
})

test_that("it adds names if needed", {
  t1 <- list()
  t1[[a]] <- list(1:10)
  t2 <- list()
  t2[[b]] <- list(5:15)
  res <- list()
  res[[a]] <- list(1:10)
  res[[b]] <- list(5:15)
  expect_equal(normalize_exclusions(c(t1, t2)), res)
})

test_that("it handles full file exclusions", {
  res <- list()
  res[[a]] <- list(Inf)
  expect_equal(normalize_exclusions(list(a)), res)

  t1 <- list()
  t1[[1]] <- a
  t1[[b]] <- 1
  res <- list()
  res[[a]] <- list(Inf)
  res[[b]] <- list(1)
  expect_equal(normalize_exclusions(t1), res)
})

test_that("it handles redundant lines", {
  t1 <- list()
  t1[[a]] <- list(c(1, 1, 1:10))
  res <- list()
  res[[a]] <- list(1:10)
  expect_equal(normalize_exclusions(t1), res)

  t1 <- list()
  t1[[a]] <- list(c(1, 1, 1:10))
  t1[[b]] <- list(1:10)
  res <- list()
  res[[a]] <- list(1:10)
  res[[b]] <- list(1:10)
  expect_equal(normalize_exclusions(t1), res)
})

test_that("it handles redundant linters", {
  t1 <- list()
  t1[[a]] <- list(c(1, 1, 1:10), my_linter = c(1, 1, 1, 2), my_linter = 3)
  res <- list()
  res[[a]] <- list(1:10, my_linter = 1:3)
  expect_equal(normalize_exclusions(t1), res)

  t1 <- list()
  t1[[a]] <- list(c(1, 1, 1:10), my_linter = c(1, 1, 1, 2))
  t1[[b]] <- list(1:10, my_linter = 1:10, my_linter = 11:20)
  res <- list()
  res[[a]] <- list(1:10, my_linter = 1:2)
  res[[b]] <- list(1:10, my_linter = 1:20)
  expect_equal(normalize_exclusions(t1), res)
})

test_that("it handles redundant files", {
  t1 <- list(1:10, 10:20)
  names(t1) <- c(a, a)
  res <- list()
  res[[a]] <- list(1:20)
  expect_equal(normalize_exclusions(t1), res)
})

test_that("it normalizes file paths, removing non-existing files", {
  t1 <- list()
  t1[[a]] <- 1:10
  t2 <- list()
  t2[["notafile"]] <- 5:15
  t3 <- list()
  t3[[c]] <- 5:15
  res <- list()
  res[[a]] <- list(1:10)
  res[[normalizePath(c)]] <- list(5:15)
  expect_equal(normalize_exclusions(c(t1, t2, t3)), res)

  res <- list()
  res[[a]] <- list(1:10)
  res[["notafile"]] <- list(5:15)
  res[[c]] <- list(5:15)
  expect_equal(normalize_exclusions(c(t1, t2, t3), normalize_path = FALSE), res)
})

test_that("it errors for invalid specifications", {
  msg_full_files <- "Full file exclusions must be character vectors of length 1."
  expect_error(normalize_exclusions(2), msg_full_files)
  expect_error(normalize_exclusions(list("a.R", 2)), msg_full_files)
  expect_error(normalize_exclusions(list("a.R" = Inf, 2)), msg_full_files)

  msg_full_lines <- "Full line exclusions must be numeric or integer vectors."
  expect_error(normalize_exclusions(list("a.R" = "Inf")), msg_full_lines)
})

unlink(c(a, b, c))
options(old_ops)
