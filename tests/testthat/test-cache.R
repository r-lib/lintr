context("clear_cache")

test_that("it calls unlink with the filename if given a file", {
  want <- list(file.path(".", "file"), recursive = TRUE)
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),
    `base::unlink` = function(...) return(list(...)),

    expect_equal(clear_cache("file", "."), want),
    expect_equal(clear_cache(file = "file", dir = "."), want),
    expect_equal(suppressWarnings(clear_cache(file = "file", path = ".")), want),  # deprecated
    expect_equal(clear_cache(file = "R/file", dir = "."), want)
  )
})

test_that("it calls unlink with the directory if given a file", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),
    `base::unlink` = function(...) return(list(...)),

    expect_equal(clear_cache(file = NULL, dir = "."), list(".", recursive = TRUE))
  )
})


context("load_cache")

test_that("it loads the saved file in a new empty environment", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),

    f1 <- tempfile(),
    on.exit(unlink(f1)),
    t1 <- "test.R",
    save(t1, file = f1),
    t2 <- load_cache(file = basename(f1), dir = dirname(f1)),

    expect_equal(ls(t2), "t1"),
    expect_equal(t2[["t1"]], "test.R")
  )
})

test_that("it returns an empty environment if no file exists", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),

    t1 <- load_cache(file = tempfile(), dir = "."),

    expect_equal(ls(t1), character(0))
  )
})


context("save_cache")

test_that("it creates a directory if it doesn't exist", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),

    e1 <- new.env(parent = emptyenv()),
    d1 <- tempfile(pattern="lintr_cache_"),
    f1 <- "test.R",

    expect_false(file.exists(d1)),
    expect_false(file.exists(file.path(d1, f1))),

    save_cache(cache = e1, file = f1, dir = d1),

    expect_true(file.exists(d1)),
    expect_true(file.info(d1)$isdir),
    expect_true(file.exists(file.path(d1, f1)))
  )
})

test_that("it saves all non-hidden objects from the environment", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),

    e1 <- new.env(parent = emptyenv()),
    e1$t1 <- 1,
    e1$t2 <- 2,

    d1 <- tempfile(pattern="lintr_cache_"),
    f1 <- "test.R",

    save_cache(cache = e1, file = f1, dir = d1),

    e2 <- new.env(parent = emptyenv()),
    load(file = file.path(d1, f1), envir = e2),

    expect_equal(e1, e2)
  )
})


context("cache_file")

test_that("it generates the same cache with different lints", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("test.R", f1)
  on.exit(unlink(f1))

  cache_file(e1, f1, list(), list())
  cache_file(e1, f1, list(), list(1))

  expect_equal(length(ls(e1)), 1)
})

test_that("it generates different caches for different linters", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("test.R", f1)
  on.exit(unlink(f1))

  cache_file(e1, f1, list(), list())
  cache_file(e1, f1, list(1), list())

  expect_equal(length(ls(e1)), 2)
})


context("retrieve_file")

test_that("it returns NULL if there is no cached result", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("test.R", f1)
  on.exit(unlink(f1))

  expect_equal(retrieve_file(e1, f1, list()), NULL)
})

test_that("it returns the cached result if found", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("test.R", f1)
  on.exit(unlink(f1))

  cache_file(e1, f1, list(), list("test.R"))
  expect_equal(retrieve_file(e1, f1, list()), list("test.R"))
})


context("cache_lint")

test_that("it generates the same cache with different lints", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "test.R")
  cache_lint(e1, t1, list(), list())
  cache_lint(e1, t1, list(), list(1))

  expect_equal(length(ls(e1)), 1)
})

test_that("it generates different caches for different linters", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "test.R")

  cache_lint(e1, t1, list(), list())
  cache_lint(e1, t1, list(1), list())

  expect_equal(length(ls(e1)), 2)
})


context("retrieve_lint")

test_that("it returns the same lints if nothing has changed", {
  e1 <- new.env(parent = emptyenv())

  lines1 <- c("test1.R", "test2.R", "test3.R")

  lints1 <- list(
    Lint("test_file", 1, line = "test1.R"),
    Lint("test_file", 2, line = "test2.R"),
    Lint("test_file", 3, line = "test3.R")
  )

  expr1 <- list(content = paste(collapse = "\n", lines1))
  cache_lint(cache = e1,
             expr = expr1,
             linter = list(),
             lints = lints1)

  t1 <- retrieve_lint(cache = e1,
                      expr = expr1,
                      linter = list(),
                      lines1
                      )

  expect_equal(t1, lints1)
})

test_that("it returns the same lints with fixed line numbers if lines added above", {
  e1 <- new.env(parent = emptyenv())

  lines1 <- c("test1.R", "test2.R", "test3.R")
  lines2 <- c("", "test1.R", "test2.R", "test3.R")

  lints1 <- list(
    Lint("test_file", 1, line = "test1.R"),
    Lint("test_file", 2, line = "test2.R"),
    Lint("test_file", 3, line = "test3.R")
  )

  expr1 <- list(content = paste(collapse = "\n", lines1))
  cache_lint(cache = e1,
             expr = expr1,
             linter = list(),
             lints = lints1)

  t1 <- retrieve_lint(cache = e1,
                      expr = expr1,
                      linter = list(),
                      lines2)

  expect_equal(t1[[1]]$line_number, lints1[[1]]$line_number + 1)
  expect_equal(t1[[2]]$line_number, lints1[[2]]$line_number + 1)
  expect_equal(t1[[3]]$line_number, lints1[[3]]$line_number + 1)
})

test_that("it returns the same lints with if lines added below", {
  e1 <- new.env(parent = emptyenv())

  lines1 <- c("test1.R", "test2.R", "test3.R")
  lines2 <- c("test1.R", "test2.R", "test3.R", "")

  lints1 <- list(
    Lint("test_file", 1, line = "test1.R"),
    Lint("test_file", 2, line = "test2.R"),
    Lint("test_file", 3, line = "test3.R")
  )

  expr1 <- list(content = paste(collapse = "\n", lines1))
  cache_lint(cache = e1,
             expr = expr1,
             linter = list(),
             lints = lints1)

  t1 <- retrieve_lint(cache = e1,
                      expr = expr1,
                      linter = list(),
                      lines2)

  expect_equal(t1, lints1)
})

test_that("it returns the same lints with fixed line numbers if lines added between", {
  e1 <- new.env(parent = emptyenv())

  lines1 <- c("test1.R", "test2.R", "test3.R")
  lines2 <- c("test1.R", "", "test2.R", "test3.R", "")

  lints1 <- list(
    Lint("test_file", 1, line = "test1.R"),
    Lint("test_file", 2, line = "test2.R"),
    Lint("test_file", 3, line = "test3.R")
  )

  expr1 <- list(content = paste(collapse = "\n", lines1))
  cache_lint(cache = e1,
             expr = expr1,
             linter = list(),
             lints = lints1)

  t1 <- retrieve_lint(cache = e1,
                      expr = expr1,
                      linter = list(),
                      lines2)

  expect_equal(t1[[1]]$line_number, lints1[[1]]$line_number)
  expect_equal(t1[[2]]$line_number, lints1[[2]]$line_number + 1)
  expect_equal(t1[[3]]$line_number, lints1[[3]]$line_number + 1)
})


context("has_lint")

test_that("it returns FALSE if there is no cached result", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "test.R")
  expect_false(has_lint(e1, t1, list()))
})

test_that("it returns TRUE if there is a cached result", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "test.R")
  cache_lint(e1, t1, list(), list())
  expect_true(has_lint(e1, t1, list()))
})


context("find_new_line")

test_that("it returns the same if the line is the same", {
  t1 <- c("test1.R",
          "test2.R",
          "test3.R")
  expect_equal(find_new_line(1, "test1.R", t1), 1)

  expect_equal(find_new_line(2, "test2.R", t1), 2)

  expect_equal(find_new_line(3, "test3.R", t1), 3)
})

test_that("it returns the correct line if it is before the current line", {
  t1 <- c("test1.R",
          "test2.R",
          "test3.R")
  expect_equal(find_new_line(1, "test1.R", t1), 1)

  expect_equal(find_new_line(2, "test1.R", t1), 1)

  expect_equal(find_new_line(3, "test1.R", t1), 1)
})

test_that("it returns the correct line if it is after the current line", {
  t1 <- c("test1.R",
          "test2.R",
          "test3.R")
  expect_equal(find_new_line(1, "test3.R", t1), 3)

  expect_equal(find_new_line(2, "test3.R", t1), 3)

  expect_equal(find_new_line(3, "test3.R", t1), 3)
})


test_that("lint() uses the provided cache directory", {
  dir <- "temp_lintr_cache"
  unlink(dir, recursive = TRUE)
  expect_false(dir.exists(dir))
  expect_lint("a <- 1", NULL, assignment_linter, cache=dir) # create the cache
  expect_true(dir.exists(dir))
  expect_lint("a <- 1", NULL, assignment_linter, cache=dir) # read the cache
  expect_true(dir.exists(dir))
  unlink(dir, recursive = TRUE)
})
