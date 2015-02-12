context("clear_cache")
test_that("it calls unlink with the filename if given a file", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),
    `base::unlink` = function(...) return(list(...)),

    expect_equal(clear_cache(file = "file", path = "."), list(file.path(".", "file"))),

    expect_equal(clear_cache(file = "R/file", path = "."), list(file.path(".", "file")))
  )
})
test_that("it calls unlink with the directory if given a file", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),
    `base::unlink` = function(...) return(list(...)),

    expect_equal(clear_cache(file = NULL, path = "."), list(".", recursive = TRUE))
  )
})

context("load_cache")
test_that("it loads the saved file in a new empty environment", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),

    f1 <- tempfile(),
    on.exit(unlink(f1)),
    t1 <- "test",
    save(t1, file = f1),
    t2 <- load_cache(file = basename(f1), path = dirname(f1)),

    expect_equal(ls(t2), "t1"),
    expect_equal(t2[["t1"]], "test")
  )
})

test_that("it returns an empty environment if no file exists", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),

    t1 <- load_cache(file = tempfile(), path = "."),

    expect_equal(ls(t1), character(0))
  )
})

context("save_cache")
test_that("it creates a directory if it doesn't exist", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),

    e1 <- new.env(parent = emptyenv()),
    f1 <- tempfile(),

    expect_false(file.exists(f1)),
    expect_false(file.exists(file.path(f1, "test"))),

    save_cache(e1, "test", f1),

    expect_true(file.exists(f1)),
    expect_true(file.exists(file.path(f1, "test")))
  )
})

test_that("it saves all non-hidden objects from the environment", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),

    e1 <- new.env(parent = emptyenv()),
    e1$t1 <- 1,
    e1$t2 <- 2,

    f1 <- tempfile(),

    save_cache(e1, "test", f1),

    e2 <- new.env(parent = emptyenv()),
    load(file.path(f1, "test"), envir = e2),

    expect_equal(e1, e2)
  )
})

context("cache_file")
test_that("it generates the same cache with different lints", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("test", f1)
  on.exit(unlink(f1))

  cache_file(e1, f1, list(), list())
  cache_file(e1, f1, list(), list(1))

  expect_equal(length(ls(e1)), 1)
})
test_that("it generates different caches for different linters", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("test", f1)
  on.exit(unlink(f1))

  cache_file(e1, f1, list(), list())
  cache_file(e1, f1, list(1), list())

  expect_equal(length(ls(e1)), 2)
})

context("retrieve_file")
test_that("it returns NULL if there is no cached result", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("test", f1)
  on.exit(unlink(f1))

  expect_equal(retrieve_file(e1, f1, list()), NULL)
})

test_that("it returns the cached result if found", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("test", f1)
  on.exit(unlink(f1))

  cache_file(e1, f1, list(), list("test"))
  expect_equal(retrieve_file(e1, f1, list()), list("test"))
})

context("cache_lint")
test_that("it generates the same cache with different lints", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "test")
  cache_lint(e1, t1, list(), list())
  cache_lint(e1, t1, list(), list(1))

  expect_equal(length(ls(e1)), 1)
})
test_that("it generates different caches for different linters", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "test")

  cache_lint(e1, t1, list(), list())
  cache_lint(e1, t1, list(1), list())

  expect_equal(length(ls(e1)), 2)
})

context("retrieve_lint")
test_that("it returns the same lints if nothing has changed", {
  e1 <- new.env(parent = emptyenv())

  lines1 <- c("test1", "test2", "test3")

  lints1 <- list(
    Lint("test_file", 1, line = "test1"),
    Lint("test_file", 2, line = "test2"),
    Lint("test_file", 3, line = "test3")
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

  lines1 <- c("test1", "test2", "test3")
  lines2 <- c("", "test1", "test2", "test3")

  lints1 <- list(
    Lint("test_file", 1, line = "test1"),
    Lint("test_file", 2, line = "test2"),
    Lint("test_file", 3, line = "test3")
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

  lines1 <- c("test1", "test2", "test3")
  lines2 <- c("test1", "test2", "test3", "")

  lints1 <- list(
    Lint("test_file", 1, line = "test1"),
    Lint("test_file", 2, line = "test2"),
    Lint("test_file", 3, line = "test3")
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

  lines1 <- c("test1", "test2", "test3")
  lines2 <- c("test1", "", "test2", "test3", "")

  lints1 <- list(
    Lint("test_file", 1, line = "test1"),
    Lint("test_file", 2, line = "test2"),
    Lint("test_file", 3, line = "test3")
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

  t1 <- list(content = "test")
  expect_false(has_lint(e1, t1, list()))
})
test_that("it returns TRUE if there is a cached result", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "test")
  cache_lint(e1, t1, list(), list())
  expect_true(has_lint(e1, t1, list()))
})

context("find_new_line")
test_that("it returns the same if the line is the same", {
  t1 <- c("test",
          "test2",
          "test3")
  expect_equal(find_new_line(1, "test", t1), 1)

  expect_equal(find_new_line(2, "test2", t1), 2)

  expect_equal(find_new_line(3, "test3", t1), 3)
})

test_that("it returns the correct line if it is before the current line", {
  t1 <- c("test",
          "test2",
          "test3")
  expect_equal(find_new_line(1, "test", t1), 1)

  expect_equal(find_new_line(2, "test", t1), 1)

  expect_equal(find_new_line(3, "test", t1), 1)
})

test_that("it returns the correct line if it is after the current line", {
  t1 <- c("test",
          "test2",
          "test3")
  expect_equal(find_new_line(1, "test3", t1), 3)

  expect_equal(find_new_line(2, "test3", t1), 3)

  expect_equal(find_new_line(3, "test3", t1), 3)
})
