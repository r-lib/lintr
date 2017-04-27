mock_package <- function(files = character(), name = character()) {
  dir <- tempfile(pattern = "mock_package_")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  for (sub_dir in file.path(dir, dirname(files))) {
    dir.create(sub_dir, showWarnings = FALSE, recursive = TRUE)
  }
  files <- file.path(dir, files)
  file.create(files)
  list(dir = dir, files = files, name = name )
}


context("clear_cache")

test_that("it deletes the file if a file is given", {
  pkg <- mock_package(files = "R/test.R", name = "my_pkg")
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),
    `lintr::find_package` = function(...) pkg$dir,
    `lintr::pkg_name` = function(...) pkg$name,
    `base::unlink` = function(...) list(...),

    e1 <- new.env(parent = emptyenv()),
    d1 <- tempfile(pattern = "lintr_cache_"),
    save_cache(cache = e1, file = pkg$file[[1]], dir = d1),

    want <- list(file.path(d1, "my_pkg", "R/test.R"), recursive = TRUE),
    expect_equal(clear_cache(pkg$file[[1]], d1), want),
    expect_equal(clear_cache(file = pkg$file[[1]], dir = d1), want),
    expect_equal(suppressWarnings(clear_cache(file = pkg$file[[1]], path = d1)), want)  # deprecated
  )
})

test_that("it deletes the directory if no file is given", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),
    `base::unlink` = function(...) list(...),

    expect_equal(clear_cache(file = NULL, dir = "."), list(".", recursive = TRUE))
  )
})


context("load_cache")

test_that("it loads the saved file in a new empty environment", {
  pkg <- mock_package(files = "R/test.R", name = "my_pkg")
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),
    `lintr::find_package` = function(...) pkg$dir,
    `lintr::pkg_name` = function(...) pkg$name,

    e1 <- new.env(parent = emptyenv()),
    assign("x", "foobar", envir = e1),
    d1 <- tempfile(pattern = "lintr_cache_"),
    save_cache(cache = e1, file = pkg$file[[1]], dir = d1),
    e2 <- load_cache(file = pkg$file[[1]], dir = d1),

    expect_equal(ls(e2), "x"),
    expect_equal(e2[["x"]], "foobar")
  )
})

test_that("it returns an empty environment if no cache file exists", {
  pkg <- mock_package(files = c("R/test.R", "test.R"), name = "my_pkg")
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),
    `lintr::find_package` = function(...) pkg$dir,
    `lintr::pkg_name` = function(...) pkg$name,

    e1 <- new.env(parent = emptyenv()),
    d1 <- tempfile(pattern = "lintr_cache_"),
    save_cache(cache = e1, file = pkg$file[[1]], dir = d1),
    e2 <- load_cache(file = pkg$file[[2]], dir = d1),

    expect_equal(ls(e2), character(0))
  )
})


context("save_cache")

test_that("it creates a directory if needed", {
  pkg <- mock_package(files = "R/test.R", name = "my_pkg")
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),
    `lintr::find_package` = function(...) pkg$dir,
    `lintr::pkg_name` = function(...) pkg$name,

    e1 <- new.env(parent = emptyenv()),
    d1 <- tempfile(pattern = "lintr_cache_"),

    expect_false(file.exists(d1)),
    expect_false(file.exists(file.path(d1, "my_pkg"))),
    expect_false(file.exists(file.path(d1, "my_pkg", "R/test.R"))),

    save_cache(cache = e1, file = pkg$files[[1]], dir = d1),

    expect_true(file.exists(d1)),
    expect_true(file.info(d1)$isdir),
    expect_true(file.exists(file.path(d1, "my_pkg"))),
    expect_true(file.info(file.path(d1, "my_pkg"))$isdir),
    expect_true(file.exists(file.path(d1, "my_pkg", "R/test.R")))
  )
})

test_that("it uses unambiguous cache file names", {
  pkg <- mock_package(files = c("R/test.R", "test.R"), name = "my_pkg")
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),
    `lintr::find_package` = function(...) pkg$dir,
    `lintr::pkg_name` = function(...) pkg$name,

    e1 <- new.env(parent = emptyenv()),
    d1 <- tempfile(pattern = "lintr_cache_"),

    expect_false(file.exists(file.path(d1, "my_pkg", "R/test.R"))),
    expect_false(file.exists(file.path(d1, "my_pkg", "test.R"))),

    save_cache(cache = e1, file = pkg$file[[1]], dir = d1),
    save_cache(cache = e1, file = pkg$file[[2]], dir = d1),

    expect_true(file.exists(file.path(d1, "my_pkg", "R/test.R"))),
    expect_true(file.exists(file.path(d1, "my_pkg", "test.R")))
  )
})

test_that("it saves all non-hidden objects from the environment", {
  pkg <- mock_package(files = "R/test.R", name = "my_pkg")
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),
    `lintr::find_package` = function(...) pkg$dir,
    `lintr::pkg_name` = function(...) pkg$name,

    e1 <- new.env(parent = emptyenv()),
    e1$t1 <- 1,
    e1$t2 <- 2,

    d1 <- tempfile(pattern = "lintr_cache_"),

    save_cache(cache = e1, file = pkg$files[[1]], dir = d1),

    e2 <- new.env(parent = emptyenv()),
    load(file = file.path(d1, "my_pkg", "R/test.R"), envir = e2),

    expect_equal(e1, e2)
  )
})


context("cache_file")

test_that("it generates the same cache with different lints", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("foobar", f1)
  on.exit(unlink(f1))

  cache_file(e1, f1, list(), list())
  cache_file(e1, f1, list(), list(1))

  expect_equal(length(ls(e1)), 1)
})

test_that("it generates different caches for different linters", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("foobar", f1)
  on.exit(unlink(f1))

  cache_file(e1, f1, list(), list())
  cache_file(e1, f1, list(1), list())

  expect_equal(length(ls(e1)), 2)
})


context("retrieve_file")

test_that("it returns NULL if there is no cached result", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("foobar", f1)
  on.exit(unlink(f1))

  expect_equal(retrieve_file(e1, f1, list()), NULL)
})

test_that("it returns the cached result if found", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("foobar", f1)
  on.exit(unlink(f1))

  cache_file(e1, f1, list(), list("foobar"))
  expect_equal(retrieve_file(e1, f1, list()), list("foobar"))
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

  lines1 <- c("foobar1", "foobar2", "foobar3")

  lints1 <- list(
    Lint("R/test.R", 1, line = "foobar1"),
    Lint("R/test.R", 2, line = "foobar2"),
    Lint("R/test.R", 3, line = "foobar3")
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

  lines1 <- c("foobar1", "foobar2", "foobar3")
  lines2 <- c("", "foobar1", "foobar2", "foobar3")

  lints1 <- list(
    Lint("R/test.R", 1, line = "foobar1"),
    Lint("R/test.R", 2, line = "foobar2"),
    Lint("R/test.R", 3, line = "foobar3")
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

test_that("it returns the same lints with lines added below", {
  e1 <- new.env(parent = emptyenv())

  lines1 <- c("foobar1", "foobar2", "foobar3")
  lines2 <- c("foobar1", "foobar2", "foobar3", "")

  lints1 <- list(
    Lint("R/test.R", 1, line = "foobar1"),
    Lint("R/test.R", 2, line = "foobar2"),
    Lint("R/test.R", 3, line = "foobar3")
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

  lines1 <- c("foobar1", "foobar2", "foobar3")
  lines2 <- c("foobar1", "", "foobar2", "foobar3", "")

  lints1 <- list(
    Lint("R/test.R", 1, line = "foobar1"),
    Lint("R/test.R", 2, line = "foobar2"),
    Lint("R/test.R", 3, line = "foobar3")
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

  t1 <- list(content = "foobar")
  expect_false(has_lint(e1, t1, list()))
})

test_that("it returns TRUE if there is a cached result", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "foobar")
  cache_lint(e1, t1, list(), list())
  expect_true(has_lint(e1, t1, list()))
})

test_that("it distinguishes global expressions from line expression with same content", {
  e1 <- new.env(parent = emptyenv())

  same_content <- "foobar"

  line_expr   <- list(content = same_content, parsed_content = data.frame())
  cache_lint(e1, line_expr, list(), list())

  global_expr <- list(content = same_content, file_lines = character())
  expect_false(has_lint(e1, global_expr, list()))
})

context("find_new_line")

test_that("it returns the same if the line is the same", {
  t1 <- c("foobar1",
          "foobar2",
          "foobar3")
  expect_equal(find_new_line(1, "foobar1", t1), 1)

  expect_equal(find_new_line(2, "foobar2", t1), 2)

  expect_equal(find_new_line(3, "foobar3", t1), 3)
})

test_that("it returns the correct line if it is before the current line", {
  t1 <- c("foobar1",
          "foobar2",
          "foobar3")
  expect_equal(find_new_line(1, "foobar1", t1), 1)

  expect_equal(find_new_line(2, "foobar1", t1), 1)

  expect_equal(find_new_line(3, "foobar1", t1), 1)
})

test_that("it returns the correct line if it is after the current line", {
  t1 <- c("foobar1",
          "foobar2",
          "foobar3")
  expect_equal(find_new_line(1, "foobar3", t1), 3)

  expect_equal(find_new_line(2, "foobar3", t1), 3)

  expect_equal(find_new_line(3, "foobar3", t1), 3)
})


context("lint with cache")

test_that("it uses the provided cache directory", {
  pkg <- mock_package(files = "R/test.R", name = "my_pkg")
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),
    `lintr::find_package` = function(...) pkg$dir,
    `lintr::pkg_name` = function(...) pkg$name,

    dir <- file.path(tempfile(pattern = "my_cache_dir_")),
    expect_false(dir.exists(file.path(dir, "my_pkg"))),
    # create the cache
    expect_lint("a <- 1", NULL, assignment_linter, cache = dir),
    expect_true(dir.exists(file.path(dir, "my_pkg"))),
    expect_length(list.files(file.path(dir, "my_pkg"), "^file.*"), 1L),
    # read the cache
    expect_lint("a <- 1", NULL, assignment_linter, cache = dir),
    expect_true(dir.exists(file.path(dir, "my_pkg")))
  )
})

test_that("it works outside of a package", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),
    `lintr::find_package` = function(...) NULL,
    `lintr::pkg_name` = function(...) NULL,

    dir <- tempfile(pattern = "my_cache_dir_"),
    expect_false(dir.exists(dir)),
    expect_lint("a <- 1", NULL, assignment_linter, cache = dir),
    expect_true(dir.exists(dir)),
    expect_length(list.files(dir, ".*"), 1L),
    expect_true(file.info(list.files(dir, ".*", full.names = TRUE)[[1L]])$isdir),
    expect_lint("a <- 1", NULL, assignment_linter, cache = dir),
    expect_true(dir.exists(dir))
  )
})
