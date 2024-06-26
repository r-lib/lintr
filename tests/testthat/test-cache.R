# Fixtures

fixtures <- list()

fixtures$retrieve_lint <- function() {
  file_name <- "R/test.R"
  lines <- c("foobar1", "foobar2", "foobar3")
  lints <- list(
    Lint(file_name, 1L, line = "foobar1"),
    Lint(file_name, 2L, line = "foobar2"),
    Lint(file_name, 3L, line = "foobar3")
  )
  expr <- list(content = paste(collapse = "\n", lines))
  list(
    lines = lines,
    linters = list(),
    lints = lints,
    expr = expr
  )
}

# Run tests with a temporary cache directory, so we don't leave files behind
# after running
withr::local_options(lintr.cache_directory = withr::local_tempdir())

# Helper functions

fhash <- function(filename) {
  digest::digest(filename, algo = "sha1")
}

# Tests

# `clear_cache`

test_that("clear_cache deletes the file if a file is given", {
  local_mocked_bindings(
    read_settings = function(...) invisible(...),
    unlink = function(...) list(...)
  )

  e1 <- new.env(parent = emptyenv())
  d1 <- withr::local_tempfile(pattern = "lintr_cache_")
  f1 <- "R/test.R"
  lintr:::save_cache(cache = e1, file = f1, path = d1)

  want <- list(file.path(d1, fhash("R/test.R")), recursive = TRUE)
  expect_identical(clear_cache(f1, d1), want)
  expect_identical(clear_cache(file = f1, path = d1), want)
})

test_that("clear_cache deletes the directory if no file is given", {
  local_mocked_bindings(
    read_settings = function(...) invisible(...),
    unlink = function(...) list(...)
  )

  expect_identical(clear_cache(file = NULL, path = "."), list(".", recursive = TRUE))
})

# `load_cache`

test_that("load_cache loads the saved file in a new empty environment", {
  e1 <- new.env(parent = emptyenv())
  e1[["x"]] <- "foobar"
  d1 <- withr::local_tempfile(pattern = "lintr_cache_")
  f1 <- "R/test.R"
  lintr:::save_cache(cache = e1, file = f1, path = d1)
  e2 <- lintr:::load_cache(file = f1, path = d1)

  expect_identical(e2, e1)
})

test_that("load_cache returns an empty environment if no cache file exists", {
  e1 <- new.env(parent = emptyenv())
  d1 <- withr::local_tempfile(pattern = "lintr_cache_")
  f1 <- "R/test.R"
  f2 <- "test.R"

  lintr:::save_cache(cache = e1, file = f1, path = d1)
  e2 <- lintr:::load_cache(file = f2, path = d1)

  expect_identical(e2, e1)
})

test_that("load_cache returns an empty environment if reading cache file fails", {
  e1 <- new.env(parent = emptyenv())
  e1[["x"]] <- "foobar"
  d1 <- withr::local_tempfile(pattern = "lintr_cache_")
  f1 <- "R/test.R"
  lintr:::save_cache(cache = e1, file = f1, path = d1)
  cache_f1 <- file.path(d1, fhash(f1))
  writeLines(character(), cache_f1)

  expect_warning(
    {
      e2 <- lintr:::load_cache(file = f1, path = d1)
    },
    "Could not load cache file"
  )
  saveRDS(e1, cache_f1)
  expect_warning(
    {
      e3 <- lintr:::load_cache(file = f1, path = d1)
    },
    "Could not load cache file"
  )
  expect_identical(ls(e2), character())
  expect_identical(ls(e3), character())
})

# `save_cache`

test_that("save_cache creates a directory if needed", {
  e1 <- new.env(parent = emptyenv())
  d1 <- withr::local_tempfile(pattern = "lintr_cache_")
  f1 <- "R/test.R"

  expect_false(file.exists(d1))
  expect_false(file.exists(file.path(d1, fhash(f1))))

  lintr:::save_cache(cache = e1, file = f1, path = d1)

  expect_true(file.exists(d1))
  expect_true(file.info(d1)$isdir)
  expect_true(file.exists(file.path(d1, fhash(f1))))
})

test_that("save_cache uses unambiguous cache file names", {
  e1 <- new.env(parent = emptyenv())
  d1 <- withr::local_tempfile(pattern = "lintr_cache_")
  f1 <- "R/test.R"
  f2 <- "test.R"

  expect_false(file.exists(file.path(d1, fhash(f1))))
  expect_false(file.exists(file.path(d1, fhash(f2))))

  lintr:::save_cache(cache = e1, file = f1, path = d1)
  lintr:::save_cache(cache = e1, file = f2, path = d1)

  expect_true(fhash(f1) != fhash(f2))
  expect_true(file.exists(file.path(d1, fhash(f1))))
  expect_true(file.exists(file.path(d1, fhash(f2))))
})

test_that("save_cache saves all non-hidden objects from the environment", {
  e1 <- new.env(parent = emptyenv())
  e1$t1 <- 1L
  e1$t2 <- 2L

  d1 <- withr::local_tempfile(pattern = "lintr_cache_")
  f1 <- "R/test.R"

  lintr:::save_cache(cache = e1, file = f1, path = d1)

  e2 <- new.env(parent = emptyenv())
  load(file = file.path(d1, fhash(f1)), envir = e2)

  expect_identical(e2, e1)
})

# `cache_file`

test_that("cache_file generates the same cache with different lints", {
  e1 <- new.env(parent = emptyenv())

  f1 <- withr::local_tempfile(lines = "foobar")

  lintr:::cache_file(e1, f1, list(), list())
  lintr:::cache_file(e1, f1, list(), list(1L))

  expect_length(ls(e1), 1L)
})

test_that("cache_file generates different caches for different linters", {
  e1 <- new.env(parent = emptyenv())

  f1 <- withr::local_tempfile(lines = "foobar")

  lintr:::cache_file(e1, f1, list(), list())
  lintr:::cache_file(e1, f1, list(1L), list())

  expect_length(ls(e1), 2L)
})

# `retrieve_file`

test_that("retrieve_file returns NULL if there is no cached result", {
  e1 <- new.env(parent = emptyenv())

  f1 <- withr::local_tempfile(lines = "foobar")

  expect_null(lintr:::retrieve_file(e1, f1, list()))
})

test_that("retrieve_file returns the cached result if found", {
  e1 <- new.env(parent = emptyenv())

  f1 <- withr::local_tempfile(lines = "foobar")

  lintr:::cache_file(e1, f1, list(), list("foobar"))
  expect_identical(lintr:::retrieve_file(e1, f1, list()), list("foobar"))
})

# `cache_lint`

test_that("cache_lint generates the same cache with different lints", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "test")
  lintr:::cache_lint(e1, t1, list(), list())
  lintr:::cache_lint(e1, t1, list(), list(1L))

  expect_length(ls(e1), 1L)
})

test_that("cache_lint generates different caches for different linters", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "test")

  lintr:::cache_lint(e1, t1, list(), list())
  lintr:::cache_lint(e1, t1, list(1L), list())

  expect_length(ls(e1), 2L)
})

# `retrieve_lint`

test_that("retrieve_lint returns the same lints if nothing has changed", {
  test_data <- fixtures$retrieve_lint()

  e1 <- new.env(parent = emptyenv())

  lintr:::cache_lint(
    cache = e1,
    expr = test_data[["expr"]],
    linter = test_data[["linters"]],
    lints = test_data[["lints"]]
  )

  t1 <- lintr:::retrieve_lint(
    cache = e1,
    expr = test_data[["expr"]],
    linter = test_data[["linters"]],
    lines = test_data[["lines"]]
  )

  expect_identical(t1, test_data[["lints"]])
})

test_that(
  "retrieve_lint returns the same lints with fixed line numbers if lines added above",
  {
    test_data <- fixtures$retrieve_lint()

    e1 <- new.env(parent = emptyenv())

    lines1 <- test_data[["lines"]]
    lines2 <- c("", lines1)
    lints <- test_data[["lints"]]

    lintr:::cache_lint(
      cache = e1,
      expr = test_data[["expr"]],
      linter = test_data[["linters"]],
      lints = lints
    )

    t1 <- lintr:::retrieve_lint(
      cache = e1,
      expr = test_data[["expr"]],
      linter = test_data[["linters"]],
      lines = lines2
    )

    expect_identical(t1[[1L]]$line_number, lints[[1L]]$line_number + 1L)
    expect_identical(t1[[2L]]$line_number, lints[[2L]]$line_number + 1L)
    expect_identical(t1[[3L]]$line_number, lints[[3L]]$line_number + 1L)
  }
)

test_that("retrieve_lint returns the same lints with lines added below", {
  test_data <- fixtures$retrieve_lint()

  e1 <- new.env(parent = emptyenv())

  lines1 <- test_data[["lines"]]
  lines2 <- c(lines1, "")

  lintr:::cache_lint(
    cache = e1,
    expr = test_data[["expr"]],
    linter = test_data[["linters"]],
    lints = test_data[["lints"]]
  )

  t1 <- lintr:::retrieve_lint(
    cache = e1,
    expr = test_data[["expr"]],
    linter = test_data[["linters"]],
    lines = lines2
  )

  expect_identical(t1, test_data[["lints"]])
})

test_that(
  "retrieve_lint returns the same lints with fixed line numbers if lines added between",
  {
    test_data <- fixtures$retrieve_lint()

    e1 <- new.env(parent = emptyenv())

    lines1 <- test_data[["lines"]]
    lines2 <- c(lines1[1L], "", lines1[2L:3L], "")

    lints1 <- test_data[["lints"]]

    lintr:::cache_lint(
      cache = e1,
      expr = test_data[["expr"]],
      linter = test_data[["linters"]],
      lints = lints1
    )

    t1 <- lintr:::retrieve_lint(
      cache = e1,
      expr = test_data[["expr"]],
      linter = test_data[["linters"]],
      lines = lines2
    )

    expect_identical(t1[[1L]]$line_number, lints1[[1L]]$line_number)
    expect_identical(t1[[2L]]$line_number, lints1[[2L]]$line_number + 1L)
    expect_identical(t1[[3L]]$line_number, lints1[[3L]]$line_number + 1L)
  }
)

# `has_lint`

test_that("has_lint returns FALSE if there is no cached result", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "foobar")
  expect_false(lintr:::has_lint(e1, t1, list()))
})

test_that("has_lint returns TRUE if there is a cached result", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "foobar")
  lintr:::cache_lint(e1, t1, list(), list())
  expect_true(lintr:::has_lint(e1, t1, list()))
})

test_that("has_lint distinguishes global expressions from line expression with same content", {
  e1 <- new.env(parent = emptyenv())

  same_content <- "foobar"

  line_expr <- list(content = same_content, parsed_content = data.frame())
  lintr:::cache_lint(e1, line_expr, list(), list())

  global_expr <- list(content = same_content, file_lines = character())
  expect_false(lintr:::has_lint(e1, global_expr, list()))
})

# `find_new_line`

test_that("find_new_line returns the same if the line is the same", {
  t1 <- c(
    "foobar1",
    "foobar2",
    "foobar3"
  )

  expect_identical(lintr:::find_new_line(1L, "foobar1", t1), 1L)
  expect_identical(lintr:::find_new_line(2L, "foobar2", t1), 2L)
  expect_identical(lintr:::find_new_line(3L, "foobar3", t1), 3L)
})

test_that("find_new_line returns the correct line if it is before the current line", {
  t1 <- c(
    "foobar1",
    "foobar2",
    "foobar3"
  )

  expect_identical(lintr:::find_new_line(1L, "foobar1", t1), 1L)
  expect_identical(lintr:::find_new_line(2L, "foobar1", t1), 1L)
  expect_identical(lintr:::find_new_line(3L, "foobar1", t1), 1L)
})

test_that("find_new_line returns the correct line if it is after the current line", {
  t1 <- c(
    "foobar1",
    "foobar2",
    "foobar3"
  )

  expect_identical(lintr:::find_new_line(1L, "foobar3", t1), 3L)
  expect_identical(lintr:::find_new_line(2L, "foobar3", t1), 3L)
  expect_identical(lintr:::find_new_line(3L, "foobar3", t1), 3L)
})

#

test_that("lint with cache uses the provided relative cache directory", {
  path <- withr::local_tempdir("my_cache_dir")
  linter <- assignment_linter()

  # create the cache
  expect_lint("a <- 1", NULL, linter, cache = path)
  expect_true(dir.exists(path))
  expect_length(list.files(file.path(path)), 1L)

  # read the cache
  expect_lint("a <- 1", NULL, linter, cache = path)
  expect_true(dir.exists(path))
})

test_that("it works outside of a package", {
  linter <- assignment_linter()

  local_mocked_bindings(find_package = function(...) NULL)
  path <- withr::local_tempfile(pattern = "my_cache_dir_")
  expect_false(dir.exists(path))
  expect_lint("a <- 1", NULL, linter, cache = path)
  expect_true(dir.exists(path))
  expect_length(list.files(path), 1L)
  expect_lint("a <- 1", NULL, linter, cache = path)
  expect_true(dir.exists(path))
})

test_that("cache = TRUE workflow works", {
  # Need a test structure with a safe to load .lintr
  withr::local_dir(file.path("dummy_packages", "package"))
  withr::local_options(lintr.linter_file = "lintr_test_config")
  files <- normalize_path(list.files(recursive = TRUE, full.names = TRUE))

  # Manually clear cache (that function is exported)
  for (f in files) {
    clear_cache(file = f)
  }
  l1 <- lint_package(cache = TRUE)
  l2 <- lint_package(cache = TRUE)
  expect_identical(l1, l2)
})

test_that("cache = TRUE works with nolint", {
  linters <- list(infix_spaces_linter())
  file <- withr::local_tempfile()

  writeLines("1+1\n", file)
  expect_length(lint(file, linters, cache = TRUE), 1L)

  writeLines("1+1 # nolint\n", file)
  expect_length(lint(file, linters, cache = TRUE), 0L)

  writeLines("1+1\n", file)
  expect_length(lint(file, linters, cache = TRUE), 1L)

  writeLines("1+1 # nolint\n", file)
  expect_length(lint(file, linters, cache = TRUE), 0L)
})
