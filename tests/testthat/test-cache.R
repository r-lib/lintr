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

test_that("clear_cache deletes the directory if no file is given", {
  local_mocked_bindings(
    read_settings = \(...) invisible(...),
    unlink = \(...) list(...)
  )

  expect_identical(clear_cache(file = NULL, path = "."), list(".", recursive = TRUE))
})

test_that("lint with cache uses the provided relative cache directory", { # nofuzz: assignment
  path <- withr::local_tempdir("my_cache_dir")
  linter <- assignment_linter()

  # create the cache
  expect_no_lint("a <- 1", linter, cache = path)
  expect_true(dir.exists(path))
  expect_length(list.files(file.path(path)), 1L)

  # read the cache
  expect_no_lint("a <- 1", linter, cache = path)
  expect_true(dir.exists(path))
})

test_that("it works outside of a package", { # nofuzz: assignment
  linter <- assignment_linter()

  local_mocked_bindings(find_package = \(...) NULL)
  path <- withr::local_tempfile(pattern = "my_cache_dir_")
  expect_false(dir.exists(path))
  expect_no_lint("a <- 1", linter, cache = path)
  expect_true(dir.exists(path))
  expect_length(list.files(path), 1L)
  expect_no_lint("a <- 1", linter, cache = path)
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

test_that("load_cache muffles warnings and catches corrupted files", {
  path <- withr::local_tempdir()
  file <- "dummy.R"
  cache_path <- file.path(path, digest::digest(file, algo = "sha1"))

  # Test warning handling during load (file must exist first)
  writeLines("dummy content", cache_path)
  local_mocked_bindings(load = \(...) warning("fake warning"), .package = "base")
  expect_silent(load_cache(file, path))


  # Test error handling when cache file is corrupted or cannot be read by load
  local_mocked_bindings(load = \(...) stop("fake error"), .package = "base")
  writeLines("not rdata", cache_path)
  expect_warning(load_cache(file, path), "Could not load cache file")
})

test_that("retrieve_lint and find_new_line handle moved and removed lines", {
  cache <- new.env(parent = emptyenv())
  expr <- list(content = "x <- 1\n", parsed_content = data.frame())
  linter <- "dummy_linter"
  lints <- list(
    Lint("test.R", line_number = 2L, line = "line_exact"),
    Lint("test.R", line_number = 2L, line = "line_low"),
    Lint("test.R", line_number = 2L, line = "line_high"),
    Lint("test.R", line_number = 2L, line = "line_missing")
  )

  # When a line cannot be found, retrieve_lint returns NULL
  cache_lint(cache, expr, linter, lints)
  lines <- c("line_low", "line_exact", "line_high")
  expect_null(retrieve_lint(cache, expr, linter, lines))

  # When all lines are found across exact, lower, or higher offsets
  cache_lint(cache, expr, linter, lints[1L:3L])
  ret <- retrieve_lint(cache, expr, linter, lines)
  expect_length(ret, 3L)
  expect_identical(vapply(ret, `[[`, integer(1L), "line_number"), c(2L, 1L, 3L))
})

test_that("parser errors and parser warnings are cached appropriately", {
  path <- withr::local_tempdir("cond_cache_dir")
  file_err <- withr::local_tempfile(fileext = ".R", lines = "function() {)")
  expect_true(length(lint(file_err, cache = path)) > 0L)
  expect_true(length(lint(file_err, cache = path)) > 0L)

  file_warn <- withr::local_tempfile(fileext = ".R", lines = "100000000000000000000000000000000000L")
  expect_true(length(lint(file_warn, cache = path)) > 0L)
  expect_true(length(lint(file_warn, cache = path)) > 0L)
})


