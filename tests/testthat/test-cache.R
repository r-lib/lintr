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

test_that("cached lints adjust correctly right when line numbers shift up/down or vanish", {
  path <- withr::local_tempdir()
  file <- withr::local_tempfile(fileext = ".R")
  linter <- commas_linter()

  # Initial run and exact match at line 2 (`x <- c(1,2)` triggers a comma spacing lint)
  writeLines(c("# top", "x <- c(1,2)", "# bottom"), file)
  l1 <- lint(filename = file, linters = linter, cache = path)
  expect_length(l1, 1L)
  expect_identical(l1[[1L]]$line_number, 2L)

  # Exact hit when exact line matches after modifying surrounding comments
  writeLines(c("# top modified", "x <- c(1,2)", "# bottom modified"), file)
  l2 <- lint(filename = file, linters = linter, cache = path)
  expect_length(l2, 1L)
  expect_identical(l2[[1L]]$line_number, 2L)

  # Line shifts earlier: move expression to line 1
  writeLines(c("x <- c(1,2)", "# mid modified", "# bottom"), file)
  l_low <- lint(filename = file, linters = linter, cache = path)
  expect_length(l_low, 1L)
  expect_identical(l_low[[1L]]$line_number, 1L)

  # Line shifts later: move expression to line 3
  writeLines(c("# top modified again", "# mid modified again", "x <- c(1,2)"), file)
  l_high <- lint(filename = file, linters = linter, cache = path)
  expect_length(l_high, 1L)
  expect_identical(l_high[[1L]]$line_number, 3L)

  # Line text no longer present (hidden by nolint)
  writeLines(c("# top modified again", "# mid modified again", "x <- c(1,2) # nolint"), file)
  expect_length(lint(filename = file, linters = linter, cache = path), 0L)
})

test_that("cache loading muffles load warnings and warns gracefully on read failures", {
  path <- withr::local_tempdir()
  file <- withr::local_tempfile(fileext = ".R", lines = "a <- 1")
  linter <- assignment_linter()

  # Populate cache
  expect_length(lint(filename = file, linters = linter, cache = path), 0L)

  # When load emits a warning during public lint(), suppressWarnings handles it silently
  local_mocked_bindings(load = \(...) cli::cli_warn("fake warning"), .package = "base")
  expect_length(lint(filename = file, linters = linter, cache = path), 0L)

  # When load encounters an error during public lint(), a descriptive warning is bubbled up
  local_mocked_bindings(load = \(...) cli::cli_abort("fake error"), .package = "base")
  expect_warning(lint(filename = file, linters = linter, cache = path), "Could not load cache file")
})

test_that("parser errors and parser warnings are cached appropriately", {
  path <- withr::local_tempdir("cond_cache_dir")
  file_err <- withr::local_tempfile(fileext = ".R", lines = "function() {)")
  expect_gt(length(lint(file_err, cache = path)), 0L)
  expect_gt(length(lint(file_err, cache = path)), 0L)

  file_warn <- withr::local_tempfile(fileext = ".R", lines = "100000000000000000000000000000000000L")
  expect_gt(length(lint(file_warn, cache = path)), 0L)
  expect_gt(length(lint(file_warn, cache = path)), 0L)
})
