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
