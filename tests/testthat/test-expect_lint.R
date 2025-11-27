# testthat's expect_success() and expect_failure() can only handle the first expectation and are
# thus less than ideal to test expect_lint(), which can process multiple lints. If you want to test
# for failure, always put the lint check or lint field that must fail first.

linter <- assignment_linter()
lint_msg <- "Use one of <-, <<- for assignment, not ="

test_that("no checks", {
  expect_success(expect_no_lint("a", linter))
  expect_success(expect_no_lint("a=1", list()))
  expect_failure(expect_no_lint("a=1", linter))
})

test_that("single check", {
  expect_failure(expect_lint(character(), lint_msg, linter))
  expect_failure(expect_lint("", lint_msg, linter))

  expect_success(expect_lint(content = "a=1", checks = lint_msg, linters = linter))
  expect_success(expect_lint("a=1", lint_msg, linter))
  expect_failure(expect_lint("a=1", "asdf", linter))
  expect_success(expect_lint("a=1", c(message = lint_msg), linter))
  expect_failure(expect_lint("a=1", c(message = NULL), linter))
  expect_success(expect_lint("a=1", list(message = lint_msg, line_number = 1L), linter))
  expect_failure(expect_lint("a=1", c(line_number = 2L, message = lint_msg), linter))

  expect_error(expect_lint("a=1", c(message = lint_msg, lineXXX = 1L), linter), "Check 1 has an invalid field: lineXXX")

  expect_failure(expect_lint("foo ()", list(ranges = list(c(2L, 2L))), function_left_parentheses_linter()))
  expect_success(expect_lint("\t1", list(ranges = list(c(1L, 1L))), whitespace_linter()))
  expect_success(expect_lint("a=1", list(message = lint_msg, line_number = 1L), linter))
  expect_failure(expect_lint("a=1", list(2L, lint_msg), linter))

  expect_success(expect_lint("1:nrow(x)", "(nrow)", seq_linter()))
})

test_that("multiple checks", {
  expect_success(
    expect_lint(file = "exclusions-test", checks = as.list(rep(lint_msg, 9L)), linters = linter, parse_settings = FALSE)
  )

  expect_success(expect_lint("a=1; b=2", list(lint_msg, lint_msg), linter))
  expect_success(expect_lint("a=1; b=2", list(c(message = lint_msg), c(message = lint_msg)), linter))
  expect_success(expect_lint("a=1; b=2", list(c(line_number = 1L), c(linter = "assignment_linter")), linter))
  expect_success(expect_lint("a=1; b=2", list(lint_msg, list(line = "a=1; b=2", type = "style")), linter))
  expect_success(expect_lint("a=1\nb=2", list(c(line_number = 1L), c(line_number = 2L)), linter))
  expect_failure(expect_lint("a=1\nb=2", list(c(line_number = 2L), c(line_number = 2L)), linter))

  expect_success(expect_lint("a=1\nb=2", list(list(line_number = 1L), list(line_number = 2L)), linter))
  expect_failure(expect_lint("a=1; b=2", list(list(line_number = 2L), list(line_number = 2L)), linter))
  expect_success(
    expect_lint("\t1\n\t2", list("tabs", list(column_number = 1L, ranges = list(c(1L, 1L)))), whitespace_linter())
  )
})

test_that("expect_lint_free works", {
  withr::local_options(
    lintr.rstudio_source_markers = FALSE,
    lintr.linter_file = "lintr_test_config"
  )
  withr::local_envvar(c(NOT_CRAN = "true", R_COVR = "false"))

  expect_lint_free(test_path("dummy_packages", "clean"))
  expect_lint_free(test_path("dummy_packages", "clean_subdir", "r"))
  expect_failure(expect_lint_free(test_path("dummy_packages", "package")))
})

test_that("expect_lint doesn't change language", {
  withr::with_envvar(c(LANGUAGE = "mr"), {
    expect_success(expect_lint("a=1", lint_msg, linter))
    expect_identical(Sys.getenv("LANGUAGE"), "mr")
  })
  withr::with_envvar(c(LANGUAGE = NA), {
    expect_success(expect_lint("a=1", lint_msg, linter))
    expect_identical(Sys.getenv("LANGUAGE", unset = NA), NA_character_)
  })
})

test_that("execution without testthat gives the right errors", {
  local_mocked_bindings(requireNamespace = function(...) FALSE)
  lint_msg <- function(nm) rex::rex("`", nm, "()` is designed to work", anything, "testthat")

  expect_error(expect_lint(), lint_msg("expect_lint"))
  expect_error(lintr::expect_lint(), lint_msg("expect_lint"))
  expect_error(expect_no_lint(), lint_msg("expect_no_lint"))
  expect_error(expect_lint_free(), lint_msg("expect_lint_free"))
})

test_that("lint order can be ignored", {
  linters <- list(assignment_linter(), infix_spaces_linter())
  expected <- lapply(linters, function(l) list(linter = attr(l, "name")))
  expect_success(expect_lint("a=1", expected, linters, ignore_order = TRUE))
  expect_success(expect_lint("a=1", rev(expected), linters, ignore_order = TRUE))

  lines <- trim_some("
    a=1
    b=2
  ")
  expected <- list(list(line_number = 1L), list(line_number = 2L))
  expect_success(expect_lint(lines, expected, assignment_linter(), ignore_order = TRUE))
  expect_success(expect_lint(lines, rev(expected), assignment_linter(), ignore_order = TRUE))

  # a fuzz test, since base R doesn't have a trivial way to do permutations
  expected <- list(
    list(linter = "assignment_linter", line_number = 1L),
    list(linter = "assignment_linter", line_number = 2L),
    list(linter = "infix_spaces_linter", line_number = 1L),
    list(linter = "infix_spaces_linter", line_number = 2L)
  )
  expect_success(expect_lint(lines, expected[sample.int(4L)], linters, ignore_order = TRUE))
})
