test_that("returns the correct linting", {
  expect_warning(
    linter <- open_curly_linter(),
    "Linter open_curly_linter was deprecated",
    fixed = TRUE
  )

  expect_lint("blah", NULL, linter)

  expect_lint(
    trim_some("
      a <- function() {
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some('
      pkg_name <- function(path = find_package()) {
        if (is.null(path)) {
          return(NULL)
        } else {
          read.dcf(file.path(path, "DESCRIPTION"), fields = "Package")[1]
        }
      }
    '),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      a <- function()
      {
        1
      }
    "),
    rex::rex("Opening curly braces should never go on their own line."),
    linter
  )

  expect_lint(
    trim_some("
      a <- function()
          {
        1
      }
    "),
    rex::rex("Opening curly braces should never go on their own line."),
    linter
  )

  expect_lint(
    trim_some("
      a <- function()
      \t{
        1
      }
    "),
    rex::rex("Opening curly braces should never go on their own line"),
    linter
  )

  expect_lint(
    "a <- function() { 1 }",
    rex::rex("Opening curly braces should always be followed by a new line"),
    linter
  )

  expect_lint(
    trim_some('
      if ("P" != "NP") { # what most people expect
        print("Cryptomania is possible")
      }
    '),
    NULL,
    linter
  )

  expect_lint("{{x}}", NULL, linter)
})

test_that("allow_single_line=TRUE works", {
  expect_warning(
    linter <- open_curly_linter(allow_single_line = TRUE),
    "Linter open_curly_linter was deprecated",
    fixed = TRUE
  )

  expect_lint("a <- function() { 1 }", NULL, linter)

  expect_lint(
    trim_some("
      a <- function() { 1
        2 }
    "),
    rex::rex("Opening curly braces should always be followed by a new line unless"),
    linter
  )
})
