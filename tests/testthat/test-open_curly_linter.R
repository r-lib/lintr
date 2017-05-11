context("open_curly_linter")

test_that("returns the correct linting", {
  msg <- rex("Opening curly braces should never go on their own line and should always be followed by a new line.")
  linter <- open_curly_linter()
  expect_is(linter, "linter")

  expect_lint("blah", NULL, open_curly_linter())

  expect_lint("a <- function() {\n}", NULL, linter)

  expect_lint(
"pkg_name <- function(path = find_package()) {
  if (is.null(path)) {
    return(NULL)
  } else {
    read.dcf(file.path(path, \"DESCRIPTION\"), fields = \"Package\")[1]
  }
}", NULL, linter)

  expect_lint("a <- function()\n{\n  1 \n}", msg, linter)

  expect_lint("a <- function()\n    {\n  1 \n}", msg, linter)

  expect_lint("a <- function()\n\t{\n  1 \n}", msg, linter)

  expect_lint("a <- function() {  \n}", msg, linter)

  expect_lint("a <- function() { 1 }", msg, linter)

  expect_lint("a <- function() { 1 }",
              NULL,
              open_curly_linter(allow_single_line = TRUE))
})
