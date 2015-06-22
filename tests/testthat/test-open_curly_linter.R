context("open_curly_linter()")
test_that("returns the correct linting", {

  expect_lint("blah", NULL, open_curly_linter())

  expect_lint("a <- function() {\n}", NULL, open_curly_linter())

  expect_lint(
"pkg_name <- function(path = find_package()) {
  if (is.null(path)) {
    return(NULL)
  } else {
    read.dcf(file.path(path, \"DESCRIPTION\"), fields = \"Package\")[1]
  }
}", NULL, open_curly_linter())

  expect_lint("a <- function()\n{\n  1 \n}",
    rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
    open_curly_linter())

  expect_lint("a <- function()\n    {\n  1 \n}",
    rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
    open_curly_linter())

  expect_lint("a <- function()\n\t{\n  1 \n}",
    rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
    open_curly_linter())

  expect_lint("a <- function() {  \n}",
    rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
    open_curly_linter())

  expect_lint("a <- function() { 1 }",
    rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
    open_curly_linter())

  expect_lint("a <- function() { 1 }",
    NULL,
    open_curly_linter(allow_single_line = TRUE))
})
