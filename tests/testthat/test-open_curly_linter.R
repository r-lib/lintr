test_that("returns the correct linting", {
  msg <- rex("Opening curly braces should never go on their own line and should always be followed by a new line.")

  expect_warning(
    linter <- open_curly_linter(),
    "Linter open_curly_linter was deprecated",
    fixed = TRUE
  )

  expect_lint("blah", NULL, linter)

  expect_lint("a <- function() {\n}", NULL, linter)

  expect_lint(
    "pkg_name <- function(path = find_package()) {
      if (is.null(path)) {
        return(NULL)
      } else {
        read.dcf(file.path(path, \"DESCRIPTION\"), fields = \"Package\")[1]
      }
    }", NULL, linter)

  expect_lint("a <- function()\n{\n  1 \n}",
              msg,
              linter)

  expect_lint("a <- function()\n    {\n  1 \n}",
              msg,
              linter)

  expect_lint("a <- function()\n\t{\n  1 \n}",
              msg,
              linter)

  expect_lint("a <- function() {  \n}",
              msg,
              linter)

  expect_lint("a <- function() { 1 }",
              msg,
              linter)

  expect_lint("a <- function() { 1 }",
    NULL,
    suppressWarnings(open_curly_linter(allow_single_line = TRUE)))

  expect_lint(
'if ("P" != "NP") { # what most people expect
    print("Cryptomania is possible")
}',
    NULL,
linter
  )

  expect_lint("{{x}}", NULL, linter)
})
