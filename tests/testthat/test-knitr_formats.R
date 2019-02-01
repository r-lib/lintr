context("knitr_formats")
test_that("it handles markdown", {
  expect_lint(file = "knitr_formats/test.Rmd",
    checks = list(
      rex("Use <-, not =, for assignment."),
      rex("local variable"),
      rex("Use <-, not =, for assignment."),
      rex("Trailing blank lines are superfluous.")
    ),
    default_linters
  )
})
test_that("it handles Sweave", {
  expect_lint(file = "knitr_formats/test.Rnw",
    checks = list(
      rex("Use <-, not =, for assignment."),
      rex("local variable"),
      rex("Use <-, not =, for assignment."),
      rex("Trailing blank lines are superfluous.")
    ),
    default_linters
  )
})
test_that("it handles reStructuredText", {
  expect_lint(file = "knitr_formats/test.Rrst",
    checks = list(
      rex("Use <-, not =, for assignment."),
      rex("local variable"),
      rex("Use <-, not =, for assignment."),
      rex("Trailing blank lines are superfluous.")
    ),
    default_linters
  )
})
test_that("it handles HTML", {
  expect_lint(file = "knitr_formats/test.Rhtml",
    checks = list(
      rex("Use <-, not =, for assignment."),
      rex("local variable"),
      rex("Use <-, not =, for assignment."),
      rex("Trailing blank lines are superfluous.")
    ),
    default_linters
  )
})
test_that("it handles tex", {
  expect_lint(file = "knitr_formats/test.Rtex",
    checks = list(
      rex("Use <-, not =, for assignment."),
      rex("local variable"),
      rex("Use <-, not =, for assignment."),
      rex("Trailing blank lines are superfluous.")
    ),
    default_linters
  )
})
test_that("it handles asciidoc", {
  expect_lint(file = "knitr_formats/test.Rtxt",
    checks = list(
      rex("Use <-, not =, for assignment."),
      rex("local variable"),
      rex("Use <-, not =, for assignment."),
      rex("Trailing blank lines are superfluous.")
    ),
    default_linters
  )
})

test_that("it does _not_ handle brew", {
  expect_lint("'<% a %>'\n",
    checks = list(
      rex("Only use double-quotes."),
      rex("Trailing blank lines are superfluous.")
    ),
    default_linters)
})

test_that("it does _not_ error with inline \\Sexpr", {
  expect_lint("#' text \\Sexpr{1 + 1} more text\n",
    checks = list(
      rex("Trailing blank lines are superfluous.")
    ),
    default_linters)
})

test_that("it does not error with text-only R-markdown", {
  expect_lint(file = "knitr_formats/text-only.Rmd",
    checks = NULL,
    default_linters)
})
