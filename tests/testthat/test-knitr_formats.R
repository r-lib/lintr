context("knitr_formats")

regexes <- list(
  assign = rex("Use <-, not =, for assignment."),
  local_var = rex("local variable"),
  quotes = rex("Only use double-quotes."),
  trailing = rex("Trailing blank lines are superfluous.")
)

test_that("it handles dir", {
  lints <- lint_dir(path = "knitr_formats", pattern = rex::rex(".R", one_of("html", "md", "nw", "rst", "tex", "txt")))
  has_lints <- length(lints) > 0
  testthat::expect(has_lints, "There should be lints")

  testthat::expect_equivalent(length(unique(names(lints))), 6, info="For every file there should be at least 1 lint")
})

test_that("it handles markdown", {
  expect_lint(file = "knitr_formats/test.Rmd",
    checks = list(
      list(regexes[["assign"]], line_number = 9),
      list(regexes[["local_var"]], line_number = 22),
      list(regexes[["assign"]], line_number = 22),
      list(regexes[["trailing"]], line_number = 24)
    ),
    default_linters
  )
})

test_that("it handles Sweave", {
  expect_lint(file = "knitr_formats/test.Rnw",
    checks = list(
      list(regexes[["assign"]], line_number = 12),
      list(regexes[["local_var"]], line_number = 24),
      list(regexes[["assign"]], line_number = 24),
      list(regexes[["trailing"]], line_number = 26)
    ),
    default_linters
  )
})

test_that("it handles reStructuredText", {
  expect_lint(file = "knitr_formats/test.Rrst",
    checks = list(
      list(regexes[["assign"]], line_number = 10),
      list(regexes[["local_var"]], line_number = 23),
      list(regexes[["assign"]], line_number = 23),
      list(regexes[["trailing"]], line_number = 25)
    ),
    default_linters
  )
})

test_that("it handles HTML", {
  expect_lint(file = "knitr_formats/test.Rhtml",
    checks = list(
      list(regexes[["assign"]], line_number = 15),
      list(regexes[["local_var"]], line_number = 27),
      list(regexes[["assign"]], line_number = 27),
      list(regexes[["trailing"]], line_number = 29)
    ),
    default_linters
  )
})

test_that("it handles tex", {
  expect_lint(file = "knitr_formats/test.Rtex",
    checks = list(
      list(regexes[["assign"]], line_number = 11),
      list(regexes[["local_var"]], line_number = 23),
      list(regexes[["assign"]], line_number = 23),
      list(regexes[["trailing"]], line_number = 25)
    ),
    default_linters
  )
})

test_that("it handles asciidoc", {
  expect_lint(file = "knitr_formats/test.Rtxt",
    checks = list(
      list(regexes[["assign"]], line_number = 9),
      list(regexes[["local_var"]], line_number = 22),
      list(regexes[["assign"]], line_number = 22),
      list(regexes[["trailing"]], line_number = 24)
    ),
    default_linters
  )
})

test_that("it does _not_ handle brew", {
  expect_lint("'<% a %>'\n",
    checks = list(
      regexes[["quotes"]],
      regexes[["trailing"]]
    ),
    default_linters
  )
})

test_that("it does _not_ error with inline \\Sexpr", {
  expect_lint("#' text \\Sexpr{1 + 1} more text",
    NULL,
    default_linters
  )
})
