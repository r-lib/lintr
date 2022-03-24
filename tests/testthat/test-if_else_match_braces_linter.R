test_that("if_else_match_braces_linter skips allowed usages", {
  expect_lint("if (TRUE) 1 else 2", NULL, if_else_match_braces_linter())
  expect_lint("if (TRUE) 1", NULL, if_else_match_braces_linter())

  lines_brace <- trim_some("
    if (TRUE) {
      1
    } else {
      2
    }
  ")
  expect_lint(lines_brace, NULL, if_else_match_braces_linter())

  # such usage is also not allowed by the style guide, but test anyway
  lines_unbrace <- trim_some("
    foo <- function(x) {
      if (TRUE)
        1
      else
        2
    }
  ")
  expect_lint(lines_unbrace, NULL, if_else_match_braces_linter())

  # else if is OK
  lines_else_if <- trim_some("
    if (x) {
     1
    } else if (y) {
     2
    } else {
     3
    }
  ")
  expect_lint(lines_else_if, NULL, if_else_match_braces_linter())
})

test_that("if_else_match_braces_linter blocks disallowed usage", {
  lines_if <- trim_some("
    foo <- function(x) {
      if (x) {
        1
      } else 2
    }
  ")
  expect_lint(
    lines_if,
    rex::rex("Either both or neither branch in `if`/`else` should use curly braces."),
    if_else_match_braces_linter()
  )

  lines_else <- trim_some("
    foo <- function(x) {
      if (x) 1 else {
        2
      }
    }
  ")
  expect_lint(
    lines_else,
    rex::rex("Either both or neither branch in `if`/`else` should use curly braces."),
    if_else_match_braces_linter()
  )
})
