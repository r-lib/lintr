test_that("else_same_line_linter skips allowed usages", {
  expect_lint("if (TRUE) 1 else 2", NULL, else_same_line_linter())
  expect_lint("if (TRUE) 1", NULL, else_same_line_linter())

  lines_brace <- trim_some("
    if (TRUE) {
      1
    } else {
      2
    }
  ")
  expect_lint(lines_brace, NULL, else_same_line_linter())

  # such usage is also not allowed by the style guide, but test anyway
  lines_unbrace <- trim_some("
    foo <- function(x) {
      if (TRUE)
        1
      else
        2
    }
  ")
  expect_lint(lines_unbrace, NULL, else_same_line_linter())
})

test_that("else_same_line_linter blocks disallowed usage", {
  lines <- trim_some("
    foo <- function(x) {
      if (x) {
        1
      }
      else {
        2
      }
    }
  ")
  expect_lint(
    lines,
    rex::rex("`else` should come on the same line as the previous `}`."),
    else_same_line_linter()
  )
})
