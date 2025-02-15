test_that("returns the correct linting", {
  skip_if_not_installed("cyclocomp")

  cc_linter_1 <- cyclocomp_linter(1L)
  cc_linter_2 <- cyclocomp_linter(2L)
  lint_msg <- rex::rex("Reduce the cyclomatic complexity of this function")

  expect_lint("if (TRUE) 1 else 2", NULL, cc_linter_2)
  expect_lint("if (TRUE) 1 else 2", lint_msg, cc_linter_1)

  expect_lint(
    "function(x) {not parsing}",
    "unexpected symbol", cc_linter_2
  )

  complex_lines <- trim_some("
    complexity <- function(x) {
      if (x > 0.0) {
        if (x > 10.0) {
          if (x > 20.0) {
            x <- x / 2.0
          } else {
            return(x)
          }
        } else {
          return(x)
        }
      } else {
        if (x < -10.0) {
          if (x < -20.0) {
            x <- x * 2.0
          } else {
            return(x)
          }
        } else {
          return(x)
        }
      }
      x
    }
  ")
  expect_lint(complex_lines, lint_msg, cc_linter_2)
  expect_lint(
    complex_lines,
    list(rex::rex("Reduce the cyclomatic complexity of this function from 10 to at most 2."), line_number = 1L),
    cc_linter_2
  )
  expect_lint(complex_lines, NULL, cyclocomp_linter(10L))
})
