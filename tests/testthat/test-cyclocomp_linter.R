test_that("returns the correct linting", {
  skip_if_not_installed("cyclocomp")

  cc_linter_1 <- cyclocomp_linter(1L)
  cc_linter_2 <- cyclocomp_linter(2L)
  lint_msg <- rex::rex("Reduce the cyclomatic complexity of this expression")

  expect_no_lint("if (TRUE) 1 else 2", cc_linter_2)
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
    list(rex::rex("Reduce the cyclomatic complexity of this expression from 10 to at most 2."), line_number = 1L),
    cc_linter_2
  )
  expect_no_lint(complex_lines, cyclocomp_linter(10L))

  # no function involved, still lints, hence use 'expression' in the message
  expect_lint(
    trim_some("
      for (i in 1:10)
        for (j in 1:10)
          for (k in 1:10)
            i*j*k
    "),
    lint_msg,
    cyclocomp_linter(5L)
  )
})

test_that("a null linter is returned, with warning, if cyclocomp is unavailable", {
  # simple requireNamspace->FALSE won't work since expect_no_lint checks for testthat
  local_mocked_bindings(
    requireNamespace = \(pkg, ...) pkg != "cyclocomp" && base::requireNamespace(pkg, ...)
  )
  expect_warning(regexp = "Please install", fixed = TRUE, {
    linter <- cyclocomp_linter(1L)
  })

  expect_error(lint(text = "if (TRUE) 1 else 2", linters = linter), "disabled", fixed = TRUE)
})
