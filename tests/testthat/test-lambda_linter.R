skip_if_not_r_version("4.1.0")

test_that("lambda_linter skips allowed usages", {
  linter <- lambda_linter()
  expect_no_lint("foo <- function(x) x", linter)
  expect_no_lint("lapply(l, \\(x) x + 1))", linter)

  expect_no_lint(
    trim_some("
      map(xs, function(x) {
        mean((x + 5)^2)
      })
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      cv <- function(x) {
        sd(x) / mean(x)
      }
    "),
    linter
  )
})

test_that("lambda_linter blocks simple disallowed usages", {
  linter <- lambda_linter()

  expect_lint("foo <- \\(x) x", lint_msg, linter)
  expect_no_lint("lapply(l, function(x) x + 1))", lint_msg, linter)

  expect_no_lint(
    trim_some("
      map(xs, \\(x) {
        mean((x + 5)^2)
      })
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      cv <- \\(x) {
        sd(x) / mean(x)
      }
    "),
    linter
  )
})

test_that("lints vectorize", {
  lint_msg <- rex::rex("nlevels(x) is better than length(levels(x)).")

  expect_lint(
    trim_some("{
      length(levels(x))
      length(levels(y))
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    length_levels_linter()
  )
})
