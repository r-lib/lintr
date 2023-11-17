# NB: suppressWarnings() is intentionally excluded
kBlockedCalls <- c(
  "suppressMessages",
  "suppressPackageStartupMessages"
)

patrick::with_parameters_test_that(
  "consecutive_suppression_linter skips allowed usages",
  {
    linter <- consecutive_suppression_linter()

    expect_lint(sprintf("%s(x)", call), NULL, linter)
    expect_lint(sprintf("%s(x, y, z)", call), NULL, linter)

    # intervening expression
    expect_lint(sprintf("%1$s(x); y; %1$s(z)", call), NULL, linter)

    # inline or potentially with gaps don't matter
    lines <- c(
      sprintf("%s(x)", call),
      "y",
      "",
      "stopifnot(z)"
    )
    expect_lint(lines, NULL, linter)

    # only suppressing calls with library()
    lines_consecutive <- c(
      sprintf("%s(x)", call),
      sprintf("%s(y)", call)
    )
    expect_lint(lines_consecutive, NULL, linter)
  },
  .test_name = kBlockedCalls,
  call = kBlockedCalls
)

patrick::with_parameters_test_that(
  "consecutive_suppression_linter blocks simple disallowed usages",
  {
    linter <- consecutive_suppression_linter()
    message <- sprintf("Unify consecutive calls to %s\\(\\)\\.", call)

    # one test of inline usage
    expect_lint(sprintf("%1$s(library(x)); %1$s(library(y))", call), message, linter)

    lines_gap <- c(
      sprintf("%s(library(x))", call),
      "",
      sprintf("%s(library(y))", call)
    )
    expect_lint(lines_gap, message, linter)

    lines_consecutive <- c(
      sprintf("%s(require(x))", call),
      sprintf("%s(require(y))", call)
    )
    expect_lint(lines_consecutive, message, linter)

    lines_comment <- c(
      sprintf("%s(library(x))", call),
      "# a comment on y",
      sprintf("%s(library(y))", call)
    )
    expect_lint(lines_comment, message, linter)
  },
  .test_name = kBlockedCalls,
  call = kBlockedCalls
)

test_that("Namespace differences are detected", {
  linter <- consecutive_suppression_linter()

  # totally different namespaces
  expect_lint(
    "ns::suppressMessages(library(x)); base::suppressMessages(library(y))",
    NULL,
    linter
  )

  # one namespaced, one not
  expect_lint(
    "ns::suppressMessages(library(x)); suppressMessages(library(y))",
    NULL,
    linter
  )
})

test_that("Consecutive calls to different blocked calls is OK", {
  expect_lint(
    "suppressPackageStartupMessages(library(x)); suppressMessages(library(y))",
    NULL,
    consecutive_suppression_linter()
  )
})

test_that("Multiple violations across different calls are caught", {
  multi_violation_lines <- c(
    "suppressPackageStartupMessages(library(x))",
    "suppressPackageStartupMessages(library(x))",
    "suppressMessages(library(x))",
    "suppressMessages(library(x))"
  )
  expect_lint(
    multi_violation_lines,
    list(
      "Unify consecutive calls to suppressPackageStartupMessages",
      "Unify consecutive calls to suppressMessages"
    ),
    consecutive_suppression_linter()
  )
})
