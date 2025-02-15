test_that("redundant_equals_linter skips allowed usages", {
  # comparisons to non-logical constants
  expect_lint("x == 1", NULL, redundant_equals_linter())
  # comparison to TRUE as a string
  expect_lint("x != 'TRUE'", NULL, redundant_equals_linter())
})

test_that("multiple lints return correct custom messages", {
  expect_lint(
    trim_some("
      list(
        x == TRUE,
        y != TRUE
      )
    "),
    list(
      list("Using == on a logical vector", line_number = 2L),
      list("Using != on a logical vector", line_number = 3L)
    ),
    redundant_equals_linter()
  )
})

test_that("Order doesn't matter", {
  expect_lint("TRUE == x", rex::rex("Using == on a logical vector is redundant."), redundant_equals_linter())
})

patrick::with_parameters_test_that(
  "redundant_equals_linter blocks simple disallowed usages",
  {
    lint_msg <- rex::rex(paste("Using", op, "on a logical vector is redundant."))
    code <- paste("x", op, bool)
    expect_lint(code, lint_msg, redundant_equals_linter())
  },
  .cases = tibble::tribble(
    ~.test_name, ~op,  ~bool,
    "==, TRUE",  "==", "TRUE",
    "==, FALSE", "==", "FALSE",
    "!=, TRUE",  "!=", "TRUE",
    "!=, FALSE", "!=", "FALSE"
  )
)
