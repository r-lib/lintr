test_that("skips allowed usages", {
  linter <- length_test_linter()

  expect_no_lint("length(x) > 0", linter)
  expect_no_lint("length(DF[key == val, cols])", linter)
  expect_no_lint("nrow(x) > 0", linter)
  expect_no_lint("nrow(DF[key == val, cols])", linter)
  expect_no_lint("ncol(x) > 0", linter)
  expect_no_lint("ncol(DF[key == val, cols])", linter)
  expect_no_lint("NROW(x) > 0", linter)
  expect_no_lint("NROW(DF[key == val, cols])", linter)
  expect_no_lint("NCOL(x) > 0", linter)
  expect_no_lint("NCOL(DF[key == val, cols])", linter)
})

local({
  linter <- length_test_linter()

  patrick::with_parameters_test_that(
    "blocks simple disallowed usages",
    {
      lint_msg_stub <- sprintf("Checking the %s of a logical vector is likely a mistake. Did you mean ", fun)
      expect_lint(
          paste0(fun, "(x == 0)"),
          rex::rex(lint_msg_stub, "`", fun, "(x) == 0`?"),
          linter
      )
      expect_lint(
          paste0(fun, "(x == y)"),
          rex::rex(lint_msg_stub, "`", fun, "(x) == y`?"),
          linter
      )
      expect_lint(
          paste0(fun, "(x + y == 2)"),
          rex::rex(lint_msg_stub, "`", fun, "(x+y) == 2`?"),
          linter
      )
    },
    fun = c("length", "nrow", "ncol", "NROW", "NCOL")
  )
})

local({
  cases <- expand.grid(op = c("<", "<=", ">", ">=", "==", "!="),
                       fun = c("length", "nrow", "ncol", "NROW", "NCOL"))
  cases$.test_name <- with(cases, paste(fun, op))
  linter <- length_test_linter()

  patrick::with_parameters_test_that(
    "all logical operators detected",
    expect_lint(
      paste0(fun, "(x ", op, " y)"),
      rex::rex("`", fun, "(x) ", op, " y`?"),
      linter
    ),
    .cases = cases
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      length(x == y)
      length(y == z)
    }"),
    list(
      list(rex::rex("length(x) == y"), line_number = 2L),
      list(rex::rex("length(y) == z"), line_number = 3L)
    ),
    length_test_linter()
  )
})
