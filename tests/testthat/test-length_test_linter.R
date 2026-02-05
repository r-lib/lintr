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

patrick::with_parameters_test_that(
  "blocks simple disallowed usages",
  {
    linter <- length_test_linter()
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

test_that("adversarial comment is handled in lint message", {
  expect_lint(
    trim_some("
      length(x + #
      y == 2)
    "),
    rex::rex("Checking the length of a logical vector is likely a mistake. Did you mean `length(x+y) == 2`?"),
    length_test_linter()
  )
})

patrick::with_parameters_test_that(
  "all logical operators detected",
  expect_lint(
    sprintf("%s(x %s y)", fun, op),
    rex::rex("`", fun, "(x) ", op, " y`?"),
    length_test_linter()
  ),
  .cases = expand.grid(
    op = c("<", "<=", ">", ">=", "==", "!="),
    fun = c("length", "nrow", "ncol", "NROW", "NCOL")
  )
)

test_that("lints vectorize", {
  linter <- length_test_linter()

  expect_lint(
    trim_some("{
      length(x == y)
      length(y == z)
    }"),
    list(
      list(rex::rex("length(x) == y"), line_number = 2L),
      list(rex::rex("length(y) == z"), line_number = 3L)
    ),
    linter
  )

  expect_lint(
    trim_some("{
      length( # comment
      x       # comment
      ==      # comment
      y       # comment
      )       # comment
      length( # comment
      y       # comment
      ==      # comment
      z       # comment
      )
    }"),
    list(
      list(rex::rex("length(x) == y"), line_number = 2L),
      list(rex::rex("length(y) == z"), line_number = 7L)
    ),
    linter
  )
})
