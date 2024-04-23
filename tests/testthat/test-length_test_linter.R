test_that("skips allowed usages", {
  linter <- length_test_linter()

  expect_lint("length(x) > 0", NULL, linter)
  expect_lint("length(DF[key == val, cols])", NULL, linter)
})

test_that("blocks simple disallowed usages", {
  linter <- length_test_linter()
  lint_msg_stub <- rex::rex("Checking the length of a logical vector is likely a mistake. Did you mean ")

  expect_lint("length(x == 0)", rex::rex(lint_msg_stub, "`length(x) == 0`?"), linter)
  expect_lint("length(x == y)", rex::rex(lint_msg_stub, "`length(x) == y`?"), linter)
  expect_lint("length(x + y == 2)", rex::rex(lint_msg_stub, "`length(x+y) == 2`?"), linter)
})

local({
  ops <- c(lt = "<", lte = "<=", gt = ">", gte = ">=", eq = "==", neq = "!=")
  linter <- length_test_linter()
  lint_msg_stub <- rex::rex("Checking the length of a logical vector is likely a mistake. Did you mean ")

  patrick::with_parameters_test_that(
    "all logical operators detected",
    expect_lint(
      paste("length(x", op, "y)"),
      rex::rex("`length(x) ", op, " y`?"),
      linter
    ),
    op = ops,
    .test_name = names(ops)
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
