test_that("returns the correct linting", {
  ops <- c(
    "+",
    "-",
    "=",
    "==",
    "!=",
    "<=",
    ">=",
    "<-",
    "<",
    ">",
    "->",
    "%%",
    "/",
    "*",
    "|",
    "||",
    "&",
    "&&",
    "%>%",
    "%Anything%",
    "%+%",
    NULL
  )

  linter <- infix_spaces_linter()
  msg <- rex("Put spaces around all infix operators.")

  expect_lint("blah", NULL, linter)

  for (op in ops) {
    expect_lint(paste0("1 ", op, " 2"), NULL, linter)
    expect_lint(paste0("1 ", op, "\n2"), NULL, linter)
    expect_lint(paste0("1 ", op, "\n 2"), NULL, linter)

    expect_lint(paste0("1", op, "2"), msg, linter)

    # unary plus and minus can have no space before them
    if (!op %in% ops[1:2]) {
      expect_lint(paste0("1 ", op, "2"), msg, linter)
    }

    expect_lint(paste0("1", op, " 2"), msg, linter)
  }

  expect_lint("b <- 2E+4", NULL, linter)
  expect_lint("a <- 1e-3", NULL, linter)
  expect_lint("a[-1]", NULL, linter)
  expect_lint("a[-1 + 1]", NULL, linter)
  expect_lint("a[1 + -1]", NULL, linter)

  expect_lint("fun(a=1)", msg, linter)
})
