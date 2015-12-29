context("infix_spaces_linter")
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
    "^",
    "*",
    "**",
    "|",
    "||",
    "&",
    "&&",
    "%>%",
    "%Anything%",
    "%+%",
    NULL)

  expect_lint("blah", NULL, infix_spaces_linter)

  for (op in ops) {
    expect_lint(paste0("1 ", op, " 2"), NULL, infix_spaces_linter)

    expect_lint(paste0("1 ", op, "\n2"), NULL, infix_spaces_linter)

    expect_lint(paste0("1 ", op, "\n 2"), NULL, infix_spaces_linter)

    expect_lint(paste0("1", op, "2"),
      rex("Put spaces around all infix operators."),
      infix_spaces_linter)

    # unary plus and minus can have no space before them
    if (!op %in% ops[1:2]) {
      expect_lint(paste0("1 ", op, "2"),
        rex("Put spaces around all infix operators."),
        infix_spaces_linter)
    }

    expect_lint(paste0("1", op, " 2"),
      rex("Put spaces around all infix operators."),
      infix_spaces_linter)
  }

  expect_lint("b <- 2E+4", NULL, infix_spaces_linter)

  expect_lint("a <- 1e-3", NULL, infix_spaces_linter)

  expect_lint("a[-1]", NULL, infix_spaces_linter)

  expect_lint("a[-1 + 1]", NULL, infix_spaces_linter)

  expect_lint("a[1 + -1]", NULL, infix_spaces_linter)

  expect_lint("fun(a=1)",
    rex("Put spaces around all infix operators."),
    infix_spaces_linter)
})
