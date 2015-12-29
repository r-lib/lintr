context("commented_code_linter")
test_that("returns the correct linting", {
  expect_lint("blah",
    NULL,
    commented_code_linter)

  expect_lint("# blah <- 1",
    rex("Commented code should be removed"),
    commented_code_linter)

  expect_lint("bleurgh <- fun_call(1) # other_call()",
    c(message = rex("Commented code should be removed"),
      column_number = 26),
    commented_code_linter)

  expect_lint(" #blah <- 1",
    c(message = rex("Commented code should be removed"),
      column_number = 3),
    commented_code_linter)

  expect_lint("#' blah <- 1",
    NULL,
    commented_code_linter)

  expect_lint(c("a <- 1",
                "# comment without code"),
    NULL,
    commented_code_linter)

  expect_lint(c("a <- 1",
                "# comment without code"),
    NULL,
    commented_code_linter)

  test_ops <- append(ops[ops != "%[^%]*%"], values = c("%>%", "%anything%"))
  for (op in test_ops) {
    expect_lint(paste("i", op, "1", collapse = ""), NULL, commented_code_linter)

    expect_lint(paste("# something like i", op, "1", collapse = ""),
      NULL,
      commented_code_linter)

    expect_lint(paste("# i", op, "1", collapse = ""),
      rex("Commented code should be removed"),
      commented_code_linter)
  }

})
