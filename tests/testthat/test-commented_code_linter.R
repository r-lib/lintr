test_that("commented_code_linter skips allowed usages", {
  linter <- commented_code_linter()

  expect_no_lint("blah", linter)
  expect_no_lint("#' blah <- 1", linter)
  expect_no_lint("a <- 1\n# comment without code", linter)
  expect_no_lint("a <- 1\n## whatever", linter)

  expect_no_lint("TRUE", linter)
  expect_no_lint("#' @examples", linter)
  expect_no_lint("#' foo(1) # list(1)", linter) # comment in roxygen block ignored
  expect_no_lint("1+1 # gives 2", linter)
  expect_no_lint("# Non-existent:", linter)
  expect_no_lint("# 1-a", linter) # "-" removed from code operators
  expect_no_lint('1+1  # for example cat("123")', linter)

  # regression test for #451
  expect_no_lint("c('#a#' = 1)", linter)
})

test_that("commented_code_linter blocks disallowed usages", {
  lint_msg <- rex::rex("Remove commented code.")
  linter <- commented_code_linter()

  expect_lint("# blah <- 1", lint_msg, linter)

  expect_lint(
    "bleurgh <- fun_call(1) # other_call()",
    list(message = lint_msg, column_number = 26L),
    linter
  )

  expect_lint(
    " #blah <- 1",
    list(message = lint_msg, column_number = 3L),
    linter
  )

  # Regression test for #742, line number and comment number don't match up
  expect_lint(
    trim_some("
    # non-code comment
    line_without_comment <- 42L
     #blah <- 1
    "),
    list(message = lint_msg, line_number = 3L, column_number = 3L),
    linter
  )

  expect_lint(
    trim_some("
      d <- t.test(
        x = dplyr::starwars$height,
        # var.equal = TRUE,
        conf.level = .99,
        mu = 175
      )
    "),
    list(message = lint_msg, line_number = 3L),
    linter
  )

  expect_lint(
    trim_some("
      d <- t.test(
        x = dplyr::starwars$height
        #, var.equal = TRUE
        , conf.level = .99
        , mu = 175
      )
    "),
    list(message = lint_msg, line_number = 3L),
    linter
  )

  expect_lint("1+1 # cat('123')", lint_msg, linter)
  expect_lint("#expect_ftype(1e-12 , t)", lint_msg, linter)
})

test_that("commented_code_linter can detect operators in comments and lint correctly", {
  linter <- commented_code_linter()
  lint_msg <- rex::rex("Remove commented code.")

  test_ops <- c(
    "+", "=", "==", "!=", "<=", ">=", "<-", "<<-", "<", ">", "->",
    "->>", "%%", "/", "^", "*", "**", "|", "||", "&", "&&", "%>%",
    "%anything%"
  )

  for (op in test_ops) {
    expect_no_lint(paste("i", op, "1", collapse = ""), linter)
    expect_no_lint(paste("# something like i", op, "1", collapse = ""), linter)
    expect_lint(paste("# i", op, "1", collapse = ""), lint_msg, linter)
  }

  skip_if_not_r_version("4.1.0")

  expect_lint(
    "# 1:3 |> sum()",
    rex::rex("Remove commented code."),
    linter
  )
})

test_that("commented_code_linter can detect commented code ending with pipes", {
  linter <- commented_code_linter()
  lint_msg <- rex::rex("Remove commented code.")

  expect_lint("# f() %>%", lint_msg, linter)

  skip_if_not_r_version("4.1.0")
  expect_lint("# f() |>", lint_msg, linter)
})
