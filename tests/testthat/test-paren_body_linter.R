# nofuzz start
test_that("paren_body_linter returns correct lints", {
  linter <- paren_body_linter()
  lint_msg <- rex::rex("Put a space between a right parenthesis and a body expression.")

  # No space after the closing parenthesis prompts a lint
  expect_lint("function()test", lint_msg, linter)
  expect_lint("print('hello')\nx <- function(x)NULL\nprint('hello')", lint_msg, linter)
  expect_lint("if (TRUE)test", lint_msg, linter)
  expect_lint("while (TRUE)test", lint_msg, linter)
  expect_lint("for (i in seq_along(1))test", lint_msg, linter)

  # A space after the closing parenthesis does not prompt a lint
  expect_no_lint("function() test", linter)

  # Symbols after the closing parenthesis of a function call do not prompt a lint
  expect_no_lint("head(mtcars)$cyl", linter)

  # paren_body_linter returns the correct line number
  expect_lint(
    "print('hello')\nx <- function(x)NULL\nprint('hello')",
    list(line_number = 2L),
    linter
  )

  expect_lint(
    "function()test",
    list(
      line_number = 1L,
      column_number = 10L,
      type = "style",
      line = "function()test",
      ranges = list(c(10L, 10L))
    ),
    linter
  )

  expect_lint(
    "for (ii in 1:10)ii",
    list(column_number = 16L, ranges = list(c(16L, 16L))),
    linter
  )

  # paren_body_linter does not lint when the function body is defined on a new line
  expect_no_lint("function()\n  test", linter)

  # paren_body_linter does not lint comments
  expect_no_lint("#function()test", linter)

  # multiple lints on the same line
  expect_lint("function()if(TRUE)while(TRUE)test", list(lint_msg, lint_msg, lint_msg), linter)

  # No space after the closing parenthesis of an anonymous function prompts a lint
  expect_lint("\\()test", lint_msg, linter)
})

test_that("multi-line versions are caught", {
  linter <- paren_body_linter()
  lint_msg <- rex::rex("Put a space between a right parenthesis and a body expression.")

  expect_lint(
    trim_some("
      function(var
                  )x
    "),
    lint_msg,
    linter
  )
  expect_lint(
    trim_some("
      if (cond
              )x
    "),
    lint_msg,
    linter
  )
  expect_lint(
    trim_some("
      while (cond
                 )x
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      \\(var
            )x
    "),
    lint_msg,
    linter
  )
})

test_that("function shorthand is handled", {
  linter <- paren_body_linter()
  lint_msg <- rex::rex("Put a space between a right parenthesis and a body expression.")

  expect_lint("\\()test", lint_msg, linter)
})
# nofuzz end
