test_that("returns the correct linting", {
  linter <- todo_comment_linter()
  lint_msg <- rex::rex("Remove TODO comments.")

  expect_no_lint('a <- "you#need#to#fixme"', linter)
  expect_no_lint("# something todo", linter)
  expect_lint(
    "cat(x) ### fixme",
    list(message = lint_msg, line_number = 1L, column_number = 8L),
    linter
  )
  expect_lint(
    "x <- \"1.0\n2.0 #FIXME\n3 #TODO 4\"; y <- 2; z <- 3 # todo later",
    list(message = lint_msg, line_number = 3L, column_number = 28L),
    linter
  )
  expect_lint(
    trim_some("
      function() {
        # TODO
        function() {
          # fixme
        }
      }
    "),
    list(
      list(message = lint_msg, line_number = 2L, column_number = 3L),
      list(message = lint_msg, line_number = 4L, column_number = 5L)
    ),
    linter
  )
})

test_that("except_regex= excludes valid TODO", {
  linter <- todo_comment_linter(except_regex = "TODO\\(#[0-9]+\\):")
  lint_msg <- rex::rex("Remove TODO comments.")

  expect_no_lint("foo() # TODO(#1234): Deprecate foo.", linter)
  # Non-excepted lints
  expect_lint(
    trim_some("
      foo() # TODO()
      bar() # TODO(#567): Deprecate bar.
    "),
    list(lint_msg, line_number = 1L),
    linter
  )
  # Only TODO() is excepted
  mixed_lines <- trim_some("
    foo() # TODO(#1234): Deprecate foo.
    bar() # fixme(#567): Deprecate bar.
  ")

  expect_lint(mixed_lines, list(lint_msg, line_number = 2L), linter)
  expect_no_lint(
    mixed_lines,
    todo_comment_linter(except_regex = c("TODO\\(#[0-9]+\\):", "fixme\\(#[0-9]+\\):"))
  )

  # ignore captured groups
  expect_no_lint(
    "# TODO(a)",
    todo_comment_linter(except_regex = "TODO\\((a|abc)\\)")
  )
})
