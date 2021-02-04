test_that("returns the correct linting", {
  linter <- todo_comment_linter(todo = c("todo", "fixme"))
  msg <- "TODO comments should be removed."

  expect_lint("a <- \"you#need#to#fixme\"", NULL, linter)
  expect_lint("# something todo", NULL, linter)
  expect_lint("cat(x) ### fixme",
              list(message = msg, line_number = 1L, column_number = 8L),
              linter)
  expect_lint("x <- \"1.0\n2.0 #FIXME\n3 #TODO 4\"; y <- 2; z <- 3 # todo later",
              list(message = msg, line_number = 3L, column_number = 28L),
              linter)
  expect_lint("function() {\n# TODO\n  function() {\n  # fixme\n  }\n}",
              list(
                list(message = msg, line_number = 2L, column_number = 1L),
                list(message = msg, line_number = 4L, column_number = 3L)
              ), linter)
})
