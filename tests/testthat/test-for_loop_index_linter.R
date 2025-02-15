test_that("for_loop_index_linter skips allowed usages", {
  linter <- for_loop_index_linter()

  expect_lint("for (xi in x) {}", NULL, linter)

  # this is OK, so not every symbol is problematic
  expect_lint("for (col in DF$col) {}", NULL, linter)
  expect_lint("for (col in S4@col) {}", NULL, linter)
  expect_lint("for (col in DT[, col]) {}", NULL, linter)

  # make sure symbol check is scoped
  expect_lint(
    trim_some("
      {
        for (i in 1:10) {
          42L
        }
        i <- 7L
      }
    "),
    NULL,
    linter
  )
})

test_that("for_loop_index_linter blocks simple disallowed usages", {
  linter <- for_loop_index_linter()
  lint_msg <- "Don't re-use any sequence symbols as the index symbol in a for loop"

  expect_lint("for (x in x) {}", lint_msg, linter)
  # these also overwrite a variable in calling scope
  expect_lint("for (x in foo(x)) {}", lint_msg, linter)
  # arbitrary nesting
  expect_lint("for (x in foo(bar(y, baz(2, x)))) {}", lint_msg, linter)
})

test_that("lints vectorize", {
  lint_msg <- "Don't re-use any sequence symbols as the index symbol in a for loop"

  expect_lint(
    trim_some("{
      for (x in x) {}
      for (y in y) {}
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    for_loop_index_linter()
  )
})
