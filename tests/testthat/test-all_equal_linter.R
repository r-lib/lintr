test_that("all_equal_linter() skips allowed usages", {
  linter <- all_equal_linter()

  # Only when used in if
  expect_no_lint("all.equal(a, b)", linter)

  expect_no_lint("if (isTRUE(all.equal(a, b))) message('equal')", linter)
})

test_that("all_equal_linter() blocks simple disallowed usages", {
  linter <- all_equal_linter()
  lint_message <- rex::rex("Wrap all.equal() in isTRUE()")

  expect_lint("if (all.equal(a, b)) message('equal')", lint_message, linter)
  expect_lint("!all.equal(a, b)", lint_message, linter)
  expect_lint("while (all.equal(a, b)) message('equal')", lint_message, linter)
})

test_that("lints vectorize", {
  lint_message <- rex::rex("Wrap all.equal() in isTRUE()")

  expect_lint(
    trim_some("{
      all.equal(a, b)
      !all.equal(a, b)
      !isTRUE(all.equal(a, b))
      if (all.equal(a, b)) message('equal')
    }"),
    list(
      list(lint_message, line_number = 3L),
      list(lint_message, line_number = 5L)
    ),
    all_equal_linter()
  )
})
