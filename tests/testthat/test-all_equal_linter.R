test_that("all_equal_linter() skips allowed usages", {
  linter <- all_equal_linter()

  # Only when used in if
  expect_no_lint("all.equal(a, b)", linter)

  expect_no_lint("if (isTRUE(all.equal(a, b))) message('equal')", linter)
})

test_that("all_equal_linter() blocks simple disallowed usages", {
  linter <- all_equal_linter()
  lint_message <- rex::rex(anything)

  expect_lint("if (all.equal(a, b)) message('equal')", lint_message, linter)
  expect_lint("!all.equal(a, b)", lint_message, linter)
})
