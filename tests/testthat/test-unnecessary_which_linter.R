test_that("unnecessary_which_linter ignores valid which usage", {
  linter <- unnecessary_which_linter()

  expect_lint("which.min(x)", NULL, linter)
  expect_lint("which.max(x)", NULL, linter)
  expect_lint("min(x)", NULL, linter)
  expect_lint("max(x)", NULL, linter)
  expect_lint("which(x > 0L)", NULL, linter)
  expect_lint("min(which(x > 0L))", NULL, linter)
  expect_lint("max(which(x > 0L))", NULL, linter)
  expect_lint("# which(x == min(x))", NULL, linter)

})

test_that("unnecessary_which_linter identifies which(x == min(x)) pattern", {
  linter <- unnecessary_which_linter()
  lint_msg <- rex("which.min(x) is more efficient than which(x == min(x))")

  expect_lint("which(x == min(x))", lint_msg, linter)
  expect_lint("result <- which(values == min(values))", lint_msg, linter)
})

test_that("unnecessary_which_linter identifies which(x == max(x)) pattern", {
  linter <- unnecessary_which_linter()
  lint_msg <- rex("which.max(x) is more efficient than which(x == max(x))")

  expect_lint("which(x == max(x))", lint_msg, linter)
  expect_lint("result <- which(values == max(values))", lint_msg, linter)
})

test_that("lints vectorize", {
  max_message <- rex::rex("which(x == max(x))")
  min_message <- rex::rex("which(x == min(x))")

  expect_lint(
    trim_some("{
      which.max(x)
      which(x == max(x))
      which.min(x)
      which(x == min(x))
    }"),
    list(
      list(max_message, line_number = 3L),
      list(min_message, line_number = 5L)
    ),
    unnecessary_which_linter()
  )
})
