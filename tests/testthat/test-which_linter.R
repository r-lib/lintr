test_that("which_linter skips allowed usages", {
  # this _could_ be combined as p1|p2, but often it's cleaner to read this way
  expect_lint("which(grepl(p1, x) | grepl(p2, x))", NULL, which_linter())
})

test_that("which_linter blocks simple disallowed usages", {
  linter <- which_linter()
  lint_msg <- rex::rex("grep(pattern, x) is better than which(grepl(pattern, x)).")

  expect_lint("which(grepl('^a', x))", lint_msg, linter)
  # options also don't matter (grep has more arguments: value, invert)
  expect_lint("which(grepl('^a', x, perl = TRUE, fixed = TRUE))", lint_msg, linter)

  expect_lint(
    trim_some('{
      which(x)
      grepl(y)
      which(grepl("pt1", x))
      which(grepl("pt2", y))
    }'),
    list(
      list(lint_msg, line_number = 4L, column_number = 3L),
      list(lint_msg, line_number = 5L, column_number = 3L)
    ),
    linter
  )
})

test_that("which_linter ignores valid which min/max usages", {
  linter <- which_linter()

  expect_lint("which.min(x)", NULL, linter)
  expect_lint("which.max(x)", NULL, linter)
  expect_lint("min(x)", NULL, linter)
  expect_lint("max(x)", NULL, linter)
  expect_lint("which(x > 0L)", NULL, linter)
  expect_lint("min(which(x > 0L))", NULL, linter)
  expect_lint("max(which(x > 0L))", NULL, linter)
  expect_lint("# which(x == min(x))", NULL, linter)
})

test_that("which_linter identifies which(x == min(x)) pattern", {
  linter <- which_linter()
  lint_msg <- rex("which.min(x) is more efficient than which(x == min(x))")

  expect_lint("which(x == min(x))", lint_msg, linter)
  expect_lint("result <- which(values == min(values))", lint_msg, linter)
})

test_that("which_linter identifies which(x == max(x)) pattern", {
  linter <- which_linter()
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
    which_linter()
  )
})
