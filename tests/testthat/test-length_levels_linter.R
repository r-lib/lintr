test_that("length_levels_linter skips allowed usages", {
  expect_no_lint("length(c(levels(x), 'a'))", length_levels_linter())
})

test_that("length_levels_linter blocks simple disallowed usages", {
  expect_lint(
    "2:length(levels(x))",
    rex::rex("nlevels(x) is better than length(levels(x))."),
    length_levels_linter()
  )
})

test_that("lints vectorize", {
  lint_msg <- rex::rex("nlevels(x) is better than length(levels(x)).")

  expect_lint(
    trim_some("{
      length(levels(x))
      length(levels(y))
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    length_levels_linter()
  )
})
