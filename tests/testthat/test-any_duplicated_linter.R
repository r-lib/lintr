test_that("any_duplicated_linter skips allowed usages", {
  linter <- any_duplicated_linter()

  expect_no_lint("x <- any(y)", linter)
  expect_no_lint("y <- duplicated(z)", linter)

  # extended usage of any is not covered
  expect_no_lint("any(duplicated(y), b)", linter)
  expect_no_lint("any(b, duplicated(y))", linter)
})

test_that("any_duplicated_linter blocks simple disallowed usages", {
  linter <- any_duplicated_linter()
  lint_msg <- rex::rex("anyDuplicated(x, ...) > 0 is better")

  expect_lint("any(duplicated(x))", lint_msg, linter)
  expect_lint("any(duplicated(foo(x)))", lint_msg, linter)
  # na.rm doesn't really matter for this since duplicated can't return NA
  expect_lint("any(duplicated(x), na.rm = TRUE)", lint_msg, linter)
  # also catch nested usage
  expect_lint("foo(any(duplicated(x)))", lint_msg, linter)
})

test_that("any_duplicated_linter catches length(unique()) equivalencies too", {
  linter <- any_duplicated_linter()
  lint_msg_x <- rex::rex("anyDuplicated(x) == 0L is better than length(unique(x)) == length(x)")
  lint_msg_df <- rex::rex("anyDuplicated(DF$col) == 0L is better than length(unique(DF$col)) == nrow(DF)")

  # non-matches
  ## different variable
  expect_no_lint("length(unique(x)) == length(y)", linter)
  ## different table
  expect_no_lint("length(unique(DF$x)) == nrow(DT)", linter)
  expect_no_lint("length(unique(l1$DF$x)) == nrow(l2$DF)", linter)

  # lintable usage
  expect_lint("length(unique(x)) == length(x)", lint_msg_x, linter)
  # argument order doesn't matter
  expect_lint("length(x) == length(unique(x))", lint_msg_x, linter)
  # nrow-style equivalency
  expect_lint("nrow(DF) == length(unique(DF$col))", lint_msg_df, linter)
  expect_lint("nrow(DF) == length(unique(DF[['col']]))", lint_msg_df, linter)
  # match with nesting too
  expect_lint("nrow(l$DF) == length(unique(l$DF[['col']]))", lint_msg_df, linter)

  # !=, <, and > usages are all alternative ways of writing a test for dupes
  #   technically, the direction of > / < matter, but writing
  #   length(unique(x)) > length(x) doesn't seem like it would ever happen.
  expect_lint("length(unique(x)) != length(x)", lint_msg_x, linter)
  expect_lint("length(unique(x)) < length(x)", lint_msg_x, linter)
  expect_lint("length(x) > length(unique(x))", lint_msg_x, linter)
})

test_that("dplyr & data.table equivalents are also linted", {
  linter <- any_duplicated_linter()

  expect_no_lint("uniqueN(x) == nrow(y)", linter)
  expect_no_lint("n_distinct(x) == nrow(y)", linter)

  expect_lint("uniqueN(x) == nrow(x)", "xxx", linter)
  expect_lint("data.table::uniqueN(x) == nrow(x)", "xxx", linter)
  expect_lint("x[, length(unique(col)) == .N]", "xxx", linter)
  expect_lint("x[, uniqueN(col) == .N]", "xxx", linter)

  expect_lint("n_distinct(x) == nrow(x)", "xxx", linter)
  expect_lint("dplyr::n_distinct(x) == nrow(x)", "xxx", linter)
  expect_lint("x %>% summarize(length(unique(col)) == n())", "xxx", linter)
  expect_lint("x %>% summarize(n_distinct(col) == n())", "xxx", linter)
})

test_that("any_duplicated_linter catches expression with two types of lint", {
  linter <- any_duplicated_linter()
  lint_msg <- rex::rex("anyDuplicated(DF$col) == 0L is better than length(unique(DF$col)) == nrow(DF)")

  expect_lint(
    trim_some("{
      any(duplicated(x))
      length(unique(DF$col)) == nrow(DF)
    }"),
    list(
      list(rex::rex("anyDuplicated(x, ...) > 0 is better"), line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    linter
  )

  # ditto for different messages within the length(unique()) tests
  expect_lint(
    trim_some("{
      length(unique(x)) == length(x)
      length(unique(DF$col)) == nrow(DF)
    }"),
    list(
      list(rex::rex("anyDuplicated(x) == 0L is better than length(unique(x)) == length(x)"), line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    linter
  )
})
