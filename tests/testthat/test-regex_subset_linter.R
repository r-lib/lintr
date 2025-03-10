test_that("regex_subset_linter skips allowed usages", {
  expect_lint("y[grepl(ptn, x)]", NULL, regex_subset_linter())
  expect_lint("x[grepl(ptn, foo(x))]", NULL, regex_subset_linter())
})

test_that("regex_subset_linter blocks simple disallowed usages", {
  linter <- regex_subset_linter()
  lint_msg <- rex::rex("Prefer grep(pattern, x, ..., value = TRUE)")

  expect_lint("x[grep(ptn, x)]", lint_msg, linter)
  expect_lint("names(y)[grepl(ptn, names(y), perl = TRUE)]", lint_msg, linter)
  expect_lint("names(foo(y))[grepl(ptn, names(foo(y)), fixed = TRUE)]", lint_msg, linter)

  # adversarial commenting
  expect_lint(
    trim_some("
      names(y #comment
      )[grepl(ptn, names(y), perl = TRUE)]
    "),
    lint_msg,
    linter
  )
})

test_that("regex_subset_linter skips grep/grepl subassignment", {
  linter <- regex_subset_linter()

  expect_no_lint("x[grep(ptn, x)] <- ''", linter)
  expect_no_lint("x[grepl(ptn, x)] <- ''", linter)
  expect_no_lint("x[grep(ptn, x, perl = TRUE)] = ''", linter)
  expect_no_lint("'' -> x[grep(ptn, x, ignore.case = TRUE)] = ''", linter)

  expect_no_lint(
    trim_some("
      x[grepl(ptn, x) # comment
      ] <- ''
    "),
    linter
  )
})

test_that("regex_subset_linter skips allowed usages for stringr equivalents", {
  linter <- regex_subset_linter()

  expect_lint("y[str_detect(x, ptn)]", NULL, linter)
  expect_lint("x[str_detect(foo(x), ptn)]", NULL, linter)
  expect_lint("x[str_detect(x, ptn)] <- ''", NULL, linter)
  expect_lint("x[str_detect(x, ptn)] <- ''", NULL, linter)
})

test_that("regex_subset_linter blocks disallowed usages for stringr equivalents", {
  linter <- regex_subset_linter()
  lint_msg <- rex::rex("Prefer stringr::str_subset(x, pattern) over")

  expect_lint("x[str_which(x, ptn)]", lint_msg, linter)
  expect_lint("names(y)[str_detect(names(y), ptn, negate = TRUE)]", lint_msg, linter)
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      x[grep(ptn, x)]
      y[str_detect(y, ptn)]
    }"),
    list(
      list(rex::rex("Prefer grep"), line_number = 2L),
      list(rex::rex("Prefer stringr::str_subset"), line_number = 3L)
    ),
    regex_subset_linter()
  )
})
