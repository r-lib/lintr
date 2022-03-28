test_that("regex_subset_linter skips allowed usages", {
  expect_lint("y[grepl(ptn, x)]", NULL, regex_subset_linter())
  expect_lint("x[grepl(ptn, foo(x))]", NULL, regex_subset_linter())
})

test_that("regex_subset_linter blocks simple disallowed usages", {
  expect_lint(
    "x[grep(ptn, x)]",
    rex::rex("Prefer grep(pattern, x, ..., value = TRUE)"),
    regex_subset_linter()
  )

  expect_lint(
    "names(y)[grepl(ptn, names(y), perl = TRUE)]",
    rex::rex("Prefer grep(pattern, x, ..., value = TRUE)"),
    regex_subset_linter()
  )

  expect_lint(
    "names(foo(y))[grepl(ptn, names(foo(y)), fixed = TRUE)]",
    rex::rex("Prefer grep(pattern, x, ..., value = TRUE)"),
    regex_subset_linter()
  )
})

test_that("regex_subset_linter skips grep/grepl subassignment", {
  expect_lint("x[grep(ptn, x)] <- ''", NULL, regex_subset_linter())
  expect_lint("x[grepl(ptn, x)] <- ''", NULL, regex_subset_linter())
  expect_lint("x[grep(ptn, x, perl = TRUE)] = ''", NULL, regex_subset_linter())
  expect_lint("'' -> x[grep(ptn, x, ignore.case = TRUE)] = ''", NULL, regex_subset_linter())
})

test_that("regex_subset_linter works for stringr equivalents", {
  expect_lint("y[str_detect(x, ptn)]", NULL, regex_subset_linter())
  expect_lint("x[str_detect(foo(x), ptn)]", NULL, regex_subset_linter())

  expect_lint(
    "x[str_which(x, ptn)]",
    rex::rex("Prefer stringr::str_subset(x, pattern) over"),
    regex_subset_linter()
  )

  expect_lint(
    "names(y)[str_detect(names(y), ptn, negate = TRUE)]",
    rex::rex("Prefer stringr::str_subset(x, pattern) over"),
    regex_subset_linter()
  )
  expect_lint("x[str_detect(x, ptn)] <- ''", NULL, regex_subset_linter())
  expect_lint("x[str_detect(x, ptn)] <- ''", NULL, regex_subset_linter())
})
