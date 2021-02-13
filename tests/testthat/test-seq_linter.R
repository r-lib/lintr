test_that("other : expressions are fine", {
  linter <- seq_linter()
  expect_lint("function() { 1:10 }", NULL, linter)
  expect_lint("function(x) { 2:length(x) }", NULL, linter)
  expect_lint("function(x) { 1:(length(x) || 1) }", NULL, linter)
})

test_that("finds 1:length(...) expressions", {
  linter <- seq_linter()
  expect_lint(
    "function(x) { 1:length(x) }",
    rex("length(...)", anything, "use seq_len"),
    linter
  )

  expect_lint(
    "function(x) { 1:nrow(x) }",
    rex("nrow(...)", anything, "use seq_len"),
    linter
  )

  expect_lint(
    "function(x) { 1:ncol(x) }",
    rex("ncol(...)", anything, "use seq_len"),
    linter
  )

  expect_lint(
    "function(x) { 1:NROW(x) }",
    rex("NROW(...)", anything, "use seq_len"),
    linter
  )

  expect_lint(
    "function(x) { 1:NCOL(x) }",
    rex("NCOL(...)", anything, "use seq_len"),
    linter
  )

  expect_lint(
    "function(x) { 1:dim(x)[1L] }",
    rex("dim(...)", anything, "use seq_len"),
    linter
  )

  expect_lint(
    "function(x) { 1L:dim(x)[[1]] }",
    rex("dim(...)", anything, "use seq_len"),
    linter
  )
})

test_that("1L is also bad", {
  expect_lint(
    "function(x) { 1L:length(x) }",
    rex("1L:length(...)", anything, "use seq_len"),
    seq_linter()
  )
})

test_that("reverse seq is ok", {
  expect_lint(
    "function(x) { length(x):1 }",
    rex("length(...):1", anything, "use seq_len"),
    seq_linter()
  )
})
