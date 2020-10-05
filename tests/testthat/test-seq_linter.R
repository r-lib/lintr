context("seq_linter")

test_that("other : expressions are fine", {
  expect_lint("function() { 1:10 }", NULL, seq_linter)
  expect_lint("function(x) { 2:length(x) }", NULL, seq_linter)
  expect_lint("function(x) { 1:(length(x) || 1) }", NULL, seq_linter)
})

test_that("finds 1:length(...) expressions", {
  expect_lint("function(x) { 1:length(x) }",
    rex("length(...)", anything, "use seq_len"),
    seq_linter)

  expect_lint("function(x) { 1:nrow(x) }",
    rex("length(...)", anything, "use seq_len"),
    seq_linter)

  expect_lint("function(x) { 1:ncol(x) }",
    rex("length(...)", anything, "use seq_len"),
    seq_linter)

  expect_lint("function(x) { 1:NROW(x) }",
    rex("length(...)", anything, "use seq_len"),
    seq_linter)

  expect_lint("function(x) { 1:NCOL(x) }",
    rex("length(...)", anything, "use seq_len"),
    seq_linter)

  expect_lint("function(x) { 1:dim(x)[1L] }",
    rex("length(...)", anything, "use seq_len"),
    seq_linter)
  
  expect_lint("function(x) { 1L:dim(x)[[1]] }",
    rex("length(...)", anything, "use seq_len"),
    seq_linter)
})

test_that("1L is also bad", {
  expect_lint("function(x) { 1L:length(x) }",
    rex("length(...)", anything, "use seq_len"),
    seq_linter)
})

test_that("reverse seq is ok", {
  expect_lint("function(x) { length(x):1 }",
    rex("length(...)", anything, "use seq_len"),
    seq_linter)
})
