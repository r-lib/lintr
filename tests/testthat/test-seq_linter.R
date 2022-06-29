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
    rex("length(...)", anything, "Use seq_along"),
    linter
  )

  expect_lint(
    "function(x) { 1:nrow(x) }",
    rex("nrow(...)", anything, "Use seq_len"),
    linter
  )

  expect_lint(
    "function(x) { 1:ncol(x) }",
    rex("ncol(...)", anything, "Use seq_len"),
    linter
  )

  expect_lint(
    "function(x) { 1:NROW(x) }",
    rex("NROW(...)", anything, "Use seq_len"),
    linter
  )

  expect_lint(
    "function(x) { 1:NCOL(x) }",
    rex("NCOL(...)", anything, "Use seq_len"),
    linter
  )

  expect_lint(
    "function(x) { 1:dim(x)[1L] }",
    rex("dim(...)", anything, "Use seq_len"),
    linter
  )

  expect_lint(
    "function(x) { 1L:dim(x)[[1]] }",
    rex("dim(...)", anything, "Use seq_len"),
    linter
  )

  expect_lint(
    "function(x) { mutate(x, .id = 1:n()) }",
    rex("n() is", anything, "Use seq_len"),
    linter
  )

  expect_lint(
    "function(x) { x[, .id := 1:.N] }",
    rex(".N is", anything, "Use seq_len"),
    linter
  )

})

test_that("1L is also bad", {
  expect_lint(
    "function(x) { 1L:length(x) }",
    rex("1L:length(...)", anything, "Use seq_along"),
    seq_linter()
  )
})

test_that("reverse seq is ok", {
  expect_lint(
    "function(x) { length(x):1 }",
    rex("length(...):1", anything, "Use seq_along"),
    seq_linter()
  )
})

test_that("Message vectorization works for multiple lints", {
  expect_lint(
    "c(1:length(x), 1:nrow(y))",
    list(
      rex::rex("1:length(...)", anything, "seq_along()"),
      rex::rex("1:nrow(...)", anything, "seq_len()")
    ),
    seq_linter()
  )
})
