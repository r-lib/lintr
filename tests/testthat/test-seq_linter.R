test_that("other : expressions are fine", {
  linter <- seq_linter()
  expect_lint("function() { 1:10 }", NULL, linter)
  expect_lint("function(x) { 2:length(x) }", NULL, linter)
  expect_lint("function(x) { 1:(length(x) || 1) }", NULL, linter)
})

test_that("seq_len(...) or seq_along(...) expressions are fine", {
  linter <- seq_linter()

  expect_lint("function(x) { seq_len(x) }", NULL, linter)
  expect_lint("function(x) { seq_along(x) }", NULL, linter)

  expect_lint("function(x) { seq(2, length(x)) }", NULL, linter)
  expect_lint("function(x) { seq(length(x), 2) }", NULL, linter)
})

test_that("finds seq(...) expressions", {
  linter <- seq_linter()

  expect_lint(
    "function(x) { seq(length(x)) }",
    rex::rex("seq(length(...))", anything, "Use seq_along(...)"),
    linter
  )

  expect_lint(
    "function(x) { seq(nrow(x)) }",
    rex::rex("seq(nrow(...))", anything, "Use seq_len(nrow(...))"),
    linter
  )

  expect_lint(
    "function(x) { rev(seq(length(x))) }",
    rex::rex("seq(length(...))", anything, "Use seq_along(...)"),
    linter
  )

  expect_lint(
    "function(x) { rev(seq(nrow(x))) }",
    rex::rex("seq(nrow(...))", anything, "Use seq_len(nrow(...))"),
    linter
  )
})

test_that("finds 1:length(...) expressions", {
  linter <- seq_linter()

  expect_lint(
    "function(x) { 1:length(x) }",
    rex::rex("length(...)", anything, "Use seq_along"),
    linter
  )

  expect_lint(
    "function(x) { 1:nrow(x) }",
    rex::rex("nrow(...)", anything, "Use seq_len"),
    linter
  )

  expect_lint(
    "function(x) { 1:ncol(x) }",
    rex::rex("ncol(...)", anything, "Use seq_len"),
    linter
  )

  expect_lint(
    "function(x) { 1:NROW(x) }",
    rex::rex("Use seq_len(NROW(...)) instead of 1:NROW(...)"),
    linter
  )

  expect_lint(
    "function(x) { 1:NCOL(x) }",
    rex::rex("Use seq_len(NCOL(...)) instead of 1:NCOL(...)"),
    linter
  )

  expect_lint(
    "function(x) { 1:dim(x)[1L] }",
    rex::rex("Use seq_len(dim(...)[1L]) instead of 1:dim(...)[1L]"),
    linter
  )

  expect_lint(
    "function(x) { 1L:dim(x)[[1]] }",
    rex::rex("Use seq_len", anything, "dim(...)"),
    linter
  )

  expect_lint(
    "function(x) { mutate(x, .id = 1:n()) }",
    rex::rex("Use seq_len(n()) instead of 1:n(),"),
    linter
  )

  expect_lint(
    "function(x) { x[, .id := 1:.N] }",
    rex::rex("Use seq_len(.N) instead of 1:.N,"),
    linter
  )
})

test_that("1L is also bad", {
  expect_lint(
    "function(x) { 1L:length(x) }",
    rex::rex("seq_along", anything, "1L:length(...)"),
    seq_linter()
  )
})

test_that("reverse seq is ok", {
  linter <- seq_linter()
  expect_lint("function(x) { rev(seq_along(x)) }", NULL, linter)
  expect_lint("function(x) { rev(seq_len(nrow(x))) }", NULL, linter)

  expect_lint(
    "function(x) { length(x):1 }",
    rex::rex("rev(seq_along(...))", anything, "length(...):1"),
    seq_linter()
  )
})

test_that("Message vectorization works for multiple lints", {
  expect_lint(
    "c(1:length(x), 1:nrow(y))",
    list(
      rex::rex("seq_along(...)", anything, "1:length(...)"),
      rex::rex("seq_len(nrow(...))", anything, "1:nrow(...)")
    ),
    seq_linter()
  )

  expect_lint(
    "c(seq(length(x)), 1:nrow(y))",
    list(
      rex::rex("seq_along(...)", anything, "seq(length(...))"),
      rex::rex("seq_len(nrow(...))", anything, "1:nrow(...)")
    ),
    seq_linter()
  )

  expect_lint(
    "c(seq(length(x)), seq(nrow(y)))",
    list(
      rex::rex("seq_along(...)", anything, "seq(length(...))"),
      rex::rex("seq_len(nrow(...))", anything, "seq(nrow(...))")
    ),
    seq_linter()
  )

  expect_lint(
    "c(1:NROW(x), seq(NCOL(y)))",
    list(
      rex::rex("seq_len(NROW(...))", anything, "1:NROW(...)"),
      rex::rex("seq_len(NCOL(...))", anything, "seq(NCOL(...))")
    ),
    seq_linter()
  )
})

test_that("Message recommends rev() correctly", {
  linter <- seq_linter()

  expect_lint(".N:1", rex::rex("Use rev(seq_len(.N))"), linter)
  expect_lint("n():1", rex::rex("Use rev(seq_len(n()))"), linter)
  expect_lint("nrow(x):1", rex::rex("Use rev(seq_len(nrow(...)))"), linter)
  expect_lint("length(x):1", rex::rex("Use rev(seq_along(...))"), linter)
})
