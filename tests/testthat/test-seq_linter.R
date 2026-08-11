test_that("other : expressions are fine", {
  linter <- seq_linter()
  expect_no_lint("1:10", linter)
  expect_no_lint("2:length(x)", linter)
  expect_no_lint("1:(length(x) || 1)", linter)
})

test_that("seq_len(...) or seq_along(...) expressions are fine", {
  linter <- seq_linter()

  expect_no_lint("seq_len(x)", linter)
  expect_no_lint("seq_along(x)", linter)

  expect_no_lint("seq(2, length(x))", linter)
  expect_no_lint("seq(length(x), 2)", linter)
})

test_that("finds seq(...) expressions", {
  linter <- seq_linter()
  lint_msg <- function(want, got) rex::rex("Use ", want, " instead of ", got)

  expect_lint(
    "seq(length(x))",
    lint_msg("seq_along(...)", "seq(length(...))"),
    linter
  )

  expect_lint(
    "seq(nrow(x))",
    lint_msg("seq_len(nrow(...))", "seq(nrow(...))"),
    linter
  )

  expect_lint(
    "rev(seq(length(x)))",
    lint_msg("seq_along(...)", "seq(length(...))"),
    linter
  )

  expect_lint(
    "rev(seq(nrow(x)))",
    lint_msg("seq_len(nrow(...))", "seq(nrow(...))"),
    linter
  )
})

test_that("finds 1:length(...) expressions", {
  linter <- seq_linter()
  lint_msg <- function(want, got) rex::rex("Use ", want, " instead of ", got)

  expect_lint(
    "1:length(x)",
    lint_msg("seq_along(...)", "1:length(...)"),
    linter
  )

  expect_lint(
    "1:nrow(x)",
    lint_msg("seq_len(nrow(...))", "1:nrow(...)"),
    linter
  )

  expect_lint(
    "1:ncol(x)",
    lint_msg("seq_len(ncol(...))", "1:ncol(...)"),
    linter
  )

  expect_lint(
    "1:NROW(x)",
    lint_msg("seq_len(NROW(...))", "1:NROW(...)"),
    linter
  )

  expect_lint(
    "1:NCOL(x)",
    lint_msg("seq_len(NCOL(...))", "1:NCOL(...)"),
    linter
  )

  expect_lint(
    "1:dim(x)[1L]",
    lint_msg("seq_len(dim(...)[1L])", "1:dim(...)[1L]"),
    linter
  )

  expect_lint(
    "1L:dim(x)[[1]]",
    rex::rex("Use seq_len", anything, "dim(...)"),
    linter
  )

  expect_lint(
    "mutate(x, .id = 1:n())",
    lint_msg("seq_len(n())", "1:n()"),
    linter
  )

  expect_lint(
    "mutate(x, .id = 1:dplyr::n())",
    lint_msg("seq_len(dplyr::n())", "1:dplyr::n()"),
    linter
  )

  expect_lint(
    trim_some("
      mutate(x, .id = 1:n( # comment
      ))
    "),
    lint_msg("seq_len(n())", "1:n()"),
    linter
  )

  expect_lint(
    "x[, .id := 1:.N]",
    lint_msg("seq_len(.N)", "1:.N"),
    linter
  )
})

test_that("1L is also bad", {
  expect_lint(
    "1L:length(x)",
    rex::rex("seq_along", anything, "1L:length(...)"),
    seq_linter()
  )
})

test_that("reverse seq is ok", {
  linter <- seq_linter()
  expect_no_lint("rev(seq_along(x))", linter)
  expect_no_lint("rev(seq_len(nrow(x)))", linter)

  expect_lint(
    "length(x):1",
    rex::rex("rev(seq_along(...))", anything, "length(...):1"),
    linter
  )
})

test_that("finds potential sequence() replacements", {
  linter <- seq_linter()
  lint_msg <- rex::rex("Use sequence()")

  expect_lint("unlist(lapply(x, seq_len))", lint_msg, linter)

  expect_lint("unlist(lapply(x, seq))", lint_msg, linter)

  # Even for prefixed purrr:: calls
  expect_lint("unlist(purrr::map(x, seq_len))", lint_msg, linter)
})

test_that("sequence() is not recommended for complex seq() calls", {
  linter <- seq_linter()

  expect_no_lint("unlist(lapply(x, seq, from = 2))", linter)
})

test_that("Message vectorization works for multiple lints", {
  linter <- seq_linter()

  expect_lint(
    trim_some("{
      1:length(x)
      1:nrow(y)
      seq(1, 10)
      seq(to = 1, from = 10)
    }"),
    list(
      list(rex::rex("seq_along(...)", anything, "1:length(...)"), line_number = 2L),
      list(rex::rex("seq_len(nrow(...))", anything, "1:nrow(...)"), line_number = 3L),
      list(rex::rex("seq_len(10)", anything, "seq(1, 10)"), line_number = 4L),
      list(rex::rex("seq_len(nrow(...))", anything, "1:nrow(...)"), line_number = 5L)
    ),
    linter
  )

  expect_lint(
    trim_some("{
      seq(length(x))
      1:nrow(y)
    }"),
    list(
      list(rex::rex("seq_along(...)", anything, "seq(length(...))"), line_number = 2L),
      list(rex::rex("seq_len(nrow(...))", anything, "1:nrow(...)"), line_number = 3L)
    ),
    linter
  )

  expect_lint(
    trim_some("{
      seq(length(x))
      seq(nrow(y))
    }"),
    list(
      list(rex::rex("seq_along(...)", anything, "seq(length(...))"), line_number = 2L),
      list(rex::rex("seq_len(nrow(...))", anything, "seq(nrow(...))"), line_number = 3L)
    ),
    linter
  )

  expect_lint(
    trim_some("{
      1:NROW(x)
      seq(NCOL(y))
    }"),
    list(
      list(rex::rex("seq_len(NROW(...))", anything, "1:NROW(...)"), line_number = 2L),
      list(rex::rex("seq_len(NCOL(...))", anything, "seq(NCOL(...))"), line_number = 3L)
    ),
    linter
  )

  expect_lint(
    trim_some("{
      1:NROW(x)
      unlist(lapply(y, seq_len))
    }"),
    list(
      list(rex::rex("seq_len(NROW(...))", anything, "1:NROW(...)"), line_number = 2L),
      list(rex::rex("sequence()"), line_number = 3L)
    ),
    linter
  )
})

test_that("Message recommends rev() correctly", {
  linter <- seq_linter()

  expect_lint(".N:1", rex::rex("Use rev(seq_len(.N))"), linter)
  expect_lint("n():1", rex::rex("Use rev(seq_len(n()))"), linter)
  expect_lint("dplyr::n():1", rex::rex("Use rev(seq_len(dplyr::n()))"), linter)
  expect_lint("nrow(x):1", rex::rex("Use rev(seq_len(nrow(...)))"), linter)
  expect_lint("length(x):1", rex::rex("Use rev(seq_along(...))"), linter)
})

test_that("seq_len(length(x)) should be seq_along(x)", {
  linter <- seq_linter()

  expect_no_lint("seq_len(length(x) - 1)", linter)
  expect_no_lint("seq_len(2*length(x))", linter)
  expect_no_lint("seq_len(foo(length(x)))", linter)
  expect_lint("seq_len(length(x))", "seq_along", linter)
  expect_lint("seq_len(length(foo(x)))", "seq_along", linter)
})

test_that("finds seq(1, n) and seq(from = 1, to = n) expressions", {
  linter <- seq_linter()
  lint_msg <- function(want, got) rex::rex("Use ", want, " instead of ", got)

  expect_lint("seq(1, 10)",                     lint_msg("seq_len(10)",        "seq(1, 10)"), linter)
  expect_lint("seq(1, 1)",                      lint_msg("seq_len(1)",         "seq(1, 1)"), linter)
  expect_lint("base::seq(1, 10)",               lint_msg("seq_len(10)",        "seq(1, 10)"), linter)
  expect_lint("seq(1L, 10L)",                   lint_msg("seq_len(10L)",       "seq(1L, 10L)"), linter)
  expect_lint("seq(1, n)",                      lint_msg("seq_len(n)",         "seq(1, n)"), linter)
  expect_lint("seq(1, length(x))",              lint_msg("seq_along(...)",     "seq(1, length(...))"), linter)
  expect_lint("seq(1, nrow(x))",                lint_msg("seq_len(nrow(...))", "seq(1, nrow(...))"), linter)
  expect_lint("seq(from = 1, to = 10)",         lint_msg("seq_len(10)",        "seq(1, 10)"), linter)
  expect_lint("seq(from = 1L, to = length(x))", lint_msg("seq_along(...)",     "seq(1L, length(...))"), linter)
  expect_lint("seq(to = 10, from = 1)",         lint_msg("seq_len(10)",        "seq(1, 10)"), linter)
  expect_lint("seq(to = length(x), from = 1L)", lint_msg("seq_along(...)",     "seq(1L, length(...))"), linter)
  expect_lint("seq(1, to = 10)",                lint_msg("seq_len(10)",        "seq(1, 10)"), linter)
  expect_lint("seq(to = 10, 1)",                lint_msg("seq_len(10)",        "seq(1, 10)"), linter)
  expect_lint("seq(to = 10, 1L)",               lint_msg("seq_len(10)",        "seq(1L, 10)"), linter)
  expect_lint("seq(10, from = 1)",              lint_msg("seq_len(10)",        "seq(1, 10)"), linter)
  expect_lint("seq(10, from = 1L)",             lint_msg("seq_len(10)",        "seq(1L, 10)"), linter)
  expect_lint("seq(from = 1, 10)",              lint_msg("seq_len(10)",        "seq(1, 10)"), linter)
  expect_lint("seq(from = 1L, 10)",             lint_msg("seq_len(10)",        "seq(1L, 10)"), linter)
  expect_lint("seq(length(x), from = 1)",       lint_msg("seq_along(...)",     "seq(1, length(...))"), linter)
  expect_lint("seq(length(x), from = 1L)",      lint_msg("seq_along(...)",     "seq(1L, length(...))"), linter)
  expect_lint("seq(from = 1, length(x))",       lint_msg("seq_along(...)",     "seq(1, length(...))"), linter)
  expect_lint("seq(to = length(x), 1)",         lint_msg("seq_along(...)",     "seq(1, length(...))"), linter)

  # Complex expressions or other arguments
  expect_no_lint("seq(0, 10)", linter)
  expect_no_lint("seq(2, 10)", linter)
  expect_no_lint("seq(10, from = 2)", linter)
  expect_no_lint("seq(from = 2, 10)", linter)
  expect_no_lint("seq(1, 10, by = 2)", linter)
  expect_no_lint("seq(10, from = 1, by = 2)", linter)
  expect_no_lint("seq(from = 1, 10, by = 2)", linter)
  expect_no_lint("seq(to = 10, 1, by = 2)", linter)
  expect_no_lint("seq(1, 10, length.out = 5)", linter)
  expect_no_lint("seq(1, 10, along.with = x)", linter)
  expect_no_lint("seq(1, 10, 2)", linter)
  expect_no_lint("seq(from = 2, to = 10)", linter)
  expect_no_lint("seq(from = 1, to = 10, by = 2)", linter)
  expect_no_lint("seq(-1, 10)", linter)
  expect_no_lint("seq(from = -1, 10)", linter)
  expect_no_lint("seq(10, from = -1)", linter)
  expect_no_lint("seq(from = -1, to = 10)", linter)
})
