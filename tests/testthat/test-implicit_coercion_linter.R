test_that("implicit_coercion_linter skips allowed usages", {
  linter <- implicit_coercion_linter()

  expect_no_lint("if (TRUE) 1", linter)
  expect_no_lint("if (FALSE) 1", linter)
  expect_no_lint("if (!TRUE) 1", linter)
  expect_no_lint("while (TRUE) break", linter)
  expect_no_lint("if (NA) 1", linter)

  # Explicit comparisons
  expect_no_lint("if (length(x) > 0L) 1", linter)
  expect_no_lint("if (length(x) == 0L) 1", linter)
  expect_no_lint("if (nrow(x) > 0L && ncol(x) > 0L) 1", linter)
  expect_no_lint("if (anyDuplicated(x) > 0L) 1", linter)
  expect_no_lint("if (sum(x) != 0) 1", linter)
  expect_no_lint("if (x + y > 0) 1", linter)
  expect_no_lint("if (x %% 2L == 0L) 1", linter)

  # Proper boolean predicates
  expect_no_lint("if (nzchar(x)) 1", linter)
  expect_no_lint("if (!nzchar(x)) 1", linter)
  expect_no_lint("if (grepl('a', x)) 1", linter)
  expect_no_lint("if (!grepl('a', x)) 1", linter)
  expect_no_lint("if (x %in% y) 1", linter)
  expect_no_lint("if (any(x > 0)) 1", linter)

  # Coercer calls inside non-boolean contexts (arguments, indexing, comparisons)
  expect_no_lint("if (foo(length(x))) 1", linter)
  expect_no_lint("if (x[length(x)]) 1", linter)
  expect_no_lint("if (x[[length(x)]]) 1", linter)
  expect_no_lint("if (is.na(match(x, y))) 1", linter)
  expect_no_lint("if (identical(length(x), 1L)) 1", linter)

  # Coercer calls in the body of if/while, not in the condition
  expect_no_lint("if (cond) length(x) else nchar(y)", linter)
  expect_no_lint("while (cond) length(x)", linter)
})

test_that("implicit_coercion_linter blocks coercer function calls in conditions", {
  linter <- implicit_coercion_linter()

  expect_lint(
    "if (length(x)) 1",
    list(
      rex::rex(
        "Use explicit comparison (e.g., length(...) > 0L) instead of relying on implicit coercion in conditional expressions." # nolint: line_length_linter.
      ),
      line_number = 1L,
      column_number = 5L
    ),
    linter
  )
  expect_lint(
    "if (!length(x)) 1",
    list(
      rex::rex(
        "Use explicit comparison (e.g., length(...) == 0L) instead of relying on implicit coercion in conditional expressions." # nolint: line_length_linter.
      ),
      line_number = 1L,
      column_number = 5L
    ),
    linter
  )
  expect_lint(
    "if (!(length(x))) 1",
    list(
      rex::rex("Use explicit comparison (e.g., length(...) == 0L)"),
      line_number = 1L,
      column_number = 5L
    ),
    linter
  )
  expect_lint(
    "while (nrow(df)) break",
    list(
      rex::rex("Use explicit comparison (e.g., nrow(...) > 0L)"),
      line_number = 1L,
      column_number = 8L
    ),
    linter
  )
  expect_lint(
    "if (!ncol(df)) 1",
    rex::rex("Use explicit comparison (e.g., ncol(...) == 0L)"),
    linter
  )
  expect_lint(
    "if (NROW(x) && !NCOL(y)) 1",
    list(
      list(rex::rex("NROW(...) > 0L"), line_number = 1L, column_number = 5L),
      list(rex::rex("NCOL(...) == 0L"), line_number = 1L, column_number = 16L)
    ),
    linter
  )
  expect_lint(
    "if (anyDuplicated(x)) 1",
    rex::rex("Use explicit comparison (e.g., anyDuplicated(...) > 0L)"),
    linter
  )
  expect_lint(
    "if (sum(x)) 1",
    rex::rex("Use explicit comparison (e.g., sum(...) != 0L)"),
    linter
  )
  expect_lint(
    "if (!sum(x)) 1",
    rex::rex("Use explicit comparison (e.g., sum(...) == 0L)"),
    linter
  )
  expect_lint(
    "if (bitwAnd(a, b)) 1",
    rex::rex("Use explicit comparison (e.g., bitwAnd(...) != 0L)"),
    linter
  )
  expect_lint(
    "if (match(x, table, nomatch = 0L)) 1",
    rex::rex("Use explicit comparison (e.g., match(...) != 0L)"),
    linter
  )
  expect_lint(
    "if (regexpr('a', x)) 1",
    rex::rex("Use explicit comparison (e.g., regexpr(...) != 0L)"),
    linter
  )
  expect_lint(
    "if (diff(x)) 1",
    rex::rex("Use explicit comparison (e.g., diff(...) != 0L)"),
    linter
  )
})

test_that("implicit_coercion_linter recommends nzchar() and grepl() for nchar() and grep()", {
  linter <- implicit_coercion_linter()

  expect_lint(
    "if (nchar(x)) 1",
    rex::rex("Use nzchar(x) instead of relying on implicit coercion of nchar(x) in conditional expressions."),
    linter
  )
  expect_lint(
    "if (!nchar(x)) 1",
    rex::rex("Use !nzchar(x) instead of relying on implicit coercion of !nchar(x) in conditional expressions."),
    linter
  )
  expect_lint(
    "if (grep('a', x)) 1",
    rex::rex("Use grepl() instead of relying on implicit coercion of grep() in conditional expressions."),
    linter
  )
  expect_lint(
    "if (!grep('a', x)) 1",
    rex::rex("Use !grepl() instead of relying on implicit coercion of !grep() in conditional expressions."),
    linter
  )
})

test_that("implicit_coercion_linter blocks literals and arithmetic in conditions", {
  linter <- implicit_coercion_linter()
  lit_msg <- rex::rex(
    "Use explicit TRUE or FALSE instead of relying on implicit coercion of literals in conditional expressions."
  )
  arith_msg <- rex::rex(
    "Use an explicit comparison (e.g., != 0) instead of relying on implicit coercion of arithmetic expressions in conditional expressions." # nolint: line_length_linter.
  )

  expect_lint("if (1) TRUE", list(lit_msg, line_number = 1L, column_number = 5L), linter)
  expect_lint("if (0L) TRUE", list(lit_msg, line_number = 1L, column_number = 5L), linter)
  expect_lint("if (-1) TRUE", list(lit_msg, line_number = 1L, column_number = 5L), linter)
  expect_lint("if (+1L) TRUE", list(lit_msg, line_number = 1L, column_number = 5L), linter)
  expect_lint("if ('TRUE') TRUE", list(lit_msg, line_number = 1L, column_number = 5L), linter)
  expect_lint("while (1L) break", list(lit_msg, line_number = 1L, column_number = 8L), linter)

  expect_lint("if (x + 1) TRUE", list(arith_msg, line_number = 1L, column_number = 5L), linter)
  expect_lint("if (x - y) TRUE", list(arith_msg, line_number = 1L, column_number = 5L), linter)
  expect_lint("if (x * y) TRUE", list(arith_msg, line_number = 1L, column_number = 5L), linter)
  expect_lint("if (x / y) TRUE", list(arith_msg, line_number = 1L, column_number = 5L), linter)
  expect_lint("if (x ^ 2) TRUE", list(arith_msg, line_number = 1L, column_number = 5L), linter)
  expect_lint("if (1:5) TRUE", list(arith_msg, line_number = 1L, column_number = 5L), linter)
  expect_lint("if (x %% 2L) TRUE", list(arith_msg, line_number = 1L, column_number = 5L), linter)
  expect_lint("if (x %/% 2L) TRUE", list(arith_msg, line_number = 1L, column_number = 5L), linter)
  expect_lint("if (-x) TRUE", list(arith_msg, line_number = 1L, column_number = 5L), linter)
  expect_lint("if (foo(x) + 1) TRUE", list(arith_msg, line_number = 1L, column_number = 5L), linter)
})

test_that("implicit_coercion_linter respects custom coercers argument", {
  expect_no_lint("if (anyDuplicated(x)) 1", implicit_coercion_linter(coercers = NULL))
  expect_lint(
    "if (my_count(x)) 1",
    rex::rex("Use explicit comparison (e.g., my_count(...) != 0L)"),
    implicit_coercion_linter(coercers = "my_count")
  )
  expect_error(
    implicit_coercion_linter(coercers = 123L),
    "`coercers` must be a character vector or `NULL`",
    fixed = TRUE
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      if (length(x) && !nchar(y)) 1
      while (1L || x %% 2L) break
    }"),
    list(
      list(rex::rex("length(...) > 0L"), line_number = 2L, column_number = 7L),
      list(rex::rex("Use !nzchar(x)"), line_number = 2L, column_number = 20L),
      list(rex::rex("Use explicit TRUE or FALSE"), line_number = 3L, column_number = 10L),
      list(rex::rex("Use an explicit comparison (e.g., != 0)"), line_number = 3L, column_number = 16L)
    ),
    implicit_coercion_linter()
  )
})
