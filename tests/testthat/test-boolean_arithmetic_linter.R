test_that("boolean_arithmetic_linter doesn't block allowed usages", {
  linter <- boolean_arithmetic_linter()

  expect_no_lint("!any(x == y)", linter)
  expect_no_lint("!any(grepl(pattern, x))", linter)
  expect_no_lint("all(x == y)", linter)
  expect_no_lint("sum(x == y) == length(z)", linter)
  expect_no_lint("sum(grepl(p, x)) == length(p)", linter)
  expect_no_lint("length(grep(p, x)) == length(p)", linter)
  expect_no_lint("sum(x$a == y) == length(x)", linter)
  expect_no_lint("sum(x[i] == y) == length(x)", linter)
  expect_no_lint("sum(unique(x) == y) == length(x)", linter)
  expect_no_lint("length(which(x$a)) == length(x)", linter)
  expect_no_lint("sum(x %in% y) == length(y)", linter)
  expect_no_lint("sum(x %chin% y) == length(y)", linter)
  expect_no_lint("sum(is.element(x, y)) == length(y)", linter)
  expect_no_lint("sum(str_detect(x, p)) == length(p)", linter)
  expect_no_lint("sum(startsWith(x, p)) == length(p)", linter)
  expect_no_lint("sum(endsWith(x, p)) == length(p)", linter)
  expect_no_lint("sum(x, y == 0) == length(x)", linter)
  expect_no_lint("sum(x == 1, y == 1) == length(x)", linter)
})

test_that("boolean_arithmetic_linter requires use of any() or !any() over length(.(<logical>))", {
  linter <- boolean_arithmetic_linter()

  expect_lint(
    "length(which(x == y)) == 0",
    rex::rex(
      "Use any() to express logical aggregations. ",
      "For example, replace length(which(x == y)) == 0 with !any(x == y)."
    ),
    linter
  )
  # anything passed to which() can be assumed to be logical
  expect_lint(
    "length(which(is_treatment)) == 0L",
    rex::rex(
      "Use any() to express logical aggregations. ",
      "For example, replace length(which(x == y)) == 0L with !any(x == y)."
    ),
    linter
  )
  # regex version
  expect_lint(
    "length(grep(pattern, x)) == 0",
    rex::rex(
      "Use any() to express logical aggregations. ",
      "For example, replace length(grep(pattern, x)) == 0 with !any(grepl(pattern, x))."
    ),
    linter
  )

  # non-== comparisons
  expect_lint(
    "length(which(x == y)) > 0L",
    rex::rex(
      "Use any() to express logical aggregations. ",
      "For example, replace length(which(x == y)) > 0L with any(x == y)."
    ),
    linter
  )
  expect_lint(
    "length(which(is_treatment)) < 1",
    rex::rex(
      "Use any() to express logical aggregations. ",
      "For example, replace length(which(x == y)) < 1 with !any(x == y)."
    ),
    linter
  )
  expect_lint(
    "length(grep(pattern, x)) >= 1L",
    rex::rex(
      "Use any() to express logical aggregations. ",
      "For example, replace length(grep(pattern, x)) >= 1L with any(grepl(pattern, x))."
    ),
    linter
  )
  expect_lint(
    "0L == sum(x == y)",
    rex::rex(
      "Use any() to express logical aggregations. ",
      "For example, replace 0L == sum(x == y) with !any(x == y)."
    ),
    linter
  )
})

test_that("boolean_arithmetic_linter requires use of all() or !all() for length(x) comparisons (#1581)", {
  linter <- boolean_arithmetic_linter()

  expect_lint(
    "sum(x == y) == length(x)",
    list(
      rex::rex(
        "Use all() to express logical aggregations. ",
        "For example, replace sum(x == y) == length(x) with all(x == y)."
      ),
      line_number = 1L,
      column_number = 1L
    ),
    linter
  )
  expect_lint(
    "sum(grepl(p, x)) != length(x)",
    list(
      rex::rex(
        "Use all() to express logical aggregations. ",
        "For example, replace sum(grepl(pattern, x)) != length(x) with !all(grepl(pattern, x))."
      ),
      line_number = 1L,
      column_number = 1L
    ),
    linter
  )
  expect_lint(
    "sum(x == 0 & y == 1) < length(y)",
    list(
      rex::rex(
        "Use all() to express logical aggregations. ",
        "For example, replace sum(x == y) < length(x) with !all(x == y)."
      ),
      line_number = 1L,
      column_number = 1L
    ),
    linter
  )
  expect_lint(
    "sum(is.na(x)) >= length(x)",
    list(
      rex::rex(
        "Use all() to express logical aggregations. ",
        "For example, replace sum(x == y) >= length(x) with all(x == y)."
      ),
      line_number = 1L,
      column_number = 1L
    ),
    linter
  )
  expect_lint(
    "length(x) == sum(x == y)",
    list(
      rex::rex(
        "Use all() to express logical aggregations. ",
        "For example, replace length(x) == sum(x == y) with all(x == y)."
      ),
      line_number = 1L,
      column_number = 14L
    ),
    linter
  )
  expect_lint(
    "length(x) > sum(x == y)",
    list(
      rex::rex(
        "Use all() to express logical aggregations. ",
        "For example, replace length(x) > sum(x == y) with !all(x == y)."
      ),
      line_number = 1L,
      column_number = 13L
    ),
    linter
  )
  expect_lint(
    "length(x) <= sum(x == y)",
    list(
      rex::rex(
        "Use all() to express logical aggregations. ",
        "For example, replace length(x) <= sum(x == y) with all(x == y)."
      ),
      line_number = 1L,
      column_number = 14L
    ),
    linter
  )

  expect_lint(
    "length(which(x == y)) == length(x)",
    list(
      rex::rex(
        "Use all() to express logical aggregations. ",
        "For example, replace length(which(x == y)) == length(x) with all(x == y)."
      ),
      line_number = 1L,
      column_number = 1L
    ),
    linter
  )
  expect_lint(
    "length(which(is_treatment)) == length(is_treatment)",
    list(
      rex::rex(
        "Use all() to express logical aggregations. ",
        "For example, replace length(which(x == y)) == length(x) with all(x == y)."
      ),
      line_number = 1L,
      column_number = 1L
    ),
    linter
  )
  expect_lint(
    "length(grep(pattern, x)) == length(x)",
    list(
      rex::rex(
        "Use all() to express logical aggregations. ",
        "For example, replace length(grep(pattern, x)) == length(x) with all(grepl(pattern, x))."
      ),
      line_number = 1L,
      column_number = 1L
    ),
    linter
  )
  expect_lint(
    "length(grep(pattern, x)) < length(x)",
    list(
      rex::rex(
        "Use all() to express logical aggregations. ",
        "For example, replace length(grep(pattern, x)) < length(x) with !all(grepl(pattern, x))."
      ),
      line_number = 1L,
      column_number = 1L
    ),
    linter
  )
})

local({
  linter <- boolean_arithmetic_linter()
  lint_msg <- rex::rex("Use any() to express logical aggregations.")

  outer_comparisons <- c("== 0", "== 0L", "> 0L", "> 0L", ">= 1", ">= 1L")

  patrick::with_parameters_test_that(
    "sum(x {op} y) {outer} lints",
    expect_lint(sprintf("sum(x %s y) %s", op, outer), lint_msg, linter),
    .cases = expand.grid(
      op = c("==", "!=", ">", "<", ">=", "<=", "&", "|", "%in%", "%chin%"),
      outer = outer_comparisons
    )
  )

  patrick::with_parameters_test_that(
    "sum({op}(x)) == 0 lints",
    expect_lint(sprintf("sum(%s(x)) == 0", op), lint_msg, linter),
    .cases = expand.grid(
      op = c(
        "!", "xor", "grepl", "str_detect", "is.element",
        "is.na", "is.finite", "is.infinite", "is.nan",
        "duplicated", "nzchar", "startsWith", "endsWith"
      ),
      outer = outer_comparisons
    )
  )
})

test_that("boolean_arithmetic_linter handles nested and compound all() expressions (#1581)", {
  linter <- boolean_arithmetic_linter()
  all_msg <- rex::rex("Use all() to express logical aggregations.")

  expect_lint("sum(!(x == y)) == length(x)", all_msg, linter)
  expect_lint("length(which(!(x == y))) == length(x)", all_msg, linter)
  expect_lint("sum(x == 1 & y == 2 & z == 3) == length(x)", all_msg, linter)
  expect_lint("sum(x %in% y) == length(x)", all_msg, linter)
  expect_lint("sum(is.element(x, y)) == length(x)", all_msg, linter)
  expect_lint("sum(str_detect(x, p)) == length(x)", all_msg, linter)
  expect_lint("sum(startsWith(x, p)) == length(x)", all_msg, linter)
  expect_lint("sum(endsWith(x, p)) == length(x)", all_msg, linter)
  expect_lint("sum(xor(x, y)) == length(x)", all_msg, linter)
  expect_lint("sum(xor(x, y)) == length(y)", all_msg, linter)
  expect_lint("sum(x == y, na.rm = TRUE) == length(x)", all_msg, linter)
  expect_lint("sum(x[i] == y) == length(x[i])", all_msg, linter)
  expect_lint(
    trim_some("
      sum(names(x # comment
      ) == y) == length(names(x))
    "),
    all_msg,
    linter
  )
})

test_that("lints vectorize", {
  any_msg <- rex::rex("Use any() to express logical aggregations.")
  all_msg <- rex::rex("Use all() to express logical aggregations.")

  expect_lint(
    trim_some("{
      length(which(x == y)) > 0L
      sum(x == y) != 0
      length(which(x == y)) == length(x)
      sum(x == y) < length(x)
    }"),
    list(
      list(any_msg, line_number = 2L),
      list(any_msg, line_number = 3L),
      list(all_msg, line_number = 4L),
      list(all_msg, line_number = 5L)
    ),
    boolean_arithmetic_linter()
  )
})
