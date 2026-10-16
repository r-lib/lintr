test_that("can_coerce_linter skips allowed usages", {
  linter <- can_coerce_linter()

  expect_no_lint("canCoerce(x, 'numeric')", linter)
  expect_no_lint("methods::canCoerce(x, 'numeric')", linter)
  expect_no_lint("as(x, 'numeric')", linter)
  expect_no_lint("try(foo(x))", linter)
  expect_no_lint("tryCatch(foo(x), error = identity)", linter)

  # Passing strict = or ext = to as() has no direct equivalent in canCoerce()
  expect_no_lint("try(as(x, 'numeric', strict = FALSE))", linter)
  expect_no_lint("try(as(x, 'numeric', ext = possibleExtends('a', 'b')))", linter)
  expect_no_lint("try(as(x, 'numeric', FALSE))", linter)

  # Multi-statement block inside try() / tryCatch(), or as() in an error handler
  expect_no_lint("try({ y <- as(x, 'numeric'); y + 1 })", linter)
  expect_no_lint("tryCatch(foo(x), error = function(e) as(x, 'numeric'))", linter)
  expect_no_lint("try(as(x, 'numeric') <- 1)", linter)
})

test_that("can_coerce_linter blocks try(as(x, cl)) and tryCatch(as(x, cl), ...)", {
  linter <- can_coerce_linter()
  try_msg <- rex::rex("Use methods::canCoerce(x, cl) instead of try(as(x, cl)).")
  trycatch_msg <- rex::rex("Use methods::canCoerce(x, cl) instead of tryCatch(as(x, cl)).")

  expect_lint(
    "try(as(x, 'numeric'))",
    list(try_msg, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "try(as(x, 'numeric'), silent = TRUE)",
    list(try_msg, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "try(silent = TRUE, expr = as(x, 'numeric'))",
    list(try_msg, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "try(silent = TRUE, as(x, 'numeric'))",
    list(try_msg, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "try(as(object = x, Class = 'numeric'))",
    list(try_msg, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "base::try(methods::as(x, 'numeric'))",
    list(try_msg, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "!inherits(try(as(x, 'numeric'), silent = TRUE), 'try-error')",
    list(try_msg, line_number = 1L, column_number = 11L),
    linter
  )

  expect_lint(
    "tryCatch(as(x, 'numeric'), error = function(e) FALSE)",
    list(trycatch_msg, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "tryCatch(error = function(e) NULL, as(x, 'numeric'))",
    list(trycatch_msg, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "tryCatch(error = function(e) NULL, expr = as(x, 'numeric'))",
    list(trycatch_msg, line_number = 1L, column_number = 1L),
    linter
  )
})

test_that("can_coerce_linter handles pipes", {
  linter <- can_coerce_linter()
  try_msg <- rex::rex("Use methods::canCoerce(x, cl) instead of try(as(x, cl)).")
  trycatch_msg <- rex::rex("Use methods::canCoerce(x, cl) instead of tryCatch(as(x, cl)).")

  expect_lint("try(x |> as('numeric'))", try_msg, linter)
  expect_lint("try(x %>% as('numeric'))", try_msg, linter)
  expect_lint("as(x, 'numeric') |> try(silent = TRUE)", try_msg, linter)
  expect_lint("as(x, 'numeric') %>% tryCatch(error = identity)", trycatch_msg, linter)
  expect_lint("x |> as('numeric') |> try()", try_msg, linter)
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      try(as(x, 'numeric'))
      tryCatch(as(y, 'character'), error = identity)
    }"),
    list(
      list(
        rex::rex("Use methods::canCoerce(x, cl) instead of try(as(x, cl))."),
        line_number = 2L,
        column_number = 3L
      ),
      list(
        rex::rex("Use methods::canCoerce(x, cl) instead of tryCatch(as(x, cl))."),
        line_number = 3L,
        column_number = 3L
      )
    ),
    can_coerce_linter()
  )
})
