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

skip_if_not_installed("tibble")
patrick::with_parameters_test_that(
  "finds seq(...) expressions",
  expect_lint(
    target_code,
    rex::rex("Use ", good_usage, " instead of ", bad_usage),
    seq_linter()
  ),
  .cases = tibble::tribble(
    ~target_code,          ~good_usage,          ~bad_usage,
    "seq(length(x))",      "seq_along(...)",     "seq(length(...))",
    "seq(nrow(x))",        "seq_len(nrow(...))", "seq(nrow(...))",
    "rev(seq(length(x)))", "seq_along(...)",     "seq(length(...))",
    "rev(seq(nrow(x)))",   "seq_len(nrow(...))", "seq(nrow(...))"
  )
)

patrick::with_parameters_test_that(
  "finds 1:x expressions",
  expect_lint(
    target_code,
    rex::rex("Use ", good_usage, " instead of ", bad_usage),
    seq_linter()
  ),
  .cases = tibble::tribble(
    ~target_code,                    ~good_usage,              ~bad_usage,
    "1:length(x)",                   "seq_along(...)",         "1:length(...)",
    "1L:length(x)",                  "seq_along(...)",         "1L:length(...)",
    "1:nrow(x)",                     "seq_len(nrow(...))",     "1:nrow(...)",
    "1:ncol(x)",                     "seq_len(ncol(...))",     "1:ncol(...)",
    "1:NROW(x)",                     "seq_len(NROW(...))",     "1:NROW(...)",
    "1:NCOL(x)",                     "seq_len(NCOL(...))",     "1:NCOL(...)",
    "1:dim(x)[1L]",                  "seq_len(dim(...)[1L])",  "1:dim(...)[1L]",
    "1L:dim(x)[[1]]",                "seq_len(dim(...)[[1]])", "1L:dim(...)[[1]]",
    "mutate(x, .id = 1:n())",        "seq_len(n())",           "1:n()",
    "mutate(x, .id = 1:dplyr::n())", "seq_len(dplyr::n())",    "1:dplyr::n()",
    "x[, .id := 1:.N]",              "seq_len(.N)",            "1:.N"
  )
)

test_that("adversarial comment is handled in 1:n()", {
  expect_lint(
    trim_some("
      mutate(x, .id = 1:n( # comment
      ))
    "),
    rex::rex("Use seq_len(n()) instead of 1:n()"),
    seq_linter()
  )
})

test_that("reverse seq is ok", {
  linter <- seq_linter()
  expect_no_lint("rev(seq_along(x))", linter)
  expect_no_lint("rev(seq_len(nrow(x)))", linter)
})

patrick::with_parameters_test_that(
  "finds potential sequence() replacements",
  expect_lint(
    sprintf("unlist(%s(x, %s))", map_fn, seq_fn),
    rex::rex("Use sequence()"),
    seq_linter()
  ),
  .cases = expand.grid(
    map_fn = c("lapply", "sapply", "map", "purrr::map"),
    seq_fn = c("seq_len", "seq")
  )
)

test_that("sequence() is not recommended for complex seq() calls", {
  expect_no_lint("unlist(lapply(x, seq, from = 2))", seq_linter())
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
      list(rex::rex("rev(seq_len(10))", anything, "seq(10, 1)"), line_number = 5L)
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

patrick::with_parameters_test_that(
  "Message recommends rev() correctly",
  expect_lint(
    target_code,
    rex::rex("Use ", good_usage, " instead of ", bad_usage),
    seq_linter()
  ),
  .cases = tibble::tribble(
    ~target_code,   ~good_usage,                ~bad_usage,
    ".N:1",         "rev(seq_len(.N))",         ".N:1",
    "n():1",        "rev(seq_len(n()))",        "n():1",
    "dplyr::n():1", "rev(seq_len(dplyr::n()))", "dplyr::n():1",
    "nrow(x):1",    "rev(seq_len(nrow(...)))",  "nrow(...):1",
    "length(x):1",  "rev(seq_along(...))",      "length(...):1"
  )
)

test_that("seq_len(length(x)) should be seq_along(x)", {
  linter <- seq_linter()

  expect_no_lint("seq_len(length(x) - 1)", linter)
  expect_no_lint("seq_len(2*length(x))", linter)
  expect_no_lint("seq_len(foo(length(x)))", linter)
  expect_lint("seq_len(length(x))", "seq_along", linter)
  expect_lint("seq_len(length(foo(x)))", "seq_along", linter)
})

patrick::with_parameters_test_that(
  "finds seq(1, n) and seq(from = 1, to = n) expressions",
  expect_lint(
    target_code,
    rex::rex("Use ", good_usage, " instead of ", bad_usage),
    seq_linter()
  ),
  .cases = tibble::tribble(
    ~target_code,                    ~good_usage,               ~bad_usage,
    "seq(1, 10)",                     "seq_len(10)",             "seq(1, 10)",
    "seq(1, 1)",                      "seq_len(1)",              "seq(1, 1)",
    "base::seq(1, 10)",               "seq_len(10)",             "seq(1, 10)",
    "seq(1L, 10L)",                   "seq_len(10L)",            "seq(1L, 10L)",
    "seq(1, n)",                      "seq_len(n)",              "seq(1, n)",
    "seq(1, length(x))",              "seq_along(...)",          "seq(1, length(...))",
    "seq(1, nrow(x))",                "seq_len(nrow(...))",      "seq(1, nrow(...))",
    "seq(from = 1, to = 10)",         "seq_len(10)",             "seq(1, 10)",
    "seq(from = 1L, to = length(x))", "seq_along(...)",          "seq(1L, length(...))",
    "seq(to = 10, from = 1)",         "seq_len(10)",             "seq(1, 10)",
    "seq(to = length(x), from = 1L)", "seq_along(...)",          "seq(1L, length(...))",
    "seq(1, to = 10)",                "seq_len(10)",             "seq(1, 10)",
    "seq(to = 10, 1)",                "seq_len(10)",             "seq(1, 10)",
    "seq(to = 10, 1L)",               "seq_len(10)",             "seq(1L, 10)",
    "seq(10, from = 1)",              "seq_len(10)",             "seq(1, 10)",
    "seq(10, from = 1L)",             "seq_len(10)",             "seq(1L, 10)",
    "seq(from = 1, 10)",              "seq_len(10)",             "seq(1, 10)",
    "seq(from = 1L, 10)",             "seq_len(10)",             "seq(1L, 10)",
    "seq(length(x), from = 1)",       "seq_along(...)",          "seq(1, length(...))",
    "seq(length(x), from = 1L)",      "seq_along(...)",          "seq(1L, length(...))",
    "seq(from = 1, length(x))",       "seq_along(...)",          "seq(1, length(...))",
    "seq(to = length(x), 1)",         "seq_along(...)",          "seq(1, length(...))",
    # Decreasing seq() calls
    "seq(10, 1)",                     "rev(seq_len(10))",        "seq(10, 1)",
    "seq(10L, 1L)",                   "rev(seq_len(10L))",       "seq(10L, 1L)",
    "seq(n, 1)",                      "rev(seq_len(n))",         "seq(n, 1)",
    "seq(length(x), 1)",              "rev(seq_along(...))",     "seq(length(...), 1)",
    "seq(nrow(x), 1)",                "rev(seq_len(nrow(...)))", "seq(nrow(...), 1)",
    "seq(from = 10, to = 1)",         "rev(seq_len(10))",        "seq(10, 1)",
    "seq(to = 1, from = 10)",         "rev(seq_len(10))",        "seq(10, 1)",
    "seq(to = 1L, from = 10)",        "rev(seq_len(10))",        "seq(10, 1L)",
    "seq(from = length(x), to = 1)",  "rev(seq_along(...))",     "seq(length(...), 1)",
    "seq(from = length(x), to = 1L)", "rev(seq_along(...))",     "seq(length(...), 1L)",
    "seq(to = 1, from = length(x))",  "rev(seq_along(...))",     "seq(length(...), 1)",
    "seq(to = 1L, from = length(x))", "rev(seq_along(...))",     "seq(length(...), 1L)",
    "seq(10, to = 1)",                "rev(seq_len(10))",        "seq(10, 1)",
    "seq(to = 1, 10)",                "rev(seq_len(10))",        "seq(10, 1)",
    "seq(from = 10, 1)",              "rev(seq_len(10))",        "seq(10, 1)",
    "seq(from = 10, 1L)",             "rev(seq_len(10))",        "seq(10, 1L)",
    "seq(1, from = 10)",              "rev(seq_len(10))",        "seq(10, 1)",
    "seq(1L, from = 10)",             "rev(seq_len(10))",        "seq(10, 1L)"
  )
)

patrick::with_parameters_test_that(
  "complex seq() expressions or other arguments are fine",
  expect_no_lint(target_code, seq_linter()),
  target_code = c(
    "seq(0, 1)",
    "seq(0L, 1L)",
    "seq(0, 1L)",
    "seq(0L, 1)",
    "seq(from = 0, to = 1)",
    "seq(to = 1, from = 0)",
    "seq(to = 1, 0)",
    "seq(from = 0, 1)",
    "seq(-1, 1)",
    "seq(from = -1, 1)",
    "seq(to = 1, from = -1)",
    "seq(from = -1, to = 1)",
    "seq(0, 10)",
    "seq(2, 10)",
    "seq(10, from = 2)",
    "seq(from = 2, 10)",
    "seq(10, to = 2)",
    "seq(to = 2, from = 10)",
    "seq(from = 10, to = 2)",
    "seq(to = 2, 10)",
    "seq(from = 10, 2)",
    "seq(10, 2)",
    "seq(1, 10, by = 2)",
    "seq(10, from = 1, by = 2)",
    "seq(from = 1, 10, by = 2)",
    "seq(to = 10, 1, by = 2)",
    "seq(to = 1, from = 10, by = -2)",
    "seq(1, 10, length.out = 5)",
    "seq(1, 10, along.with = x)",
    "seq(1, 10, 2)",
    "seq(from = 2, to = 10)",
    "seq(from = 1, to = 10, by = 2)",
    "seq(-1, 10)",
    "seq(from = -1, 10)",
    "seq(10, from = -1)",
    "seq(from = -1, to = 10)",
    "seq(10, -1)",
    "seq(from = 10, -1)",
    "seq(-1, from = 10)",
    "seq(from = 10, to = -1)"
  )
)
