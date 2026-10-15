test_that("decimal_fraction_linter flags decimal fraction multipliers/divisors", {
  linter <- decimal_fraction_linter()

  expect_lint(
    "as.integer(x * 0.1)",
    rex::rex("Use", anything, "as.integer", anything, "/", anything, "10L", anything, "avoid floating-point rounding"),
    linter
  )
  expect_lint(
    "as.integer(x * 3.2)",
    rex::rex(
      "Use", anything, "as.integer", anything, "16L", anything, "/ 5L", anything, "avoid floating-point rounding"
    ),
    linter
  )
  expect_lint(
    "as.integer(x * 0.14)",
    rex::rex(
      "Use", anything, "as.integer", anything, "7L", anything, "/ 50L", anything, "avoid floating-point rounding"
    ),
    linter
  )
  expect_lint(
    "as.integer(x * -0.14)",
    rex::rex(
      "Use", anything, "as.integer", anything, "-7L", anything, "/ 50L", anything, "avoid floating-point rounding"
    ),
    linter
  )
  expect_lint(
    "as.integer(x * 7.001)",
    rex::rex("as.integer", anything, "7001L", anything, "/", anything, "1000L"),
    linter
  )
  expect_lint(
    "as.integer(x / 0.13)",
    rex::rex(
      "Use", anything, "as.integer", anything, "100L", anything, "/ 13L", anything, "avoid floating-point rounding"
    ),
    linter
  )
  expect_lint(
    "as.integer(x * 365.2501)",
    rex::rex("as.integer", anything, "3652501L", anything, "/", anything, "10000L"),
    linter
  )
  expect_lint(
    "ceiling(x * 3.2)",
    rex::rex(
      "Use", anything, "ceiling", anything, "16L", anything, "/ 5L",
      anything, "avoid floating-point rounding"
    ),
    linter
  )
  expect_lint(
    "floor(x / 0.13)",
    rex::rex("Use", anything, "floor", anything, "100L", anything, "/ 13L", anything, "avoid floating-point rounding"),
    linter
  )
  expect_lint(
    "trunc(x * 0.14)",
    rex::rex("Use", anything, "trunc", anything, "7L", anything, "/ 50L", anything, "avoid floating-point rounding"),
    linter
  )
})

test_that("decimal_fraction_linter flags negative exponent fractional literals", {
  linter <- decimal_fraction_linter()

  expect_lint(
    "as.integer(x * 3e-5)",
    rex::rex(
      "Use", anything, "as.integer", anything, "3L", anything, "/ 100000L", anything, "avoid floating-point rounding"
    ),
    linter
  )
  expect_lint(
    "as.integer(x * 1e-3)",
    rex::rex("Use", anything, "as.integer", anything, "/ 1000L", anything, "avoid floating-point rounding"),
    linter
  )
})

test_that("decimal_fraction_linter skips integer-like multipliers/divisors", {
  linter <- decimal_fraction_linter()
  linter_exact <- decimal_fraction_linter(lint_exact_binary = TRUE)

  expect_no_lint("as.integer(x * 365)", linter)
  expect_no_lint("as.integer(x * 3.0)", linter)
  expect_no_lint("as.integer(0.125 * x)", linter)
  expect_no_lint("as.integer(x * 1.25)", linter)
  expect_no_lint("as.integer(x * 3)", linter)
  expect_no_lint("as.integer(x * 3L)", linter)
  expect_no_lint("as.integer(x * 365.0)", linter)
  expect_no_lint("as.integer(x * 365.25)", linter)
  expect_no_lint("as.integer(365.25 * x)", linter)
  expect_no_lint("as.integer((x + y) * 365.25)", linter)
  expect_no_lint("ceiling(x * 365.25)", linter)
  expect_no_lint("as.integer(x * 12.25)", linter)
  expect_no_lint("as.integer(x * 1e3)", linter)
  expect_no_lint("as.integer(x * 3.2e2)", linter)
  expect_no_lint("as.integer(x * NaN)", linter)
  expect_no_lint("as.integer(x * Inf)", linter)
  expect_no_lint("as.integer(x / 0.5)", linter)
  expect_no_lint("as.integer(x / Inf)", linter)
  expect_no_lint("as.integer(0.13 / x)", linter)
  expect_no_lint("as.numeric(x * 365.25)", linter)
  expect_lint(
    "as.integer(0.125 * x)",
    list(message = rex::rex("Use", anything, "8L", anything, "avoid floating-point rounding")),
    linter_exact
  )
  expect_lint(
    "as.integer(x * 0.5)",
    list(message = rex::rex("Use", anything, "as.integer", anything, "x / 2L")),
    linter_exact
  )
  expect_lint(
    "as.integer(x / 0.25)",
    list(message = rex::rex("Use", anything, "as.integer", anything, "4L")),
    linter_exact
  )
})

test_that("decimal_fraction_linter respects lint_exact_binary", {
  linter <- decimal_fraction_linter()
  linter_exact <- decimal_fraction_linter(lint_exact_binary = TRUE)

  expect_no_lint("as.integer(x * 0.5)", linter)
  expect_lint(
    "as.integer(x * 0.5)",
    list(message = rex::rex("Use", anything, "as.integer", anything, "x / 2L")),
    linter_exact
  )
  expect_no_lint("as.integer(x / 0.25)", linter)
  expect_lint(
    "as.integer(x / 0.25)",
    list(message = rex::rex("Use", anything, "as.integer", anything, "4L")),
    linter_exact
  )
  expect_no_lint("as.integer(1.25 * x)", linter)
  expect_lint(
    "as.integer(1.25 * x)",
    list(message = rex::rex("Use", anything, "5L", anything, "/ 4L")),
    linter_exact
  )
})

test_that("decimal_fraction_linter helpers parse and normalize decimal fractions", {
  parts <- extract_decimal_parts("3.2")
  expect_identical(
    convert_parts_to_fraction(parts[["integer_part"]], parts[["fractional_part"]]),
    list(numerator = 16L, denominator = 5L)
  )
  parts <- extract_decimal_parts("0.14")
  expect_identical(
    convert_parts_to_fraction(parts[["integer_part"]], parts[["fractional_part"]]),
    list(numerator = 7L, denominator = 50L)
  )
  parts <- extract_decimal_parts("-0.14")
  expect_identical(
    convert_parts_to_fraction(parts[["integer_part"]], parts[["fractional_part"]]),
    list(numerator = -7L, denominator = 50L)
  )
  parts <- extract_decimal_parts("7.001")
  expect_identical(
    convert_parts_to_fraction(parts[["integer_part"]], parts[["fractional_part"]]),
    list(numerator = 7001L, denominator = 1000L)
  )
  parts <- extract_decimal_parts("3e-5")
  expect_identical(
    convert_parts_to_fraction(parts[["integer_part"]], parts[["fractional_part"]]),
    list(numerator = 3L, denominator = 100000L)
  )
  parts <- extract_decimal_parts("1e-3")
  expect_identical(
    convert_parts_to_fraction(parts[["integer_part"]], parts[["fractional_part"]]),
    list(numerator = 1L, denominator = 1000L)
  )
  parts <- extract_decimal_parts("1.23e1")
  expect_identical(
    convert_parts_to_fraction(parts[["integer_part"]], parts[["fractional_part"]]),
    list(numerator = 123L, denominator = 10L)
  )

  expect_true(is_exact_binary_literal("25"))
  expect_false(is_exact_binary_literal("1"))

  parts <- extract_decimal_parts("1.2300e0")
  expect_identical(parts[["fractional_part"]], "23")

  expect_identical(reduce_fraction(12L, 20L), list(numerator = 3L, denominator = 5L))
  expect_null(reduce_fraction(1L, 1L))
  expect_null(reduce_fraction(1L, 0L))
  expect_identical(integer_gcd(18L, 12L), 6L)
})

test_that("decimal_fraction_linter lint ranges start at decimal literal", {
  linter <- decimal_fraction_linter()

  expect_lint(
    "as.integer(x * 0.14)",
    list(
      message = rex::rex("Use", anything, "7L", anything, "/ 50L", anything, "avoid floating-point rounding"),
      line_number = 1L,
      column_number = 16L,
      ranges = list(c(16L, 19L))
    ),
    linter
  )
  expect_lint(
    "as.integer(x / 0.13)",
    list(
      message = rex::rex("Use", anything, "100L", anything, "/ 13L", anything, "avoid floating-point rounding"),
      line_number = 1L,
      column_number = 16L,
      ranges = list(c(16L, 19L))
    ),
    linter
  )
})

test_that("decimal_fraction_linter drops redundant integer factors in messages", {
  linter <- decimal_fraction_linter()

  lints <- lint(text = "as.integer(x * 0.1)", linters = linter)
  message <- lints[[1L]]$message
  expect_false(grepl("* 1L", message, fixed = TRUE))
  expect_false(grepl("/ 1L", message, fixed = TRUE))

  lints <- lint(text = "as.integer(x / 0.2)", linters = linter)
  message <- lints[[1L]]$message
  expect_false(grepl("/ 1L", message, fixed = TRUE))
})
