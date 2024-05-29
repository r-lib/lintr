test_that("linter returns correct linting", {
  linter <- undesirable_operator_linter(op = c("$" = "As an alternative, use the `[[` accessor.", "<<-" = NA))
  msg_assign <- rex::escape("Avoid undesirable operator `<<-`.")
  msg_dollar <- rex::escape("Avoid undesirable operator `$`. As an alternative, use the `[[` accessor.")

  expect_lint("x <- foo:::getObj()", NULL, linter)
  expect_lint("cat(\"10$\")", NULL, linter)
  expect_lint(
    "a <<- log(10)",
    list(message = msg_assign, line_number = 1L, column_number = 3L),
    linter
  )
  expect_lint(
    "data$parsed == c(1, 2)",
    list(message = msg_dollar, line_number = 1L, column_number = 5L),
    linter
  )
})

test_that("undesirable_operator_linter handles '=' consistently", {
  linter <- undesirable_operator_linter(op = c("=" = "As an alternative, use '<-'"))

  expect_lint("a = 2L", rex::rex("Avoid undesirable operator `=`."), linter)
  expect_lint("lm(data = mtcars)", NULL, linter)
  expect_lint("function(a = 1) { }", NULL, linter)
})

test_that("undesirable_operator_linter handles infixes correctly", {
  linter <- undesirable_operator_linter(list("%oo%" = NA))
  expect_lint("a %oo% b", rex::rex("Avoid undesirable operator `%oo%`."), linter)
  expect_lint("a %00% b", NULL, linter)

  # somewhat special case: %% is in infix_metadata
  expect_lint(
    "foo(x %% y, x %/% y)",
    rex::rex("Avoid undesirable operator `%%`."),
    undesirable_operator_linter(list("%%" = NA))
  )
})

test_that("undesirable_operator_linter vectorizes messages", {
  expect_lint(
    "x <<- c(pkg:::foo, bar %oo% baz)",
    list(
      rex::rex("Avoid undesirable operator `<<-`. It assigns"),
      rex::rex("Avoid undesirable operator `:::`. It accesses"),
      rex::rex("Avoid undesirable operator `%oo%`.", end)
    ),
    undesirable_operator_linter(modify_defaults(default_undesirable_operators, "%oo%" = NA))
  )
})

test_that("invalid inputs fail correctly", {
  error_msg <- "`op` should be a non-empty named character vector"

  expect_error(
    undesirable_operator_linter("***"),
    error_msg,
    fixed = TRUE
  )
  expect_error(
    undesirable_operator_linter(c("***" = NA, NA)),
    error_msg,
    fixed = TRUE
  )
  expect_error(
    undesirable_operator_linter(op = NULL),
    error_msg,
    fixed = TRUE
  )
  expect_error(
    undesirable_operator_linter(op = character(0L)),
    error_msg,
    fixed = TRUE
  )

  expect_error(
    undesirable_operator_linter(c("***" = NA)),
    'Did not recognize any valid operators in request for: "***"',
    fixed = TRUE
  )
  expect_error(
    undesirable_operator_linter(c("***" = NA, "///" = NA)),
    'Did not recognize any valid operators in request for: "***" and "///"',
    fixed = TRUE
  )
})
