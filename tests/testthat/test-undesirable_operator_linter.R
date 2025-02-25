test_that("linter returns correct linting", {
  linter <- undesirable_operator_linter(op = c("$" = "As an alternative, use the `[[` accessor.", "<<-" = NA))
  msg_assign <- rex::rex("Avoid undesirable operator `<<-`.")
  msg_dollar <- rex::rex("Avoid undesirable operator `$`. As an alternative, use the `[[` accessor.")

  expect_no_lint("x <- foo:::getObj()", linter)
  expect_no_lint("cat(\"10$\")", linter)
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

  expect_no_lint("`%%`(10, 2)", linter)
})

test_that("undesirable_operator_linter handles '=' consistently", {
  linter <- undesirable_operator_linter(op = c("=" = "As an alternative, use '<-'"))

  expect_lint("a = 2L", rex::rex("Avoid undesirable operator `=`."), linter)
  expect_no_lint("lm(data = mtcars)", linter)
  expect_no_lint("function(a = 1) { }", linter)
})

test_that("undesirable_operator_linter handles infixes correctly", {
  linter_oo <- undesirable_operator_linter(list("%oo%" = NA))
  linter_mod <- undesirable_operator_linter(list("%%" = NA))
  expect_lint("a %oo% b", rex::rex("Avoid undesirable operator `%oo%`."), linter_oo)
  expect_no_lint("a %00% b", linter_oo)

  # somewhat special case: %% is in infix_metadata
  expect_lint(
    "foo(x %% y, x %/% y)",
    rex::rex("Avoid undesirable operator `%%`."),
    linter_mod
  )

  expect_lint(
    "`%%`(10, 2)",
    rex::rex("Avoid undesirable operator `%%`."),
    linter_mod
  )
})

test_that("undesirable_operator_linter vectorizes messages", {
  expect_lint(
    "x <<- c(pkg:::foo, bar %oo% baz, `->>`(a, 2))",
    list(
      rex::rex("Avoid undesirable operator `<<-`. It assigns"),
      rex::rex("Avoid undesirable operator `:::`. It accesses"),
      rex::rex("Avoid undesirable operator `%oo%`.", end),
      rex::rex("Avoid undesirable operator `->>`. It assigns")
    ),
    undesirable_operator_linter(modify_defaults(default_undesirable_operators, "%oo%" = NA))
  )
})

test_that("invalid inputs fail correctly", {
  expect_error(
    undesirable_operator_linter(c("***" = NA, NA)),
    "Found missing entries to `op`: 2",
    fixed = TRUE
  )
  expect_error(
    undesirable_operator_linter(op = NULL),
    "`op` should be a non-empty character vector",
    fixed = TRUE
  )
  expect_error(
    undesirable_operator_linter(op = character(0L)),
    "`op` should be a non-empty character vector",
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

test_that("Default recommendations can be specified multiple ways", {
  linter_na <- undesirable_operator_linter(c(`%f%` = NA))
  linter_unnamed <- undesirable_operator_linter("%f%")
  linter_mixed1 <- undesirable_operator_linter(c("%f%", `%b%` = "no %b%"))
  linter_mixed2 <- undesirable_operator_linter(c("%f%", `%b%` = NA))

  lint_message <- rex::rex("Avoid undesirable operator `%f%`")

  lint_str <- "a %f% b"
  expect_lint(lint_str, lint_message, linter_na)
  expect_lint(lint_str, lint_message, linter_unnamed)
  expect_lint(lint_str, lint_message, linter_mixed1)
  expect_lint(lint_str, lint_message, linter_mixed2)
})
