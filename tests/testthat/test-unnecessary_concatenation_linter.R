test_that("unnecessary_concatenation_linter skips allowed usages", {
  linter <- unnecessary_concatenation_linter()

  expect_no_lint("c(x)", linter)
  expect_no_lint("c(1, 2)", linter)
  expect_no_lint("c(x, recursive = TRUE)", linter)
  expect_no_lint("c(1, recursive = FALSE)", linter)
  expect_no_lint("lapply(1, c)", linter)
  expect_no_lint("c(a = 1)", linter)
  expect_no_lint("c('a' = 1)", linter)
})

test_that("unnecessary_concatenation_linter blocks disallowed usages", {
  linter <- unnecessary_concatenation_linter()
  msg_c <- rex::rex("Remove unnecessary c() of a constant.")
  msg_e <- rex::rex("Replace unnecessary c() by NULL or, whenever possible, vector()")

  expect_lint(
    "c()",
    list(message = msg_e, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "c(NULL)",
    list(message = msg_c, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "c(1)",
    list(message = msg_c, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "c (\n'a' )",
    list(message = msg_c, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "c(y, c('c('),\nc())",
    list(
      list(message = msg_c, line_number = 1L, column_number = 6L),
      list(message = msg_e, line_number = 2L, column_number = 1L)
    ),
    linter
  )
})

local({
  pipes <- pipes(exclude = "%$%")
  linter <- unnecessary_concatenation_linter()
  const_msg <- rex::rex("Remove unnecessary c() of a constant.")
  no_arg_msg <- rex::rex("Replace unnecessary c() by NULL or, whenever possible, vector()")

  patrick::with_parameters_test_that(
    "Correctly handles concatenation within magrittr pipes",
    {
      expect_no_lint(sprintf('"a" %s c("b")', pipe), linter)
      expect_lint(sprintf('"a" %s c()', pipe), const_msg, linter)
      expect_lint(sprintf('"a" %s list("b", c())', pipe), no_arg_msg, linter)
    },
    pipe = pipes,
    .test_name = names(pipes)
  )
})

test_that("logic survives adversarial comments", {
  expect_no_lint(
    trim_some('
      "a" %T>% # comment
        c("b")
    '),
    unnecessary_concatenation_linter()
  )
})

test_that("symbolic expressions are allowed, except by request", {
  linter <- unnecessary_concatenation_linter()
  linter_strict <- unnecessary_concatenation_linter(allow_single_expression = FALSE)
  lint_msg <- rex::rex("Remove unnecessary c() of a constant expression.")

  expect_no_lint("c(alpha / 2)", linter)
  expect_no_lint("c(paste0('.', 1:2))", linter)
  expect_no_lint("c(DF[cond > 1, col])", linter)

  # allow_single_expression = FALSE turns both into lints
  expect_lint("c(alpha / 2)", lint_msg, linter_strict)
  expect_lint("c(paste0('.', 1:2))", lint_msg, linter_strict)
  expect_lint("c(DF[cond > 1, col])", lint_msg, linter_strict)
})

test_that("sequences with : are linted whenever a constant is involved", {
  linter <- unnecessary_concatenation_linter()
  linter_strict <- unnecessary_concatenation_linter(allow_single_expression = FALSE)
  const_msg <- rex::rex("Remove unnecessary c() of a constant.")
  expr_msg <- rex::rex("Remove unnecessary c() of a constant expression.")

  expect_lint("c(1:10)", const_msg, linter)
  expect_lint("c(1:sum(x))", const_msg, linter)

  # this is slightly different if a,b are factors, in which case : does
  #   something like interaction
  expect_no_lint("c(a:b)", linter)
  expect_lint("c(a:b)", expr_msg, linter_strict)
  expect_no_lint("c(a:foo(b))", linter)
  expect_lint("c(a:foo(b))", expr_msg, linter_strict)
})

test_that("c(...) does not lint under !allow_single_expression", {
  expect_no_lint("c(...)", unnecessary_concatenation_linter(allow_single_expression = FALSE))
})

test_that("invalid allow_single_expression argument produce informative error messages", {
  expect_error(
    expect_no_lint("c()", unnecessary_concatenation_linter(allow_single_expression = 1.0)),
    rex::rex("`allow_single_expression` must be a single logical value.")
  )

  expect_error(
    expect_no_lint("c()", unnecessary_concatenation_linter(allow_single_expression = c(TRUE, FALSE))),
    rex::rex("`allow_single_expression` must be a single logical value.")
  )
})
