test_that("unnecessary_nesting_linter skips allowed usages", {
  linter <- unnecessary_nesting_linter()

  # parallel stops() and return()s are OK
  double_stop_lines <- c(
    "if (A) {",
    " stop()",
    "} else {",
    " stop()",
    "}"
  )
  expect_lint(double_stop_lines, NULL, linter)

  double_return_lines <- c(
    "if (A) {",
    " return()",
    "} else {",
    " return()",
    "}"
  )
  expect_lint(double_return_lines, NULL, linter)
})

test_that("parallel stop()/warning() branches are OK", {
  stop_warning_lines <- c(
    "if (i == force.iter) {",
    "  stop(msg, call. = FALSE)",
    "} else {",
    "  warning(attempt, call. = FALSE)",
    "}"
  )
  expect_lint(stop_warning_lines, NULL, unnecessary_nesting_linter())
})

# TODO(michaelchirico): consider if there's a nice easy pattern to enforce for
#   multiple if/else cases. This test in particular would be easy to un-nest,
#   but it's not true in general.
test_that("Multiple if/else statements don't require unnesting", {
  # with further branches, reducing nesting might be less readable
  if_else_if_else_lines <- c(
    "if (x == 'a') {",
    "  stop()",
    "} else if (x == 'b') {",
    "  do_b()",
    "} else {",
    "  stop()",
    "}"
  )
  expect_lint(if_else_if_else_lines, NULL, unnecessary_nesting_linter())
})

test_that("else-less if statements don't lint", {
  multi_statement_if_lines <- c(
    "if (x == 4) {",
    "  msg <- 'failed'",
    "  stop(msg)",
    "}"
  )
  expect_lint(multi_statement_if_lines, NULL, unnecessary_nesting_linter())
})

test_that("non-terminal expressions are not considered for the logic", {
  multi_statement_if_lines <- c(
    "if (x == 4) {",
    "  x <- 5",
    "  return(x)",
    "} else {",
    "  return(x)",
    "}"
  )
  expect_lint(multi_statement_if_lines, NULL, unnecessary_nesting_linter())
})

test_that("parallels in further nesting are skipped", {
  terminal_if_else_lines <- c(
    "if (length(bucket) > 1) {",
    "  return(age)",
    "} else {",
    "  if (grepl('[0-9]', age)) {",
    "    return(age)",
    "  } else {",
    "    return('unknown')",
    "  }",
    "}"
  )
  expect_lint(terminal_if_else_lines, NULL, unnecessary_nesting_linter())
})

test_that("unnecessary_nesting_linter blocks if/else with one exit branch", {
  linter <- unnecessary_nesting_linter()

  if_stop_lines <- c(
    "if (A) {",
    "  stop()",
    "} else {",
    "  B",
    "}"
  )
  expect_lint(
    if_stop_lines,
    rex::rex("Reduce the nesting of this if/else statement by unnesting the portion"),
    linter
  )

  if_return_lines <- c(
    "if (A) {",
    "  return()",
    "} else {",
    "  B",
    "}"
  )
  expect_lint(
    if_return_lines,
    rex::rex("Reduce the nesting of this if/else statement by unnesting the portion"),
    linter
  )

  # also find exits in the later branch
  else_stop_lines <- c(
    "if (A) {",
    "  B",
    "} else {",
    "  stop()",
    "}"
  )
  expect_lint(
    else_stop_lines,
    rex::rex("Reduce the nesting of this if/else statement by unnesting the portion"),
    linter
  )

  else_return_lines <- c(
    "if (A) {",
    "  B",
    "} else {",
    "  return()",
    "}"
  )
  expect_lint(
    else_return_lines,
    rex::rex("Reduce the nesting of this if/else statement by unnesting the portion"),
    linter
  )
})

test_that("unnecessary_nesting_linter skips one-line functions", {
  linter <- unnecessary_nesting_linter()

  anonymous_function_lines <- c(
    "foo <- function(x) {",
    "  return(x)",
    "}"
  )
  expect_lint(anonymous_function_lines, NULL, linter)

  # purrr anonymous functions also get skipped
  purrr_function_lines <- c(
    "purrr::map(x, ~ {",
    "  .x",
    "})"
  )
  expect_lint(purrr_function_lines, NULL, linter)
})

test_that("unnecessary_nesting_linter skips one-expression for loops", {
  linter <- unnecessary_nesting_linter()

  for_lines <- c(
    "for (i in 1:10) {",
    "  print(i)",
    "}"
  )
  expect_lint(for_lines, NULL, linter)

  # also for extended control flow functionality from packages
  foreach_lines <- c(
    "foreach (i = 1:10) %dopar% {",
    "  print(i)",
    "}"
  )
  expect_lint(foreach_lines, NULL, linter)
})

test_that("unnecessary_nesting_linter skips one-expression if and else clauses", {
  lines <- c(
    "if (TRUE) {",
    "  x",
    "} else {",
    "  y",
    "}"
  )
  expect_lint(lines, NULL, unnecessary_nesting_linter())
})

test_that("unnecessary_nesting_linter skips one-expression while loops", {
  lines <- c(
    "while (x < 10) {",
    "  x <- x + 1",
    "}"
  )
  expect_lint(lines, NULL, unnecessary_nesting_linter())
})

test_that("unnecessary_nesting_linter skips one-expression repeat loops", {
  lines <- c(
    "repeat {",
    "  x <- x + 1",
    "}"
  )
  expect_lint(lines, NULL, unnecessary_nesting_linter())
})

test_that("unnecessary_nesting_linter skips one-expression switch statements", {
  lines <- c(
    "switch(x,",
    "  a = {",
    "    do_a()",
    "  },",
    "  b = {",
    "    do_b()",
    "  }",
    ")"
  )
  expect_lint(lines, NULL, unnecessary_nesting_linter())
})

test_that("unnecessary_nesting_linter passes for multi-line braced expressions", {
  lines <- c(
    "tryCatch(",
    "  {",
    "    foo(x)",
    "    bar(x)",
    "  },",
    "  error = identity",
    ")"
  )
  expect_lint(lines, NULL, unnecessary_nesting_linter())
})

test_that("unnecessary_nesting_linter skips if unbracing won't reduce nesting", {
  linter <- unnecessary_nesting_linter()

  test_that_lines <- c(
    "test_that('this works', {",
    "  expect_true(TRUE)",
    "})"
  )
  expect_lint(test_that_lines, NULL, linter)
  data_table_lines <- c(
    "DT[, {",
    "  plot(x, y)",
    "}]"
  )
  expect_lint(data_table_lines, NULL, linter)
  data_table_assign_lines <- c(
    "DT[, x := {",
    "  foo(x, y)",
    "}]"
  )
  expect_lint(data_table_assign_lines, NULL, linter)
})

test_that("rlang's double-brace operator is skipped", {
  expect_lint(
    "rename(DF, col = {{ val }})",
    NULL,
    unnecessary_nesting_linter()
  )
})

test_that("unnecessary_nesting_linter blocks one-expression braced expressions", {
  linter <- unnecessary_nesting_linter()
  lint_msg <- rex::rex("Reduce the nesting of this statement by removing the braces {}.")

  expect_lint(
    trim_some("
      tryCatch(
        {
          foo(x)
        },
        error = identity
      )
    "),
    lint_msg,
    linter
  )

  # NB: styler would re-style this anyway
  expect_lint(
    trim_some("
      tryCatch({
        foo()
      }, error = identity)
    "),
    NULL,
    linter
  )
})
