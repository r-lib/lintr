test_that("vector_logic_linter skips allowed usages", {
  expect_lint("if (TRUE) 5 else if (TRUE) 2", NULL, vector_logic_linter())

  expect_lint("if (TRUE || FALSE) 1; while (TRUE && FALSE) 2", NULL, vector_logic_linter())

  # function calls and extractions may aggregate to scalars -- only catch
  #   usages at the highest logical level
  expect_lint("if (agg_function(x & y)) 1", NULL, vector_logic_linter())
  expect_lint("if (DT[x | y, cond]) 1", NULL, vector_logic_linter())
})

test_that("vector_logic_linter blocks simple disallowed usages", {
  expect_lint(
    "if (TRUE & FALSE) 1",
    rex::rex("Conditional expressions require scalar logical operators"),
    vector_logic_linter()
  )

  expect_lint(
    "while (TRUE | TRUE) 2",
    rex::rex("Conditional expressions require scalar logical operators"),
    vector_logic_linter()
  )
})

test_that("vector_logic_linter detects nested conditions", {
  expect_lint(
    "if (TRUE & TRUE || FALSE) 4",
    rex::rex("Conditional expressions require scalar logical operators"),
    vector_logic_linter()
  )

  expect_lint(
    "if (TRUE && (TRUE | FALSE)) 4",
    rex::rex("Conditional expressions require scalar logical operators"),
    vector_logic_linter()
  )

  # don't match potentially OK usages nested within calls
  expect_lint("if (TRUE && any(TRUE | FALSE)) 4", NULL, vector_logic_linter())
  # even if the usage is nested in those calls (b/181915948)
  expect_lint("if (TRUE && any(TRUE | FALSE | TRUE)) 4", NULL, vector_logic_linter())

  # don't match potentially OK usages in the branch itself
  lines <- trim_some("
    if (TRUE) {
      x | y
    }
  ")
  expect_lint(lines, NULL, vector_logic_linter())
})

test_that("vector_logic_linter catches usages in expect_true()/expect_false()", {
  expect_lint(
    "expect_true(TRUE & FALSE)",
    rex::rex("Conditional expressions require scalar logical operators"),
    vector_logic_linter()
  )

  expect_lint(
    "expect_false(TRUE | TRUE)",
    rex::rex("Conditional expressions require scalar logical operators"),
    vector_logic_linter()
  )

  # ditto with namespace qualification
  expect_lint(
    "testthat::expect_true(TRUE & FALSE)",
    rex::rex("Conditional expressions require scalar logical operators"),
    vector_logic_linter()
  )

  expect_lint(
    "testthat::expect_false(TRUE | TRUE)",
    rex::rex("Conditional expressions require scalar logical operators"),
    vector_logic_linter()
  )

  # valid nested usage within aggregator
  expect_lint("testthat::expect_false(any(TRUE | TRUE))", NULL, vector_logic_linter())
})
