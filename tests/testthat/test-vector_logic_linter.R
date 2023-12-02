test_that("vector_logic_linter skips allowed usages", {
  linter <- vector_logic_linter()

  expect_lint("if (TRUE) 5 else if (TRUE) 2", NULL, linter)
  expect_lint("if (TRUE || FALSE) 1; while (TRUE && FALSE) 2", NULL, linter)

  # function calls and extractions may aggregate to scalars -- only catch
  #   usages at the highest logical level
  expect_lint("if (agg_function(x & y)) 1", NULL, linter)
  expect_lint("if (DT[x | y, cond]) 1", NULL, linter)

  # don't match potentially OK usages nested within calls
  expect_lint("if (TRUE && any(TRUE | FALSE)) 4", NULL, linter)
  # even if the usage is nested in those calls (b/181915948)
  expect_lint("if (TRUE && any(TRUE | FALSE | TRUE)) 4", NULL, linter)

  # don't match potentially OK usages in the branch itself
  lines <- trim_some("
    if (TRUE) {
      x | y
    }
  ")
  expect_lint(lines, NULL, linter)


  # valid nested usage within aggregator
  expect_lint("testthat::expect_false(any(TRUE | TRUE))", NULL, linter)
})

test_that("vector_logic_linter blocks simple disallowed usages", {
  linter <- vector_logic_linter()
  lint_msg <- rex::rex("Conditional expressions require scalar logical operators")

  expect_lint("if (TRUE & FALSE) 1", lint_msg, linter)
  expect_lint("while (TRUE | TRUE) 2", lint_msg, linter)
})

test_that("vector_logic_linter detects nested conditions", {
  linter <- vector_logic_linter()
  lint_msg <- rex::rex("Conditional expressions require scalar logical operators")

  expect_lint("if (TRUE & TRUE || FALSE) 4", lint_msg, linter)
  expect_lint("if (TRUE && (TRUE | FALSE)) 4", lint_msg, linter)
})

test_that("vector_logic_linter catches usages in expect_true()/expect_false()", {
  linter <- vector_logic_linter()
  lint_msg <- rex::rex("Conditional expressions require scalar logical operators")

  expect_lint("expect_true(TRUE & FALSE)", lint_msg, linter)
  expect_lint("expect_false(TRUE | TRUE)", lint_msg, linter)

  # ditto with namespace qualification
  expect_lint("testthat::expect_true(TRUE & FALSE)", lint_msg, linter)
  expect_lint("testthat::expect_false(TRUE | TRUE)", lint_msg, linter)
})

test_that("vector_logic_linter doesn't get mixed up from complex usage", {
  expect_lint(
    trim_some("
      if (a) {
        expect_true(ok)
        x <- 2
        a | b
      }
    "),
    NULL,
    vector_logic_linter()
  )
})

test_that("vector_logic_linter recognizes some false positves around bitwise &/|", {
  linter <- vector_logic_linter()

  expect_lint("if (info & as.raw(12)) { }", NULL, linter)
  expect_lint("if (as.raw(12) & info) { }", NULL, linter)
  expect_lint("if (info | as.raw(12)) { }", NULL, linter)
  expect_lint("if (info & as.octmode('100')) { }", NULL, linter)
  expect_lint("if (info | as.octmode('011')) { }", NULL, linter)
  expect_lint("if (info & as.hexmode('100')) { }", NULL, linter)
  expect_lint("if (info | as.hexmode('011')) { }", NULL, linter)
  # implicit as.octmode() coercion
  expect_lint("if (info & '100') { }", NULL, linter)
  expect_lint("if (info | '011') { }", NULL, linter)
  expect_lint("if ('011' | info) { }", NULL, linter)

  # further nesting
  expect_lint("if ((info & as.raw(12)) == as.raw(12)) { }", NULL, linter)
  expect_lint("if ((info | as.raw(12)) == as.raw(12)) { }", NULL, linter)
  expect_lint('if ((mode & "111") != as.octmode("111")) { }', NULL, linter)
  expect_lint('if ((mode | "111") != as.octmode("111")) { }', NULL, linter)
  expect_lint('if ((mode & "111") != as.hexmode("111")) { }', NULL, linter)
  expect_lint('if ((mode | "111") != as.hexmode("111")) { }', NULL, linter)
})
