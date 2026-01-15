test_that("vector_logic_linter skips allowed usages", {
  linter <- vector_logic_linter()

  expect_no_lint("if (TRUE) 5 else if (TRUE) 2", linter)
  expect_no_lint("if (TRUE || FALSE) 1; while (TRUE && FALSE) 2", linter)

  # function calls and extractions may aggregate to scalars -- only catch
  #   usages at the highest logical level
  expect_no_lint("if (agg_function(x & y)) 1", linter)
  expect_no_lint("if (DT[x | y, cond]) 1", linter)

  # don't match potentially OK usages nested within calls
  expect_no_lint("if (TRUE && any(TRUE | FALSE)) 4", linter)
  # even if the usage is nested in those calls (b/181915948)
  expect_no_lint("if (TRUE && any(TRUE | FALSE | TRUE)) 4", linter)

  # don't match potentially OK usages in the branch itself
  expect_no_lint(
    trim_some("
      if (TRUE) {
        x | y
      }
    "),
    linter
  )

  # valid nested usage within aggregator
  expect_no_lint("testthat::expect_false(any(TRUE | TRUE))", linter)
})

test_that("vector_logic_linter blocks simple disallowed usages", {
  linter <- vector_logic_linter()

  expect_lint("if (TRUE & FALSE) 1", rex::rex("Use `&&` in conditional expressions."), linter)
  expect_lint("while (TRUE | TRUE) 2", rex::rex("Use `||` in conditional expressions."), linter)
})

test_that("vector_logic_linter detects nested conditions", {
  linter <- vector_logic_linter()

  expect_lint(
    "if (TRUE & TRUE || FALSE) 4",
    list(rex::rex("Use `&&` in conditional expressions."), column_number = 10L),
    linter
  )
  expect_lint(
    "if (TRUE && (TRUE | FALSE)) 4",
    list(rex::rex("Use `||` in conditional expressions."), column_number = 19L),
    linter
  )
})

test_that("vector_logic_linter catches usages in expect_true()/expect_false()", {
  linter <- vector_logic_linter()
  and_msg <- rex::rex("Use `&&` in conditional expressions.")
  or_msg <- rex::rex("Use `||` in conditional expressions.")

  expect_lint("expect_true(TRUE & FALSE)", and_msg, linter)
  expect_lint("expect_false(TRUE | TRUE)", or_msg, linter)

  # ditto with namespace qualification
  expect_lint("testthat::expect_true(TRUE & FALSE)", and_msg, linter)
  expect_lint("testthat::expect_false(TRUE | TRUE)", or_msg, linter)
})

test_that("vector_logic_linter doesn't get mixed up from complex usage", {
  expect_no_lint(
    trim_some("
      if (a) {
        expect_true(ok)
        x <- 2
        a | b
      }
    "),
    vector_logic_linter()
  )
})

test_that("vector_logic_linter recognizes some false positves around bitwise &/|", {
  linter <- vector_logic_linter()

  expect_no_lint("if (info & as.raw(12)) { }", linter)
  expect_no_lint("if (as.raw(12) & info) { }", linter)
  expect_no_lint("if (info | as.raw(12)) { }", linter)
  expect_no_lint("if (info & as.octmode('100')) { }", linter)
  expect_no_lint("if (info | as.octmode('011')) { }", linter)
  expect_no_lint("if (info & as.hexmode('100')) { }", linter)
  expect_no_lint("if (info | as.hexmode('011')) { }", linter)
  # implicit as.octmode() coercion
  expect_no_lint("if (info & '100') { }", linter)
  expect_no_lint("if (info | '011') { }", linter)
  expect_no_lint("if ('011' | info) { }", linter)

  # further nesting
  expect_no_lint("if ((info & as.raw(12)) == as.raw(12)) { }", linter)
  expect_no_lint("if ((info | as.raw(12)) == as.raw(12)) { }", linter)
  expect_no_lint('if ((mode & "111") != as.octmode("111")) { }', linter)
  expect_no_lint('if ((mode | "111") != as.octmode("111")) { }', linter)
  expect_no_lint('if ((mode & "111") != as.hexmode("111")) { }', linter)
  expect_no_lint('if ((mode | "111") != as.hexmode("111")) { }', linter)
})

test_that("incorrect subset/filter usage is caught", {
  linter <- vector_logic_linter()
  and_msg <- rex::rex("Use `&` in subsetting expressions")
  or_msg <- rex::rex("Use `|` in subsetting expressions")

  expect_lint("filter(x, y && z)", and_msg, linter)
  expect_lint("filter(x, y || z)", or_msg, linter)
  expect_lint("subset(x, y && z)", and_msg, linter)
  expect_lint("subset(x, y || z)", or_msg, linter)

  expect_lint("x %>% filter(y && z)", and_msg, linter)
  expect_lint("filter(x, a & b, c | d, e && f)", list(and_msg, column_number = 27L), linter)
})

test_that("native pipe usage is caught in subset/filter logic", {
  expect_lint("x |> filter(y && z)", rex::rex("Use `&` in subsetting"), vector_logic_linter())
})

test_that("subsetting logic handles nesting", {
  linter <- vector_logic_linter()
  and_msg <- rex::rex("Use `&` in subsetting expressions")
  or_msg <- rex::rex("Use `|` in subsetting expressions")

  expect_lint("filter(x, a & b || c)", or_msg, linter)
  expect_lint("filter(x, a && b | c)", and_msg, linter)

  # adversarial commenting
  expect_lint(
    trim_some("
      filter(x, a #comment
      && b | c)
    "),
    and_msg,
    linter
  )

  expect_lint(
    trim_some("
      filter(x, a && #comment
      b | c)
    "),
    and_msg,
    linter
  )

  # but not valid usage
  expect_no_lint("filter(x, y < mean(y, na.rm = AA && BB))", linter)
  expect_no_lint("subset(x, y < mean(y, na.rm = AA && BB) & y > 0)", linter)
  expect_no_lint("subset(x, y < x[y > 0, drop = AA && BB, y])", linter)
})

test_that("scalar logic in anonymous functions within filter/subset is allowed", {
  linter <- vector_logic_linter()

  expect_no_lint("filter(x, vapply(y, function(i) is.numeric(i) && mean(i) > 1, NA))", linter)
  expect_no_lint("subset(x, sapply(y, function(i) is.null(i) || length(i) == 0))", linter)
  expect_no_lint("filter(x, vapply(y, \\(i) i && z, NA))", linter)
  expect_no_lint("filter(x, Map(function(a, b) a && b, x, y))", linter)
  expect_no_lint("filter(x, vapply(y, function(i) any(sapply(i, function(j) j && z)), NA))", linter)
})

test_that("vector logic in if conditions inside anonymous functions is still linted", {
  linter <- vector_logic_linter()
  or_msg <- rex::rex("Use `||` in conditional expressions.")
  and_msg <- rex::rex("Use `&&` in conditional expressions.")

  expect_lint(
    "subset(x, sapply(col, function(x) { if (A | B) do_ab(x) else do_other(x) }))",
    or_msg,
    linter
  )
  expect_lint(
    "filter(x, vapply(y, function(i) { if (a & b) 1 else 0 }, 1))",
    and_msg,
    linter
  )
  expect_lint(
    "subset(x, sapply(col, \\(x) { if (A | B) x }))",
    or_msg,
    linter
  )
  expect_lint(
    "subset(x, sapply(col, function(x) { while (A | B) x }))",
    or_msg,
    linter
  )
  expect_no_lint("subset(x, sapply(col, function(x) { if (any(a | b)) x }))", linter)
})

test_that("filter() handling is conservative about stats::filter()", {
  linter <- vector_logic_linter()
  and_msg <- rex::rex("Use `&` in subsetting expressions")

  # NB: this should be invalid, filter= is a vector argument
  expect_no_lint("stats::filter(x, y && z)", linter)
  # The only logical argument to stats::filter(), exclude by keyword
  expect_no_lint("filter(x, circular = y && z)", linter)
  # But presence of circular= doesn't invalidate lint
  expect_lint("filter(x, circular = TRUE, y && z)", and_msg, linter)
  expect_lint("filter(x, y && z, circular = TRUE)", and_msg, linter)
  expect_no_lint(
    trim_some("
      filter(x, circular # comment
      = y && z)
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      filter(x, circular = # comment
        y && z)
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      filter(x, circular # comment
      = # comment
      y && z)
    "),
    linter
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      if (AA & BB) {}
      if (CC | DD) {}
      filter(x, EE && FF)
      subset(y, GG || HH)
    }"),
    list(
      list(rex::rex("`&&`"), line_number = 2L),
      list(rex::rex("`||`"), line_number = 3L),
      list(rex::rex("`&`"), line_number = 4L),
      list(rex::rex("`|`"), line_number = 5L)
    ),
    vector_logic_linter()
  )
})
