test_that("function_brace_linter skips allowed usages", {
  expect_lint("function(x) 4", NULL, function_brace_linter())

  lines <- trim_some("
    function(x) {
      x + 4
    }
  ")
  expect_lint(lines, NULL, function_brace_linter())
})

test_that("function_brace_linter blocks disallowed usage", {
  lines <- trim_some("
    function(x)
      x+4
  ")
  expect_lint(
    lines,
    rex::rex("Any function spanning multiple lines must use curly braces."),
    function_brace_linter()
  )
})
