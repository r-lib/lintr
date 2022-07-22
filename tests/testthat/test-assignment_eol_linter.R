test_that("assignment_eol_linter skips assignments mid-line", {
  expect_lint("x <- y", NULL, assignment_eol_linter())
  expect_lint("foo(bar = 1)", NULL, assignment_eol_linter())
})

test_that("assignment_eol_linter flags assignments end of line", {
  expect_lint("x <<-\ny", "<<-", assignment_eol_linter())
  expect_lint("foo(bar =\n1)", "=", assignment_eol_linter())
})

test_that("assignment_eol_linter flags commented end of line assignments by default", {
  expect_lint("# x <<-\n# y", "<<-", assignment_eol_linter())
  expect_lint("# foo(bar =\n# 1)", "=", assignment_eol_linter())
})

test_that("assignment_eol_linter skips commented end of line assignments", {
  expect_lint("# x <<-\n# y", NULL, assignment_eol_linter(allow_comments = TRUE))
  expect_lint("# foo(bar =\n# 1)", NULL, assignment_eol_linter(allow_comments = TRUE))
})

test_that("assignment_eol_linter flags multiple instances in a file", {
  expect_lint(
    "\n\nblah=\n42\nblh2<-\n54",
    list(
      list(message = "=", line_number = 3L, column_number = 1L),
      list(message = "<-", line_number = 5L, column_number = 1L)
    ),
    assignment_eol_linter
  )
})
