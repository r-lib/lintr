test_that("trailing_assignment_linter skips assignments mid-line", {
  expect_lint("x <- y", NULL, trailing_assignment_linter())
  expect_lint("foo(bar = 1)", NULL, trailing_assignment_linter())
})

test_that("trailing_assignment_linter flags assignments end of line", {
  expect_lint("x <<-\ny", "<<-", trailing_assignment_linter())
  expect_lint("foo(bar =\n1)", "=", trailing_assignment_linter())
})

test_that("trailing_assignment_linter flags assignments end of line with trailing spaces", {
  expect_lint("x <<-  \ny", "<<-", trailing_assignment_linter())
  expect_lint("foo(bar =\t\n1)", "=", trailing_assignment_linter())
})

test_that("trailing_assignment_linter flags commented end of line assignments by default", {
  expect_lint("# x <<-\n# y", "<<-", trailing_assignment_linter())
  expect_lint("# foo(bar =\n# 1)", "=", trailing_assignment_linter())
})

test_that("trailing_assignment_linter skips commented end of line assignments", {
  expect_lint("# x <<-\n# y", NULL, trailing_assignment_linter(allow_comments = TRUE))
  expect_lint("# foo(bar =\n# 1)", NULL, trailing_assignment_linter(allow_comments = TRUE))
})

test_that("trailing_assignment_linter allows left assignment at end of line when using pipes", {
  expect_lint(
    "iris_long <-\n  iris %>%\n  gather(measure, value, -Species) %>%\n  arrange(-value)",
    NULL,
    trailing_assignment_linter()
  )
})

test_that("trailing_assignment_linter allows right assignment at end of line when using pipes", {
  expect_lint(
    "iris %>%\n  gather(measure, value, -Species) %>%\n  arrange(-value) ->\n  iris_long",
    NULL,
    trailing_assignment_linter()
  )

  expect_lint(
    "iris %>%\n  gather(measure, value, -Species) %>%\n  arrange(\n    -value\n    ) ->>\n  iris_long",
    NULL,
    trailing_assignment_linter()
  )
})

test_that("trailing_assignment_linter flags left assignment when new line pipes aren't allowed", {
  expect_lint(
    "iris_long <-\n  iris %>%\n  gather(measure, value, -Species) %>%\n  arrange(-value)",
    "<-",
    trailing_assignment_linter(allow_piping = FALSE)
  )
})

test_that("trailing_assignment_linter flags right assignment when new line pipes aren't allowed", {
  expect_lint(
    "iris %>%\n  gather(measure, value, -Species) %>%\n  arrange(\n    -value\n    ) ->>\n  iris_long",
    "->>",
    trailing_assignment_linter(allow_piping = FALSE)
  )
})

test_that("trailing_assignment_linter flags multiple instances in a file", {
  expect_lint(
    "\n\nblah=\n42\nblh2<-\n54",
    list(
      list(message = "=", line_number = 3L, column_number = 1L),
      list(message = "<-", line_number = 5L, column_number = 1L)
    ),
    trailing_assignment_linter()
  )
})
