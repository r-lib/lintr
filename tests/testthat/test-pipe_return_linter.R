test_that("pipe_return_linter skips allowed usages", {
  linter <- pipe_return_linter()

  normal_pipe_lines <- c(
    "x %>%",
    "  filter(str > 5) %>%",
    "  summarize(str = sum(str))"
  )
  expect_lint(normal_pipe_lines, NULL, linter)

  normal_function_lines <- c(
    "pipeline <- function(x) {",
    "  out <- x %>%",
    "    filter(str > 5) %>%",
    "    summarize(str = sum(str))",
    "  return(out)",
    "}"
  )
  expect_lint(normal_function_lines, NULL, linter)

  nested_return_lines <- c(
    "pipeline <- function(x) {",
    "  x_squared <- x %>%",
    "    sapply(function(xi) {",
    "      return(xi ** 2)",
    "    })",
    "  return(x_squared)",
    "}"
  )
  expect_lint(nested_return_lines, NULL, linter)
})

test_that("pipe_return_linter blocks simple disallowed usages", {
  lines <- c(
    "pipeline <- function(x) {",
    "  out <- x %>%",
    "    filter(str > 5) %>%",
    "    summarize(str = sum(str)) %>%",
    "    return()",
    "}"
  )
  expect_lint(
    lines,
    rex::rex("Using return() as the final step of a magrittr pipeline"),
    pipe_return_linter()
  )
})
