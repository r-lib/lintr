test_that("pipe_return_linter skips allowed usages", {
  linter <- pipe_return_linter()

  normal_pipe_lines <- trim_some("
    x %>%
      filter(str > 5) %>%
      summarize(str = sum(str))
  ")
  expect_no_lint(normal_pipe_lines, linter)

  normal_function_lines <- trim_some("
    pipeline <- function(x) {
      out <- x %>%
        filter(str > 5) %>%
        summarize(str = sum(str))
      return(out)
    }
  ")
  expect_no_lint(normal_function_lines, linter)

  nested_return_lines <- trim_some("
    pipeline <- function(x) {
      x_squared <- x %>%
        sapply(function(xi) {
          return(xi ** 2)
        })
      return(x_squared)
    }
  ")
  expect_no_lint(nested_return_lines, linter)
})

test_that("pipe_return_linter blocks simple disallowed usages", {
  lines <- trim_some("
    pipeline <- function(x) {
      out <- x %>%
        filter(str > 5) %>%
        summarize(str = sum(str)) %>%
        return()
    }
  ")
  expect_lint(
    lines,
    rex::rex("Avoid return() as the final step of a magrittr pipeline"),
    pipe_return_linter()
  )
})

test_that("lints vectorize", {
  lint_msg <- rex::rex("Avoid return() as the final step of a magrittr pipeline")

  expect_lint(
    trim_some("{
      function(x) {
        x %>% return()
      }
      function(y) {
        y %>% return()
      }
    }"),
    list(
      list(lint_msg, line_number = 3L),
      list(lint_msg, line_number = 6L)
    ),
    pipe_return_linter()
  )
})
