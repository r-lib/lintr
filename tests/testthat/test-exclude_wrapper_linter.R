test_that("exclude_wrapper_linter create one wrapped linters", {
  linter <- exclude_wrapper_linter(line_length_linter(80L), exclusions = "inst/")

  expect_lint("blah", NULL, linter)
  expect_lint(strrep("x", 80L), NULL, linter)

  lint_msg <- rex::rex("Lines should not be more than 80 characters")

  expect_lint(
    strrep("x", 81L),
    list(
      message = lint_msg,
      column_number = 81L
    ),
    linter
  )
})

test_that("exclude_wrapper_linter create multi wrapped linters", {
  linter <- exclude_wrapper_linter(line_length_linter(80L), T_and_F_symbol_linter(), exclusions = "inst/")

  expect_lint("blah\nTRUE", NULL, linter)
  expect_lint(paste0(strrep("x", 80L), "\nFALSE\nTRUE"), NULL, linter)

  expect_lint(
    paste0(strrep("x", 81L), "\nF\nT"),
    list(
      list(
        message = rex::rex("Lines should not be more than 80 characters"),
        column_number = 81L
      ),
      list(message = "Use FALSE instead of the symbol F.", line_number = 2L, column_number = 2L),
      list(message = "Use TRUE instead of the symbol T.", line_number = 3L, column_number = 2L)
    ),
    linter
  )
})

test_that("exclude from linting", {
  lines <- c(strrep("x", 81L), "F", "T")
  file <- withr::local_tempfile(lines = lines)
  text <- paste0(lines, collapse = "\n")
  file <- normalizePath(file)
  linter <- exclude_wrapper_linter(line_length_linter(80L), T_and_F_symbol_linter(),
                                   exclusions = c(file))

  lint_from_file <- lint(file, linters = linter)

  expect_identical(lint_from_file, structure(list(), class = c("lints", "list")))
})
