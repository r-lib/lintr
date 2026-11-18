lint_msg <- function(marker) rex::rex("Put a space after the comment marker `", marker, "`.")
lint_msg_one <- function(marker) rex::rex("Put only one space after the comment marker `", marker, "`.")
lint_msg_none <- function(marker) rex::rex("Remove the space after the comment marker `", marker, "`.")

test_that("comment_spacing_linter skips allowed usages", {
  linter <- comment_spacing_linter()

  expect_no_lint("# comment", linter)
  expect_no_lint("x <- 1 # comment", linter)
  expect_no_lint("## comment", linter)
  expect_no_lint("#' @param x A number.", linter)
  expect_no_lint("#* @get /echo", linter)
  # more than one space is fine, e.g. to indent a continuation line
  expect_no_lint("#    (a + b)^2", linter)
  # a comment marker with no text, as used in blank roxygen lines or decorations
  expect_no_lint("#", linter)
  expect_no_lint("#'", linter)
  expect_no_lint("##########", linter)
  # '#' inside a string is not a comment
  expect_no_lint("x <- '#comment'", linter)
})

test_that("comment_spacing_linter skips special comments", {
  linter <- comment_spacing_linter()

  expect_no_lint(
    trim_some("
      #!/usr/bin/env Rscript
      x <- 1
    "),
    linter
  )
  # knitr::spin() chunk headers and text
  expect_no_lint("#+ chunk-label, eval = FALSE", linter)
  expect_no_lint("#- chunk-label", linter)
  # Quarto chunk options
  expect_no_lint("#| echo: false", linter)
  # xaringan highlighting and printed output
  expect_no_lint("x <- 1 #<<", linter)
  expect_no_lint("#> [1] 1", linter)
})

test_that("comment_spacing_linter blocks disallowed usages", {
  linter <- comment_spacing_linter()

  expect_lint("#comment", list(lint_msg("#"), line_number = 1L, column_number = 1L), linter)
  expect_lint("x <- 1 #comment", list(lint_msg("#"), column_number = 8L, ranges = list(c(8L, 15L))), linter)
  expect_lint("##comment", lint_msg("##"), linter)
  expect_lint("#'@param x A number.", lint_msg("#'"), linter)
  expect_lint("#'#comment", lint_msg("#'"), linter)
  # only a shebang on the first line is exempt
  expect_lint(
    trim_some("
      x <- 1
      #!/usr/bin/env Rscript
    "),
    list(lint_msg("#"), line_number = 2L),
    linter
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("
      #'@title Foo
      foo <- function(x) {
        #comment
        x # comment
      } ##comment
    "),
    list(
      list(lint_msg("#'"), line_number = 1L),
      list(lint_msg("#"), line_number = 3L),
      list(lint_msg("##"), line_number = 5L)
    ),
    comment_spacing_linter()
  )
})

test_that("default_style = 'one' requires exactly one space", {
  linter <- comment_spacing_linter(default_style = "one")

  expect_no_lint("# comment", linter)
  expect_no_lint("x <- 1 # comment", linter)
  expect_no_lint("#' @param x A number.", linter)
  expect_no_lint("#", linter)
  expect_no_lint("#> [1] 1", linter)

  expect_lint("#  comment", list(lint_msg_one("#"), line_number = 1L, column_number = 1L), linter)
  expect_lint("x <- 1 #  comment", list(lint_msg_one("#"), column_number = 8L), linter)
  expect_lint("#'  @param x A number.", lint_msg_one("#'"), linter)
  # a missing space is still reported as such
  expect_lint("#comment", lint_msg("#"), linter)
})

test_that("overrides = list(one = ...) and list(one_or_more = ...) work", {
  linter <- comment_spacing_linter(overrides = list(one = "#'"))
  expect_lint("#'  @param x A number.", lint_msg_one("#'"), linter)
  expect_no_lint("#    (a + b)^2", linter)

  linter <- comment_spacing_linter(default_style = "one", overrides = list(one_or_more = "#"))
  expect_no_lint("#    (a + b)^2", linter)
  expect_lint("##  comment", lint_msg_one("##"), linter)
})

test_that("one_or_hanging allows more spaces only to continue the comment above", {
  linter <- comment_spacing_linter(overrides = list(one_or_hanging = "#'"))

  expect_no_lint("#' @param x A number.", linter)
  expect_no_lint(
    trim_some("
      #' @param x A number,
      #'   or a string,
      #'   or NULL.
    "),
    linter
  )
  # a blank comment line still continues the block
  expect_no_lint(
    trim_some("
      #' @examples
      #'
      #'   x <- 1
    "),
    linter
  )

  expect_lint("#'   @param x A number.", list(lint_msg_one("#'"), line_number = 1L), linter)
  # the line above has no comment
  expect_lint(
    trim_some("
      x <- 1
      #'   or a string.
    "),
    list(lint_msg_one("#'"), line_number = 2L),
    linter
  )
  # the comment above has a different marker
  expect_lint(
    trim_some("
      # @param x A number,
      #'   or a string.
    "),
    list(lint_msg_one("#'"), line_number = 2L),
    linter
  )
  # the comment above is in a different column
  expect_lint(
    trim_some("
      x <- 1 #' @param x A number,
      #'   or a string.
    "),
    list(lint_msg_one("#'"), line_number = 2L),
    linter
  )
})

test_that("default_style = 'one_or_hanging' works across expressions", {
  linter <- comment_spacing_linter(default_style = "one_or_hanging")

  expect_no_lint(
    trim_some("
      x <- 1 # the first
      y <- 2 #   and the second
    "),
    linter
  )
  expect_lint(
    trim_some("
      x <- 1 # the first
      y <- 22 #   and the second
    "),
    list(lint_msg_one("#"), line_number = 2L),
    linter
  )
})

test_that("overrides = list(none = ...) works", {
  linter <- comment_spacing_linter(overrides = list(none = "#'"))

  expect_no_lint("#'@export", linter)
  expect_no_lint("#'", linter)
  expect_lint("#' @export", list(lint_msg_none("#'"), line_number = 1L, column_number = 1L), linter)
  expect_lint("#'   @export", lint_msg_none("#'"), linter)
  # other markers keep the default style
  expect_no_lint("# comment", linter)
  expect_lint("#comment", lint_msg("#"), linter)
})

test_that("overrides = list(any = ...) works", {
  linter <- comment_spacing_linter(overrides = list(any = "#'"))

  expect_no_lint("#'@export", linter)
  expect_no_lint("#'  @export", comment_spacing_linter(default_style = "one", overrides = list(any = "#'")))
  # markers match exactly
  expect_lint("##'@export", lint_msg("##'"), linter)
  expect_lint("#comment", lint_msg("#"), linter)
})

test_that("arguments are validated", {
  expect_error(comment_spacing_linter(default_style = "none"), "should be one of")
  expect_error(comment_spacing_linter(overrides = c(any = "#'")), "must be a named list")
  expect_error(comment_spacing_linter(overrides = list("#'")), "must be a named list")
  expect_error(comment_spacing_linter(overrides = list(hanging = "#'")), "must be one of")
  expect_error(comment_spacing_linter(overrides = list(any = "'")), "Unknown comment marker")
  expect_error(comment_spacing_linter(overrides = list(one = "#'", any = "#'")), "given more than once")
})
