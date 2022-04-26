test_that("brace_linter lints braces correctly", {
  open_curly_msg <- rex::rex(
    "Opening curly braces should never go on their own line and should always be followed by a new line."
  )
  closed_curly_msg <- rex::rex(paste(
    "Closing curly-braces should always be on their own line,",
    "unless they are followed by an else."
  ))

  linter <- brace_linter()
  expect_lint("blah", NULL, linter)
  expect_lint("a <- function() {\n}", NULL, linter)
  expect_lint("a <- function() {  \n}", NULL, linter)

  expect_lint("a <- function() { 1 }", list(open_curly_msg, closed_curly_msg), linter)
  # allowed by allow_single_line
  expect_lint("a <- function() { 1 }", NULL, brace_linter(allow_single_line = TRUE))

  expect_lint(
    trim_some("
      a <- if(1) {
        1} else {
        2
      }
    "),
    closed_curly_msg,
    linter
  )
  expect_lint(
    trim_some("
      a <- if(1) {
        1
      } else {
        2}
    "),
    closed_curly_msg,
    linter
  )

  expect_lint(
    trim_some("
      a <- if(1) {
        1} else {
        2}
    "),
    list(
      closed_curly_msg,
      closed_curly_msg
    ),
    linter
  )

  # }) is allowed
  expect_lint("eval(bquote({\n...\n}))", NULL, linter)

  # }, is allowed
  expect_lint(
    trim_some("
      fun({
        statements
      }, param)"),
    NULL,
    linter
  )
  expect_lint(
    trim_some("
      fun(function(a) {
        statements
      }, param)"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      out <- lapply(stuff, function(i) {
        do_something(i)
      }) %>% unlist
    "),
    NULL,
    linter
  )

  # {{ }} is allowed
  expect_lint("{{ x }}", NULL, linter)

  expect_lint(
    trim_some("
      pkg_name <- function(path = find_package()) {
        if (is.null(path)) {
          return(NULL)
        } else {
          read.dcf(file.path(path, \"DESCRIPTION\"), fields = \"Package\")[1]
        }
      }
    "),
    NULL,
    linter
  )

  expect_lint("a <- function()\n{\n  1 \n}", open_curly_msg, linter)
  expect_lint("a <- function()\n    {\n  1 \n}", open_curly_msg, linter)
  expect_lint("a <- function()\n\t{\n  1 \n}", open_curly_msg, linter)

  # trailing comments are allowed
  expect_lint(
    trim_some('
      if ("P" != "NP") { # what most people expect
        print("Cryptomania is possible")
      }
    '),
    NULL,
    linter
  )
})

test_that("brace_linter lints spaces before open braces", {
  linter <- brace_linter()
  msg <- rex::rex("There should be a space before an opening curly brace.")

  expect_lint(
    "blah <- function(){\n}",
    list(
      message = msg,
      column_number = 19L
    ),
    linter
  )

  expect_lint(
    "\nblah <- function(){\n\n\n}",
    list(
      message = msg,
      column_number = 19L
    ),
    linter
  )

  # should also lint if/else
  expect_lint(
    "a <- if (a){\n} else{\n}",
    list(
      list(message = msg, line_number = 1L, column_number = 12L),
      list(message = msg, line_number = 2L, column_number = 7L)
    ),
    linter
  )

  # should lint repeat{
  expect_lint(
    "repeat{\nblah\n}",
    list(message = msg, line_number = 1L, column_number = 7L),
    linter
  )

  # should ignore strings and comments, as in regexes:
  expect_lint("grepl('(iss){2}', 'Mississippi')", NULL, linter)
  expect_lint(
    "x <- 123 # dont flag (paren){brace} if inside a comment",
    NULL,
    linter
  )
  # should not be thrown when the brace lies on subsequent line
  expect_lint(
    trim_some("
      x <- function()
                     {2}
    "),
    list(
      rex::rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
      rex::rex("Closing curly-braces should always be on their own line, unless they are followed by an else.")
    ), #, but not msg
    linter
  )
})

test_that("brace_linter lints else correctly", {
  linter <- brace_linter()
  expect_lint("if (TRUE) 1 else 2", NULL, linter)
  expect_lint("if (TRUE) 1", NULL, linter)

  lines_brace <- trim_some("
    if (TRUE) {
      1
    } else {
      2
    }
  ")
  expect_lint(lines_brace, NULL, linter)

  # such usage is also not allowed by the style guide, but test anyway
  lines_unbrace <- trim_some("
    foo <- function(x) {
      if (TRUE)
        1
      else
        2
    }
  ")
  expect_lint(lines_unbrace, NULL, linter)

  lines <- trim_some("
    foo <- function(x) {
      if (x) {
        1
      }
      else {
        2
      }
    }
  ")
  expect_lint(
    lines,
    rex::rex("`else` should come on the same line as the previous `}`."),
    linter
  )
})

test_that("brace_linter lints function expressions correctly", {
  linter <- brace_linter()
  expect_lint("function(x) 4", NULL, linter)

  lines <- trim_some("
    function(x) {
      x + 4
    }
  ")
  expect_lint(lines, NULL, linter)

  lines <- trim_some("
    function(x)
      x+4
  ")
  expect_lint(
    lines,
    rex::rex("Any function spanning multiple lines should use curly braces."),
    linter
  )
})

test_that("brace_linter lints if/else matching braces correctly", {
  linter <- brace_linter()
  expect_lint("if (TRUE) 1 else 2", NULL, linter)
  expect_lint("if (TRUE) 1", NULL, linter)

  lines_brace <- trim_some("
    if (TRUE) {
      1
    } else {
      2
    }
  ")
  expect_lint(lines_brace, NULL, linter)

  # such usage is also not allowed by the style guide, but test anyway
  lines_unbrace <- trim_some("
    foo <- function(x) {
      if (TRUE)
        1
      else
        2
    }
  ")
  expect_lint(lines_unbrace, NULL, linter)

  # else if is OK
  lines_else_if <- trim_some("
    if (x) {
     1
    } else if (y) {
     2
    } else {
     3
    }
  ")
  expect_lint(lines_else_if, NULL, linter)

  lines_if <- trim_some("
    foo <- function(x) {
      if (x) {
        1
      } else 2
    }
  ")
  expect_lint(
    lines_if,
    rex::rex("Either both or neither branch in `if`/`else` should use curly braces."),
    linter
  )

  lines_else <- trim_some("
    foo <- function(x) {
      if (x) 1 else {
        2
      }
    }
  ")
  expect_lint(
    lines_else,
    rex::rex("Either both or neither branch in `if`/`else` should use curly braces."),
    linter
  )
})
