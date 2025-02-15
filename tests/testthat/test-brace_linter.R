test_that("brace_linter lints braces correctly", {
  open_curly_msg <- rex::rex(
    "Opening curly braces should never go on their own line"
  )
  closed_curly_msg <- rex::rex(
    "Closing curly-braces should always be on their own line, ",
    "unless they are followed by an else."
  )

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
  # }] is too
  expect_lint("df[, {\n...\n}]", NULL, linter)

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

  # ,<\n>{ is allowed
  expect_lint(
    trim_some("
      switch(
        x,
        'a' = do_something(x),
        'b' = do_another(x),
        {
          do_first(x)
          do_second(x)
        }
      )
    "),
    NULL,
    linter
  )

  # a comment before ,<\n>{ is allowed
  expect_lint(
    trim_some("
      switch(
        x,
        'a' = do_something(x),
        'b' = do_another(x), # comment
        {
          do_first(x)
          do_second(x)
        }
      )
    "),
    NULL,
    linter
  )

  # a comment before <\n>{ is allowed
  expect_lint(
    trim_some("
      switch(stat,
      o = {
        x <- 0.01
      },
      # else
      {
        x <- 2
      }
    )
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      fun(
        'This is very very very long text.',
        {
          message('This is the code.')
          message('It\\'s stupid, but proves my point.')
        }
      )
    "),
    NULL,
    linter
  )

  # (\n{ is allowed optionally
  expect_lint(
    trim_some("
      tryCatch(
        {
          print(1)
        },
        error = function(err) {
        }
      )
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

  expect_lint(
    "a <- function()
     # comment
    {
      1
    }",
    open_curly_msg,
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
  lint_msg <- rex::rex("There should be a space before an opening curly brace.")

  expect_lint(
    "blah <- function(){\n}",
    list(
      message = lint_msg,
      column_number = 19L
    ),
    linter
  )

  expect_lint(
    "\nblah <- function(){\n\n\n}",
    list(
      message = lint_msg,
      column_number = 19L
    ),
    linter
  )

  # should also lint if/else
  expect_lint(
    "a <- if (a){\n} else{\n}",
    list(
      list(message = lint_msg, line_number = 1L, column_number = 12L),
      list(message = lint_msg, line_number = 2L, column_number = 7L)
    ),
    linter
  )

  # should lint repeat{
  expect_lint(
    "repeat{\nblah\n}",
    list(message = lint_msg, line_number = 1L, column_number = 7L),
    linter
  )

  # should ignore strings and comments, as in regexes:
  expect_lint("grepl('(iss){2}', 'Mississippi')", NULL, linter)
  expect_lint(
    "x <- 123 # don't flag (paren){brace} if inside a comment",
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
      rex::rex("Opening curly braces should never go on their own line"),
      rex::rex("Closing curly-braces should always be on their own line")
    ), # , but not lint_msg
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
  msg_always <- rex::rex("Wrap function bodies in curly braces.")
  msg_multi_line <- rex::rex("Wrap multi-line function bodies in curly braces.")
  msg_not_inline <- rex::rex("Wrap function bodies starting on a new line in curly braces.")
  msgs_open_close <- list(
    rex::rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
    rex::rex("Closing curly-braces should always be on their own line, unless they are followed by an else.")
  )

  linter_always <- brace_linter(function_bodies = "always")
  linter_multi_line <- brace_linter(function_bodies = "multi_line")
  linter_not_inline <- brace_linter(function_bodies = "not_inline")
  linter_never <- brace_linter(function_bodies = "never")

  lines <- trim_some("
    function(x) {
      x + 4
    }
  ")
  expect_no_lint(lines, linter_always)
  expect_no_lint(lines, linter_multi_line)
  expect_no_lint(lines, linter_not_inline)
  expect_no_lint(lines, linter_never)

  expect_lint("function(x) { x + 4 }", msgs_open_close, linter_always)
  expect_lint("function(x) { x + 4 }", msgs_open_close, linter_multi_line)
  expect_lint("function(x) { x + 4 }", msgs_open_close, linter_not_inline)
  expect_lint("function(x) { x + 4 }", msgs_open_close, linter_never)
  # function_bodies = "always" should only prohibit inline functions with allow_single_line = FALSE (the default):
  expect_no_lint(
    "function(x) { x + 4 }",
    brace_linter(allow_single_line = TRUE, function_bodies = "always")
  )

  expect_lint("function(x) x + 4", msg_always, linter_always)
  expect_no_lint("function(x) x + 4", linter_multi_line)
  expect_no_lint("function(x) x + 4", linter_not_inline)
  expect_no_lint("function(x) x + 4", linter_never)

  lines <- trim_some("
    function(x) x +
      4
  ")
  expect_lint(lines, msg_always, linter_always)
  expect_lint(lines, msg_multi_line, linter_multi_line)
  expect_no_lint(lines, linter_not_inline)
  expect_no_lint(lines, linter_never)

  lines <- trim_some("
    function(x)
      x + 4
  ")
  expect_lint(lines, msg_always, linter_always)
  expect_lint(lines, msg_multi_line, linter_multi_line)
  expect_lint(lines, msg_not_inline, linter_not_inline)
  expect_no_lint(lines, linter_never)

  # missing newline after opening brace; closing brace not on sep line
  lines <- trim_some("
    foo <- function(x) { x +
      4 }
  ")
  expect_lint(lines, msgs_open_close, linter_always)
  expect_lint(lines, msgs_open_close, linter_multi_line)
  expect_lint(lines, msgs_open_close, linter_not_inline)
  expect_lint(lines, msgs_open_close, linter_never)

  # fn body wrapped in additional unneeded parentheses
  lines <- trim_some("
    foo <- function(x) ({
      x + 1
    })
  ")
  expect_lint(lines, msg_always, linter_always)
  expect_lint(lines, msg_multi_line, linter_multi_line)
  expect_no_lint(lines, linter_not_inline)
  expect_no_lint(lines, linter_never)
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

# Keep up to date with https://github.com/tidyverse/style/issues/191
test_that("empty brace expressions are always allowed inline", {
  expect_lint("while (FALSE) {}", NULL, brace_linter())
  expect_lint("while (FALSE) { }", NULL, brace_linter())
  # only applies when `{` is "attached" to the preceding token on the same line
  expect_lint("while (FALSE)\n{}", rex::rex("Opening curly braces"), brace_linter())
  expect_lint("while (FALSE)\n{ }", rex::rex("Opening curly braces"), brace_linter())
  expect_lint("while (FALSE) {}", NULL, brace_linter(allow_single_line = TRUE))
  expect_lint("while (FALSE) { }", NULL, brace_linter(allow_single_line = TRUE))
})

test_that("formula syntax is linted properly", {
  linter <- brace_linter()
  lint_msg_open <- rex::rex("Opening curly braces should never go on their own line")
  lint_msg_closed <- rex::rex("Closing curly-braces should always be on their own line")

  expect_lint(
    trim_some("
      map(
        .x = 1:4,
        .f = ~ {
                .x + 1
              }
      )"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      map(
        .x = 1:4,
        .f = ~ {.x + 1}
      )"),
    list(
      list(message = lint_msg_open, line_number = 3L, column_number = 10L),
      list(message = lint_msg_closed, line_number = 3L, column_number = 17L)
    ),
    linter
  )

  expect_lint(
    trim_some("
      map(
        .x = 1:4,
        .f = ~ { .x + 1
              }
      )"),
    list(
      list(message = lint_msg_open, line_number = 3L, column_number = 10L)
    ),
    linter
  )

  expect_lint(
    trim_some("
      map(
        .x = 1:4,
        .f = ~ {
                .x + 1}
      )"),
    list(
      list(message = lint_msg_closed, line_number = 4L, column_number = 17L)
    ),
    linter
  )
})

test_that("code with pipes is handled correctly", {
  linter <- brace_linter()
  lint_msg_open <- rex::rex("Opening curly braces should never go on their own line")
  lint_msg_closed <- rex::rex("Closing curly-braces should always be on their own line")

  expect_lint(
    trim_some("
      out <- lapply(stuff, function(i) {
        do_something(i)
      }) %>% unlist
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      1:4 %!>% {
          sum(.)
        }
    "),
    NULL,
    linter
  )

  # %>%\n{ is allowed
  expect_lint(
    trim_some("
      1:4 %T>%
        {
          sum(.)
        }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      xx %<>% { sum(.)
        }
    "),
    list(
      list(message = lint_msg_open, line_number = 1L, column_number = 9L)
    ),
    linter
  )

  expect_lint(
    trim_some("
      x %>%
        {
          uvwxyz }
    "),
    list(
      list(message = lint_msg_closed, line_number = 3L, column_number = 12L)
    ),
    linter
  )

  expect_lint(
    trim_some("
      1:4 %>%
        { sum(.) }
    "),
    list(
      list(message = lint_msg_closed, line_number = 2L, column_number = 12L)
    ),
    linter
  )

  expect_lint(
    "1:4 %>% { sum(.) }",
    list(
      list(message = lint_msg_open, line_number = 1L, column_number = 9L),
      list(message = lint_msg_closed, line_number = 1L, column_number = 18L)
    ),
    linter
  )

  skip_if_not_r_version("4.1.0")

  expect_lint(
    trim_some("
      out <- lapply(stuff, function(i) {
        do_something(i)
      }) |> unlist()
    "),
    NULL,
    linter
  )

  expect_lint(
    "local({ 1:4 |> sum() })",
    list(
      list(message = lint_msg_open, line_number = 1L, column_number = 7L)
    ),
    linter
  )
})

test_that("function shorthand is treated like 'full' function", {
  skip_if_not_r_version("4.1.0")
  linter <- brace_linter()

  expect_lint("a <- \\() {  \n}", NULL, linter)
  expect_lint(
    trim_some("
      x <- \\()
              {2}
    "),
    list(
      rex::rex("Opening curly braces should never go on their own line"),
      rex::rex("Closing curly-braces should always be on their own line")
    ),
    linter
  )
})
