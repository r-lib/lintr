test_that("indentation linter flags unindented expressions", {
  linter <- indentation_linter(indent = 2L)

  expect_lint(
    trim_some("
      lapply(1:10, function(i) {
        i %% 2
      })
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      lapply(1:10, function(i) {
       i %% 2  # indentation is only 1 character
      })
    "),
    "Indentation",
    linter
  )

  expect_lint(
    trim_some("
      lapply(1:10, function(i) {
       # indentation is only 1 character
        i %% 2
      })
    "),
    "Indentation",
    linter
  )

  # no double-block indents even if the indentation-starting tokens are immediately next to each other
  expect_lint(
    trim_some("
      local({
        # no lint
      })

      local({
          # must lint
      })
    "),
    list(line_number = 6L, message = "Indentation"),
    linter
  )

  expect_lint(
    trim_some("
      lapply(1:10, function(i) {
          i %% 2
      })
    "),
    NULL,
    indentation_linter(indent = 4L)
  )

  expect_lint(
    trim_some("
      lapply(1:10, function(i) {
        i %% 2  # indentation is only 2 characters
      })
    "),
    "Indentation",
    indentation_linter(indent = 4L)
  )

  # ugly code, but still correctly indented
  expect_lint(
    trim_some("
      list(
           1,
           2)
    "),
    NULL,
    linter
  )

  # comments do not trigger hanging indent rule
  expect_lint(
    trim_some("
      list( # comment
        ok
      )
    "),
    NULL,
    linter
  )

  # comments do not suppress block indents (#1751)
  expect_lint(
    trim_some("
      a <- # comment
        42L
    "),
    NULL,
    linter
  )

  # assignment triggers indent
  expect_lint(
    trim_some("
      a <-
        expr(
          42
        )
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (cond)
        code

      if (cond) code else code2

      if (cond) {
        code
      } else
        code

      if (cond) {
        code
      } else {
        code
      }
    "),
    NULL,
    linter
  )
})

test_that("indentation linter flags improper closing curly braces", {
  linter <- indentation_linter(indent = 2L)
  expect_lint(
    trim_some("
      lapply(1:10, function(i) {
        {
          i %% 2
        }
      })
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      lapply(1:10, function(i) {
        i %% 2
        } # closing curly doesn't return to parent indentation
      )
    "),
    "Indentation",
    linter
  )
})

test_that("function argument indentation works in tidyverse-style", {
  linter <- indentation_linter()
  expect_lint(
    trim_some("
      function(a = 1L,
               b = 2L) {
        a + b
      }
    "),
    NULL,
    linter
  )

  # new style (#1754)
  expect_lint(
    trim_some("
      function(
          a = 1L,
          b = 2L) {
        a + b
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      function(
            a = 1L,
            b = 2L) {
        a + b
      }
    "),
    "Indentation should be 4",
    linter
  )

  # Hanging is only allowed if there is an argument next to "("
  expect_lint(
    trim_some("
      function(
               a = 1L,
               b = 2L) {
        a + b
      }
    "),
    "Indentation should be 4",
    linter
  )

  # Block is only allowed if there is no argument next to ")"
  expect_lint(
    trim_some("
      function(
        a = 1L,
        b = 2L) {
        a + b
      }
    "),
    "Indentation should be 4",
    linter
  )

  expect_lint(
    trim_some("
      function(
        a = 1L,
        b = 2L
      ) {
        a + b
      }
    "),
    NULL,
    linter
  )

  # anchor is correctly found with assignments as well
  expect_lint(
    trim_some("
      test <- function(a = 1L,
                       b = 2L) {
        a + b
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      function(a = 1L,
         b = 2L) {
        a + b
      }
    "),
    "Hanging",
    linter
  )

  # This is a case for brace_linter
  expect_lint(
    trim_some("
      function(a = 1L,
               b = 2L)
      {
        a + b
      }
    "),
    NULL,
    linter
  )
})

test_that("function argument indentation works in always-hanging-style", {
  linter <- indentation_linter(hanging_indent_style = "always")
  expect_lint(
    trim_some("
      function(a = 1L,
               b = 2L) {
        a + b
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      function(
          a = 1L,
          b = 2L) {
        a + b
      }
    "),
    "Hanging",
    linter
  )

  expect_lint(
    trim_some("
      function(
               a = 1L,
               b = 2L) {
        a + b
      }
    "),
    NULL,
    linter
  )

  # Block is only allowed if there is no argument next to ")"
  expect_lint(
    trim_some("
      function(
        a = 1L,
        b = 2L) {
        a + b
      }
    "),
    "Hanging",
    linter
  )

  expect_lint(
    trim_some("
      function(
        a = 1L,
        b = 2L
      ) {
        a + b
      }
    "),
    NULL,
    linter
  )

  # anchor is correctly found with assignments as well
  expect_lint(
    trim_some("
      test <- function(a = 1L,
                       b = 2L) {
        a + b
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      function(a = 1L,
         b = 2L) {
        a + b
      }
    "),
    "Hanging",
    linter
  )

  # This is a case for brace_linter
  expect_lint(
    trim_some("
      function(a = 1L,
               b = 2L)
      {
        a + b
      }
    "),
    NULL,
    linter
  )
})

test_that("indentation with operators works", {
  linter <- indentation_linter()
  expect_lint(
    trim_some("
      a %>%
        b()
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      (a + b + c) /
        (d + e + f) /
        (g + h + i)
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      a %>%
          b()
    "),
    "Indentation",
    linter
  )

  expect_lint(
    trim_some("
      a +
       b()
    "),
    "Indentation",
    linter
  )

  expect_lint(
    trim_some("
      abc$
        def$
        ghi
    "),
    NULL,
    linter
  )
})

test_that("indentation with bracket works", {
  linter <- indentation_linter()

  expect_lint(
    trim_some("
      dt[
        , col := 42L
      ][
        , ok
      ]

      bla[hanging,
          also_ok]
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      abc[[
        'elem'
      ]]

      def[[a,
           b]]
    "),
    NULL,
    linter
  )
})

test_that("indentation works with control flow statements", {
  linter <- indentation_linter()

  expect_lint(
    trim_some("
      if (TRUE) {
        do_something
      } else {
        do_other_thing
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      while (1 > 2) {
      do_something
      }
    "),
    "Indentation",
    linter
  )

  expect_lint(
    trim_some("
      if (FALSE) {
        do_something
        } else {
        do_other_thing
      }
    "),
    "Indentation",
    linter
  )
})

test_that("indentation lint messages are dynamic", {
  linter <- indentation_linter()

  expect_lint(
    trim_some("
      local({
          # should be 2
      })
    "),
    rex::rex("Indentation should be 2 spaces but is 4 spaces."),
    linter
  )

  expect_lint(
    trim_some("
      fun(
        3) # should be 4
    "),
    rex::rex("Hanging indent should be 4 spaces but is 2 spaces."),
    linter
  )
})

test_that("indentation within string constants is ignored", {
  expect_lint(
    trim_some("
      x <- '
        an indented string
      '
    "),
    NULL,
    indentation_linter()
  )

  expect_lint(
    trim_some("
      x <- '
         an indented string with 3 spaces indentation
      '
    "),
    NULL,
    indentation_linter()
  )
})

test_that("combined hanging and block indent works", {
  linter <- indentation_linter()
  expect_lint(
    trim_some("
      func(hang, and,
           block(
             combined
           ))
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      func(ha,
           func2(ab,
                 block(
                   indented
                 )))
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      func(func2(
        a = 42
      ))
    "),
    NULL,
    linter
  )

  # Adapted from cli R/ansi.R L231-234
  expect_lint(
    trim_some("
      stopifnot(is.character(style) && length(style) == 1 ||
                  is_rgb_matrix(style) && ncol(style) == 1,
                is.logical(bg) && length(bg) == 1,
                is.numeric(colors) && length(colors) == 1)
    "),
    NULL,
    linter
  )

  # Adapted from cli inst/scripts/up.R L26-37
  expect_lint(
    trim_some("
      http_head(url, ...)$
        then(function(res) {
          if (res$status_code < 300) {
            cli_alert_success()
          } else {
            cli_alert_danger()
          }
        })$
        catch(error = function(err) {
          e <- if (grepl('timed out', err$message)) 'timed out' else 'error'
          cli_alert_danger()
        })
    "),
    NULL,
    linter
  )
})

test_that("hanging_indent_stlye works", {
  code_block_multi_line <- "map(x, f,\n  extra_arg = 42\n)"
  code_hanging_multi_line <- "map(x, f,\n    extra_arg = 42\n)"
  code_block_same_line <- "map(x, f,\n  extra_arg = 42)"
  code_hanging_same_line <- "map(x, f,\n    extra_arg = 42)"

  tidy_linter <- indentation_linter()
  hanging_linter <- indentation_linter(hanging_indent_style = "always")
  non_hanging_linter <- indentation_linter(hanging_indent_style = "never")

  expect_lint(code_block_multi_line, NULL, tidy_linter)
  expect_lint(code_block_multi_line, "Hanging indent", hanging_linter)
  expect_lint(code_block_multi_line, NULL, non_hanging_linter)

  expect_lint(code_hanging_multi_line, "Indent", tidy_linter)
  expect_lint(code_hanging_multi_line, NULL, hanging_linter)
  expect_lint(code_hanging_multi_line, "Indent", non_hanging_linter)

  expect_lint(code_block_same_line, "Hanging indent", tidy_linter)
  expect_lint(code_block_same_line, "Hanging indent", hanging_linter)
  expect_lint(code_block_same_line, NULL, non_hanging_linter)

  expect_lint(code_hanging_same_line, NULL, tidy_linter)
  expect_lint(code_hanging_same_line, NULL, hanging_linter)
  expect_lint(code_hanging_same_line, "Indent", non_hanging_linter)

  # regression test for #1898
  expect_lint(
    trim_some("
      outer_fun(inner_fun(x,
        one_indent = 42L
      ))
    "),
    NULL,
    tidy_linter
  )

  expect_lint(
    trim_some("
      outer_fun(inner_fun(x, # this is first arg
        one_indent = 42L # this is second arg
      ))
    "),
    NULL,
    tidy_linter
  )

  expect_lint(
    trim_some("
      outer_fun(inner_fun(
        x,
        one_indent = 42L
      ))
    "),
    NULL,
    tidy_linter
  )

  expect_lint(
    trim_some("
      outer_fun(
        inner_fun(
          x,
          one_indent = 42L
        )
      )
    "),
    NULL,
    tidy_linter
  )
})

test_that("assignment_as_infix works", {
  # test function call restorator and LEFT_ASSIGN suppressor
  code_infix <- trim_some("
    ok_code <-
      var1 +
      f(
        var2 +
          var3
      ) +
      var4
  ")

  # test that innermost ancestor token decides the indentation
  code_infix_2 <- trim_some("
    lapply(x,
      function(e) {
        temp_var <-
          e +
          42
      }
    )
  ")

  # test brace restorator
  code_infix_3 <- trim_some("
    ok_code <-
      if (condition) {
        a +
          b
      } else {
        c +
          d
      } +
      e
  ")

  # test EQ_ASSIGN, EQ_SUB and EQ_FORMALS suppressors
  code_infix_4 <- trim_some("
    # EQ_ASSIGN
    ok_code =
      a +
      b

    # EQ_SUB
    f(
      a =
        b +
        c
    )

    # EQ_FORMALS
    f <- function(
      a =
        b +
        c
    ) {
      NULL
    }
  ")

  code_no_infix <- trim_some("
    ok_code <-
      var1 +
        f(
          var2 +
            var3
        ) +
        var4
  ")

  tidy_linter <- indentation_linter()
  no_infix_linter <- indentation_linter(assignment_as_infix = FALSE)

  expect_lint(code_infix, NULL, tidy_linter)
  expect_lint(code_infix_2, NULL, tidy_linter)
  expect_lint(code_infix_3, NULL, tidy_linter)
  expect_lint(code_infix_4, NULL, tidy_linter)
  expect_lint(code_no_infix, rex::rex("Indentation should be 2 spaces but is 4 spaces."), tidy_linter)

  expect_lint(code_infix, rex::rex("Indentation should be 4 spaces but is 2 spaces."), no_infix_linter)
  expect_lint(code_infix_2, rex::rex("Indentation should be 8 spaces but is 6 spaces."), no_infix_linter)
  expect_lint(code_infix_3, rex::rex("Indentation should be 4 spaces but is 2 spaces."), no_infix_linter)
  expect_lint(code_infix_4, list(
    list(line_number = 4L, rex::rex("Indentation should be 4 spaces but is 2 spaces.")),
    list(line_number = 10L, rex::rex("Indentation should be 6 spaces but is 4 spaces.")),
    list(line_number = 17L, rex::rex("Indentation should be 6 spaces but is 4 spaces."))
  ), no_infix_linter)
  expect_lint(code_no_infix, NULL, no_infix_linter)
})

test_that("consecutive same-level lints are suppressed", {
  bad_code <- trim_some("
    ok_code <- 42

    wrong_hanging <- fun(a, b, c,
                           d, e %>%
                             f())

    wrong_block <- function() {
        a + b
        c + d
        if (a == 24)
          boo
    }

    wrong_hanging_args <- function(a = 1, b = 2,
      c = 3, d = 4,
      e = 5, f = 6)
    {
      a + b + c + d + e + f
    }
  ")

  expect_lint(
    bad_code,
    list(
      list(line_number = 4L, message = "Hanging indent"),
      list(line_number = 8L, message = "Indentation"),
      list(line_number = 15L, message = "Hanging indent")
    ),
    indentation_linter()
  )
})

test_that("native pipe is supported", {
  skip_if_not_r_version("4.1.0")
  linter <- indentation_linter()

  expect_lint(
    trim_some("
      a |>
        foo()
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      b <- a |>
        foo()
    "),
    NULL,
    linter
  )
})

test_that("it doesn't error on invalid code", {
  # Part of #1427
  expect_lint("function() {)", list(linter = "error", message = rex::rex("unexpected ')'")), indentation_linter())
})

test_that("function shorthand is handled", {
  skip_if_not_r_version("4.1.0")
  linter <- indentation_linter()

  expect_lint(
    trim_some("
      lapply(1:10, \\(i) {
        i %% 2
      })
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      lapply(1:10, \\(i) {
       i %% 2  # indentation is only 1 character
      })
    "),
    "Indentation",
    linter
  )

  expect_lint(
    trim_some("
      \\(
          a = 1L,
          b = 2L) {
        a + b
      }
    "),
    NULL,
    linter
  )
})

test_that("lint metadata works for 0-space case", {
  expect_lint(
    trim_some("
    if (TRUE) {
    FALSE
    }
    "),
    list(ranges = list(1L:2L)),
    indentation_linter()
  )
})
