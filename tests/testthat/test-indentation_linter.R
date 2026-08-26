# fuzzer disable: comment_injection
test_that("indentation linter flags unindented expressions", {
  linter <- indentation_linter(indent = 2L)

  expect_no_lint(
    trim_some("
      lapply(1:10, function(i) {
        i %% 2
      })
    "),
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

  expect_no_lint(
    trim_some("
      lapply(1:10, function(i) {
          i %% 2
      })
    "),
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

  expect_lint(
    trim_some("
      list(
           1,
           2)
    "),
    list(
      list(
        rex::rex("Indentation should be 2 spaces but is 5 spaces (or start argument on previous line)."),
        line_number = 2L
      ),
      list(
        rex::rex("Closing ')' should be on a separate line."),
        line_number = 3L
      )
    ),
    linter
  )

  expect_lint(
    trim_some("
      list(
        1,
        2)
    "),
    list(
      rex::rex("Closing ')' should be on a separate line."),
      line_number = 3L
    ),
    linter
  )

  expect_no_lint(
    trim_some("
      list(
        1,
        2
      )
    "),
    linter
  )

  # comments do not trigger hanging indent rule
  expect_no_lint(
    trim_some("
      list( # comment
        ok
      )
    "),
    linter
  )

  # comments do not suppress block indents (#1751)
  expect_no_lint(
    trim_some("
      a <- # comment
        42L
    "),
    linter
  )

  # assignment triggers indent
  expect_no_lint(
    trim_some("
      a <-
        expr(
          42
        )
    "),
    linter
  )

  expect_no_lint(
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
    linter
  )
})

test_that("indentation linter flags improper closing curly braces", {
  linter <- indentation_linter(indent = 2L)
  expect_no_lint(
    trim_some("
      lapply(1:10, function(i) {
        {
          i %% 2
        }
      })
    "),
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

test_that("function argument indentation works in tidyverse-style", { # nofuzz: function_lambda
  linter <- indentation_linter()
  expect_no_lint(
    trim_some("
      function(a = 1L,
               b = 2L) {
        a + b
      }
    "),
    linter
  )

  # old double-indent style (#1754, #2830) lints under tidyverse style
  expect_lint(
    trim_some("
      function(
          a = 1L,
          b = 2L) {
        a + b
      }
    "),
    list(
      list(rex::rex("Indentation should be 2 spaces but is 4 spaces."), line_number = 2L),
      list(rex::rex("Closing ')' should be on a separate line."), line_number = 3L)
    ),
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
    list(
      list(rex::rex("Indentation should be 2 spaces but is 6 spaces."), line_number = 2L),
      list(rex::rex("Closing ')' should be on a separate line."), line_number = 3L)
    ),
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
    list(
      list(
        rex::rex("Indentation should be 2 spaces but is 9 spaces (or start argument on previous line)"),
        line_number = 2L
      ),
      list(rex::rex("Closing ')' should be on a separate line."), line_number = 3L)
    ),
    linter
  )

  expect_no_lint(
    trim_some("
      function(
        a = 1L,
        b = 2L
      ) {
        a + b
      }
    "),
    linter
  )

  # anchor is correctly found with assignments as well
  expect_no_lint( # nofuzz: assignment
    trim_some("
      test <- function(a = 1L,
                       b = 2L) {
        a + b
      }
    "),
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
  expect_no_lint(
    trim_some("
      function(a = 1L,
               b = 2L)
      {
        a + b
      }
    "),
    linter
  )

  expect_lint(
    trim_some("
      function(
        a = function(
              x) {
          x
        }) {
        a()
      }
    "),
    list(
      list(rex::rex("Indentation should be 4 spaces but is 8 spaces."), line_number = 3L),
      list(rex::rex("Closing ')' should be on a separate line."), line_number = 3L),
      list(rex::rex("Closing ')' should be on a separate line."), line_number = 5L)
    ),
    linter
  )

  # proper single indentation of nested function definitions.
  expect_no_lint(
    trim_some("
      function(
        a = function(
          x
        ) {
          x
        }
      ) {
        a()
      }
    "),
    linter
  )
})

test_that("function argument indentation works in always-hanging-style", { # nofuzz: function_lambda
  linter <- indentation_linter(hanging_indent_style = "always")
  expect_no_lint(
    trim_some("
      function(a = 1L,
               b = 2L) {
        a + b
      }
    "),
    linter
  )

  expect_lint(
    trim_some("
      function(a = 1L,
        b = 2L
      ) {
        a + b
      }
    "),
    "Hanging",
    linter
  )

  expect_no_lint(
    trim_some("
      function(a = 1L,
               b = 2L
      ) {
        a + b
      }
    "),
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
    list(
      list(rex::rex("Indentation should be 2 spaces but is 4 spaces."), line_number = 2L),
      list(rex::rex("Closing ')' should be on a separate line."), line_number = 3L)
    ),
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
    list(
      list(
        rex::rex("Indentation should be 2 spaces but is 9 spaces (or start argument on previous line)."),
        line_number = 2L
      ),
      list(rex::rex("Closing ')' should be on a separate line."), line_number = 3L)
    ),
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
    list(rex::rex("Closing ')' should be on a separate line."), line_number = 3L),
    linter
  )

  expect_no_lint(
    trim_some("
      function(
        a = 1L,
        b = 2L
      ) {
        a + b
      }
    "),
    linter
  )

  # anchor is correctly found with assignments as well
  expect_no_lint( # nofuzz: assignment
    trim_some("
      test <- function(a = 1L,
                       b = 2L) {
        a + b
      }
    "),
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
  expect_no_lint(
    trim_some("
      function(a = 1L,
               b = 2L)
      {
        a + b
      }
    "),
    linter
  )
})

test_that("indentation with operators works", {
  linter <- indentation_linter()
  expect_no_lint(
    trim_some("
      a %>%
        b()
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      (a + b + c) /
        (d + e + f) /
        (g + h + i)
    "),
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

  expect_no_lint(
    trim_some("
      abc$
        def$
        ghi
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      abc@
        def@
        ghi
    "),
    linter
  )

  expect_lint(
    trim_some("
      first_step() +
                  second_step()
    "),
    list(rex::rex("Indentation should be 2 spaces but is 12 spaces."), line_number = 2L),
    linter
  )
})

test_that("indentation with bracket works", {
  linter <- indentation_linter()

  expect_no_lint(
    trim_some("
      dt[
        , col := 42L
      ][
        , ok
      ]

      bla[hanging,
          also_ok]
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      abc[[
        'elem'
      ]]

      def[[a,
           b]]
    "),
    linter
  )
})

test_that("indentation works with control flow statements", {
  linter <- indentation_linter()

  expect_no_lint(
    trim_some("
      if (TRUE) {
        do_something
      } else {
        do_other_thing
      }
    "),
    linter
  )

  # Multiline conditional expressions (#2007)
  expect_no_lint(
    trim_some("
      foo <- function(info) {
        if (info$is_dispersion ||
          info$is_zero_inflated ||
          info$is_zeroinf) {
          NULL
        }
      }
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      communicate_warning <- function(changed, transformers) {
        if (any(changed, na.rm = TRUE) &&
          !parse_tree_must_be_identical(transformers) &&
          !getOption(\"styler.quiet\", FALSE)
        ) {
          cat(\"Please review the changes carefully!\", fill = TRUE)
        }
      }
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      foo <- function(info) {
        while (info$is_dispersion ||
          info$is_zero_inflated) {
          NULL
        }
      }
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      foo <- function(info) {
        if (
          info$is_dispersion ||
            info$is_zero_inflated ||
            info$is_zeroinf
        ) {
          NULL
        }
      }
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      if (TRUE ||
        FALSE) {
        TRUE
      }
    "),
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

  expect_lint(
    trim_some("
      foo <- function(info) {
        if (info$is_dispersion ||
            info$is_zero_inflated) {
          NULL
        }
      }
    "),
    list(
      rex::rex("Indentation should be 4 spaces but is 6 spaces (or start argument on previous line)."),
      line_number = 3L
    ),
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(info) {
        if (info$is_dispersion ||
         info$is_zero_inflated) {
          NULL
        }
      }
    "),
    list(rex::rex("Indentation should be 4 spaces but is 3 spaces."), line_number = 3L),
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
      fun(x,
        3) # should be 4
    "),
    rex::rex("Hanging indent should be 4 spaces but is 2 spaces."),
    linter
  )
})

test_that("indentation within string constants is ignored", {
  linter <- indentation_linter()

  expect_no_lint(
    trim_some("
      x <- '
        an indented string
      '
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      x <- '
         an indented string with 3 spaces indentation
      '
    "),
    linter
  )

  # first line of a multi-line string can induce a lint
  expect_lint(
    trim_some("
      foo(
      '
        string
      ')
      bar('
        string2
      ')
      baz('
      string3
      ')
        x <- '
        string4
      '
    "),
    list(
      list(rex::rex("Indentation should be 2 spaces but is 0 spaces."), line_number = 2L),
      list(rex::rex("Closing ')' should be on a separate line."), line_number = 4L),
      list(rex::rex("Indentation should be 0 spaces but is 2 spaces"), line_number = 11L)
    ),
    linter
  )
})

test_that("combined hanging and block indent works", {
  linter <- indentation_linter()

  expect_no_lint(
    trim_some("
      func(hang, and,
           block(
             combined
           ))
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      func(ha,
           func2(ab,
                 block(
                   indented
                 )))
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      func(func2(
        a = 42
      ))
    "),
    linter
  )

  # Adapted from cli R/ansi.R L231-234
  expect_no_lint(
    trim_some("
      stopifnot(is.character(style) && length(style) == 1 ||
                  is_rgb_matrix(style) && ncol(style) == 1,
                is.logical(bg) && length(bg) == 1,
                is.numeric(colors) && length(colors) == 1)
    "),
    linter
  )

  # Adapted from cli inst/scripts/up.R L26-37
  expect_no_lint(
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
    linter
  )

  # S4 equivalence
  expect_no_lint(
    trim_some("
      http_head(url, ...)@
        then(function(res) {
          if (res$status_code < 300) {
            cli_alert_success()
          } else {
            cli_alert_danger()
          }
        })@
        catch(error = function(err) {
          e <- if (grepl('timed out', err$message)) 'timed out' else 'error'
          cli_alert_danger()
        })
    "),
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

  expect_no_lint(code_block_multi_line, tidy_linter)
  expect_lint(code_block_multi_line, "Hanging indent", hanging_linter)
  expect_no_lint(code_block_multi_line, non_hanging_linter)

  expect_lint(code_hanging_multi_line, "Indent", tidy_linter)
  expect_no_lint(code_hanging_multi_line, hanging_linter)
  expect_lint(code_hanging_multi_line, "Indent", non_hanging_linter)

  expect_lint(code_block_same_line, "Hanging indent", tidy_linter)
  expect_lint(code_block_same_line, "Hanging indent", hanging_linter)
  expect_no_lint(code_block_same_line, non_hanging_linter)

  expect_no_lint(code_hanging_same_line, tidy_linter)
  expect_no_lint(code_hanging_same_line, hanging_linter)
  expect_lint(code_hanging_same_line, "Indent", non_hanging_linter)

  # regression test for #1898
  expect_no_lint(
    trim_some("
      outer_fun(inner_fun(x,
        one_indent = 42L
      ))
    "),
    tidy_linter
  )

  expect_no_lint(
    trim_some("
      outer_fun(inner_fun(x, # this is first arg
        one_indent = 42L # this is second arg
      ))
    "),
    tidy_linter
  )

  expect_no_lint(
    trim_some("
      outer_fun(inner_fun(
        x,
        one_indent = 42L
      ))
    "),
    tidy_linter
  )

  expect_no_lint(
    trim_some("
      outer_fun(
        inner_fun(
          x,
          one_indent = 42L
        )
      )
    "),
    tidy_linter
  )

  expect_lint(
    trim_some("
      outer_f(
        inner_g(x,
        y = 2)
      )
    "),
    list(rex::rex("Hanging indent should be 10 spaces but is 2 spaces."), line_number = 3L),
    tidy_linter
  )

  # don't suggest starting argument on previous line
  expect_lint(
    trim_some("
      outer_f(
        inner_g(x,
              y = 2)
      )
    "),
    list(rex::rex("Hanging indent should be 10 spaces but is 8 spaces."), line_number = 3L),
    tidy_linter
  )
})

test_that("previous token is respected when recommending to 'start argument on previous line'", {
  expect_lint(
    trim_some("
      result <- {
                 do_something()
      }
      abc[[
           'elem'
      ]]
      def[
          'key'
      ]
      first_step() +
                    second_step()
    "),
    list(
      list(rex::rex("should be 2 spaces but is 11 spaces."), line_number = 2L),
      list(rex::rex("should be 2 spaces but is 5 spaces (or start argument on previous line)."), line_number = 5L),
      list(rex::rex("should be 2 spaces but is 4 spaces (or start argument on previous line)."), line_number = 8L),
      list(rex::rex("should be 2 spaces but is 14 spaces."), line_number = 11L)
    ),
    indentation_linter()
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

  expect_no_lint(code_infix, tidy_linter)
  expect_no_lint(code_infix_2, tidy_linter)
  expect_no_lint(code_infix_3, tidy_linter)
  expect_no_lint(code_infix_4, tidy_linter)
  expect_lint(code_no_infix, rex::rex("Indentation should be 2 spaces but is 4 spaces."), tidy_linter)

  expect_lint(code_infix, rex::rex("Indentation should be 4 spaces but is 2 spaces."), no_infix_linter)
  expect_lint(code_infix_2, rex::rex("Indentation should be 8 spaces but is 6 spaces."), no_infix_linter)
  expect_lint(code_infix_3, rex::rex("Indentation should be 4 spaces but is 2 spaces."), no_infix_linter)
  expect_lint(code_infix_4, list(
    list(line_number = 4L, rex::rex("Indentation should be 4 spaces but is 2 spaces.")),
    list(line_number = 10L, rex::rex("Indentation should be 6 spaces but is 4 spaces.")),
    list(line_number = 17L, rex::rex("Indentation should be 6 spaces but is 4 spaces."))
  ), no_infix_linter)
  expect_no_lint(code_no_infix, no_infix_linter)
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
  linter <- indentation_linter()

  expect_no_lint(
    trim_some("
      a |>
        foo()
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      b <- a |>
        foo()
    "),
    linter
  )
})

test_that("it doesn't error on invalid code", {
  # Part of #1427
  expect_lint("function() {)", list(linter = "error", message = rex::rex("unexpected ')'")), indentation_linter())
})

test_that("function shorthand is handled", {
  linter <- indentation_linter()

  expect_no_lint(
    trim_some("
      lapply(1:10, \\(i) {
        i %% 2
      })
    "),
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

  expect_no_lint(
    trim_some(R"(
      \(
        a = 1L,
        b = 2L
      ) {
        a + b
      }
    )"),
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

test_that("for loop gets correct linting", {
  linter <- indentation_linter()
  lint_msg <- rex::rex("Indentation should be 2 spaces")

  expect_no_lint(
    trim_some("
       for (i in 1:10)
         1
    "),
    linter
  )
  expect_lint(
    trim_some("
       for (i in 1:10)
          1
    "),
    lint_msg,
    linter
  )

  expect_no_lint(
    trim_some("
       for (i in 1:10) {
         1
       }
    "),
    linter
  )
  expect_lint(
    trim_some("
       for (i in 1:10) {
          1
       }
    "),
    lint_msg,
    linter
  )
})

test_that("closing parentheses on multi-line calls without first-line args work (#2144)", {
  linter <- indentation_linter()

  expect_lint(
    trim_some('
      test_that("unreachable_code_linter works in sub expressions", {
        linter <- unreachable_code_linter()
        msg <- rex::rex("Code and comments coming after a return() or stop()")

        expect_lint(
          lines,
          list(
            list(line_number = 4L, message = msg),
            list(line_number = 7L, message = msg),
            list(line_number = 10L, message = msg),
            list(line_number = 15L, message = msg)
          ),
          linter)
      })
    '),
    list(
      rex::rex("Closing ')' should be on a separate line."),
      line_number = 13L
    ),
    linter
  )

  expect_no_lint(
    trim_some('
      test_that("unreachable_code_linter works in sub expressions", {
        linter <- unreachable_code_linter()
        msg <- rex::rex("Code and comments coming after a return() or stop()")

        expect_lint(
          lines,
          list(
            list(line_number = 4L, message = msg),
            list(line_number = 7L, message = msg),
            list(line_number = 10L, message = msg),
            list(line_number = 15L, message = msg)
          ),
          linter
        )
      })
    '),
    linter
  )

  expect_lint(
    trim_some("
      foo(
        a,
        b)
    "),
    list(rex::rex("Closing ')' should be on a separate line."), line_number = 3L),
    linter
  )

  expect_no_lint(
    trim_some("
      foo(
        a,
        b
      )
    "),
    linter
  )

  expect_lint(
    trim_some("
      foo(
        a,
        b,
        c)
    "),
    list(rex::rex("Closing ')' should be on a separate line."), line_number = 4L),
    linter
  )

  expect_lint(
    trim_some("
      dt[
        1:10,
        x := 1]
    "),
    list(rex::rex("Closing ']' should be on a separate line."), line_number = 3L),
    linter
  )

  expect_no_lint(
    trim_some("
      dt[
        1:10,
        x := 1
      ]
    "),
    linter
  )

  expect_lint(
    trim_some("
      x[[
        'a',
        exact = TRUE]]
    "),
    list(rex::rex("Closing ']' should be on a separate line."), line_number = 3L),
    linter
  )

  expect_no_lint(
    trim_some("
      x[[
        'a',
        exact = TRUE
      ]]
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      if (TRUE) {
        1 + 1}
    "),
    linter
  )

  expect_lint(
    trim_some("
      foo(
          a,
          b)
    "),
    list(
      list(
        rex::rex("Indentation should be 2 spaces but is 4 spaces (or start argument on previous line)."),
        line_number = 2L
      ),
      list(rex::rex("Closing ')' should be on a separate line."), line_number = 3L)
    ),
    linter
  )

  expect_lint(
    trim_some("
      foo(
            a,
            b)
    "),
    list(
      list(rex::rex("Indentation should be 2 spaces but is 6 spaces."), line_number = 2L),
      list(rex::rex("Closing ')' should be on a separate line."), line_number = 3L)
    ),
    linter
  )
})
# fuzzer enable: comment_injection
