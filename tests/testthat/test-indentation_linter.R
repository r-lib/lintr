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

test_that("use_hybrid_indent works", {
  code_hybrid <- "map(x, f,\n  extra_arg = 42\n)"
  code_non_hybrid <- "map(x, f,\n    extra_arg = 42)"

  expect_lint(code_hybrid, NULL, indentation_linter())
  expect_lint(code_hybrid, "Hanging indent", indentation_linter(use_hybrid_indent = FALSE))

  expect_lint(code_non_hybrid, "Indent", indentation_linter())
  expect_lint(code_non_hybrid, NULL, indentation_linter(use_hybrid_indent = FALSE))
})
