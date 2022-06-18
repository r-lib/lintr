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
