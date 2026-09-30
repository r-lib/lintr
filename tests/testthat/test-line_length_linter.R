# fuzzer disable: comment_injection
test_that("line_length_linter skips allowed usages", {
  linter <- line_length_linter(80L)

  expect_no_lint("blah", linter)
  expect_no_lint(strrep("x", 80L), linter)
})

test_that("line_length_linter blocks disallowed usages", {
  linter <- line_length_linter(80L)
  lint_msg <- rex::rex("Lines should not be more than 80 characters. This line is 81 characters.")

  expect_lint(
    strrep("x", 81L),
    list(
      message = lint_msg,
      column_number = 81L
    ),
    linter
  )

  expect_lint(
    paste(rep(strrep("x", 81L), 2L), collapse = "\n"),
    list(
      list(
        message = lint_msg,
        line_number = 1L,
        column_number = 81L
      ),
      list(
        message = lint_msg,
        line_number = 2L,
        column_number = 81L
      )
    ),
    linter
  )

  linter <- line_length_linter(20L)
  lint_msg <- rex::rex("Lines should not be more than 20 characters. This line is 22 characters.")
  expect_no_lint(strrep("a", 20L), linter)
  expect_lint(
    strrep("a", 22L),
    list(
      message = lint_msg,
      column_number = 21L
    ),
    linter
  )

  # Don't duplicate lints
  expect_length(
    lint(
      "x <- 2 # ------------\n",
      linters = linter,
      parse_settings = FALSE
    ),
    1L
  )
})

test_that("Multiple lints give custom messages", {
  expect_lint(
    trim_some("{
      abcdefg
      hijklmnop
    }"),
    list(
      list("9 characters", line_number = 2L),
      list("11 characters", line_number = 3L)
    ),
    line_length_linter(5L)
  )
})

test_that("string bodies can be ignored", {
  linter <- line_length_linter(10L, ignore_string_bodies = TRUE)
  lint_msg <- rex::rex("Lines should not be more than 10 characters. This line is 15 characters.")

  expect_no_lint(
    trim_some("
      1234567890
      str <- '
      123456789012345
      '
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      1234567890
      my_fun78('
      123456789012345
               '
      )
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      1234567890
      my_fun('90
      123456789012345
      123456789'
      )
    "),
    linter
  )

  expect_lint(
    trim_some("
      1234567890
      my_fun789('
      123456789012345
                '
      )
    "),
    list(
      list("11 characters", line_number = 2L),
      list("11 characters", line_number = 4L)
    ),
    linter
  )

  expect_lint(
    trim_some("
      1234567890
      my_fun('9012345
      1234567890
      123456789'
      )
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      1234567890
      my_fun('90
      1234567890
      12345678'); 234
    "),
    lint_msg,
    linter
  )

  expect_lint(
    "'1'; '2'; '345'",
    lint_msg,
    linter
  )

  expect_lint(
    "123456789012345",
    lint_msg,
    linter
  )

  expect_lint('"short" # 15!!!', lint_msg, linter)
  expect_lint('foo("a", long_)', lint_msg, linter)
})

test_that("allow_alignment_calls exempts tabular calls", {
  linter <- line_length_linter(40L)

  expect_no_lint(
    trim_some('
      df <- tibble::tribble(
        ~col_one                  , ~col_two                  ,
        "very_long_string_value1" , "very_long_string_value2"
      )
    '),
    linter
  )

  expect_no_lint(
    trim_some('
      dt <- rowwiseDT(
        col_one =                 , col_two =                 ,
        "very_long_string_value1" , "very_long_string_value2"
      )
    '),
    linter
  )

  expect_lint(
    trim_some('
      df <- tibble::tribble(
        ~col_one                  , ~col_two                  ,
        "very_long_string_value1" , "very_long_string_value2"
      )
    '),
    list(
      list("57 characters", line_number = 2L),
      list("55 characters", line_number = 3L)
    ),
    line_length_linter(40L, allow_alignment_calls = character())
  )
})

test_that("allow_long_test_names exempts test_that() descriptions by default", {
  linter <- line_length_linter(40L)

  expect_no_lint(
    trim_some('
      test_that("a very long test description that exceeds 40 chars", {
        expect_true(TRUE)
      })
    '),
    linter
  )

  # Namespace-qualified and hanging description on its own line
  expect_no_lint(
    trim_some('
      testthat::test_that(
        "a very long test description that exceeds 40 chars",
        {
          expect_true(TRUE)
        }
      )
    '),
    linter
  )

  # Boundary case where the string ends within the limit, but `, {` pushes the header over
  expect_no_lint(
    trim_some('
      test_that("exact_28_char_test_name_1234", {
        expect_true(TRUE)
      })
    '),
    linter
  )

  # Trailing comment on an already-long test header is allowed
  expect_no_lint(
    trim_some('
      test_that("a very long test description that exceeds 40 chars", { # comment
        expect_true(TRUE)
      })
    '),
    linter
  )

  # Multi-line string literal description
  expect_no_lint(
    trim_some('
      test_that("first line of a very long test description that exceeds 40 chars
      middle line of a very long test description that also exceeds 40 chars
      final line of a very long test description that exceeds 40 chars", {
        expect_true(TRUE)
      })
    '),
    linter
  )
})

test_that("allow_long_test_names still lints non-header long lines and can be disabled", {
  linter <- line_length_linter(40L)

  # Long lines inside the test body still lint
  expect_lint(
    trim_some('
      test_that("a very long test description that exceeds 40 chars", {
        expect_identical(very_long_variable_one, very_long_variable_two)
      })
    '),
    list("66 characters", line_number = 2L),
    linter
  )

  # Short test header with a long trailing comment still lints
  expect_lint(
    trim_some('
      test_that("short", { # a very long trailing comment that exceeds 40 chars
        expect_true(TRUE)
      })
    '),
    list("73 characters", line_number = 1L),
    linter
  )

  # Short test name with an inline test body on the same line still lints
  expect_lint(
    'test_that("short", expect_true(very_long_variable_name))',
    list("56 characters", line_number = 1L),
    linter
  )

  # Non-string-literal descriptions and other namespaces still lint
  expect_lint(
    trim_some('
      test_that(paste("a very long test description", "that exceeds 40 chars"), {
        expect_true(TRUE)
      })
    '),
    list("75 characters", line_number = 1L),
    linter
  )
  expect_lint(
    trim_some('
      other::test_that("a very long test description that exceeds 40 chars", {
        expect_true(TRUE)
      })
    '),
    list("72 characters", line_number = 1L),
    linter
  )

  # Disabling allow_long_test_names lints long test descriptions
  expect_lint(
    trim_some('
      test_that("a very long test description that exceeds 40 chars", {
        expect_true(TRUE)
      })
    '),
    list("65 characters", line_number = 1L),
    line_length_linter(40L, allow_long_test_names = FALSE)
  )
})
# fuzzer enable: comment_injection
