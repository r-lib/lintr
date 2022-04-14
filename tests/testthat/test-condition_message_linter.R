test_that("condition_message_linter skips allowed usages", {
  expect_lint("stop('a string', 'another')", NULL, condition_message_linter())
  expect_lint("warning('a string', 'another')", NULL, condition_message_linter())
  expect_lint("message('a string', 'another')", NULL, condition_message_linter())

  # paste/paste0 allowed when using other seps and/or collapse
  expect_lint("stop(paste(x, collapse = ''))", NULL, condition_message_linter())
  expect_lint("message(paste(x, sep = '-'))", NULL, condition_message_linter())

  # sprintf is OK (really should be gettextf but offering translations
  #   at google internally is not likely to happen any time soon)
  expect_lint("stop(sprintf('A %s!', 'string'))", NULL, condition_message_linter())

  # get multiple sep= in one expression
  expect_lint(
    trim_some("
      tryCatch(
        foo(x),
        error = function(e) stop(paste(a, b, sep = '-')),
        warning = function(w) warning(paste(a, b, sep = '--')),
      )
    "),
    NULL,
    condition_message_linter()
  )
})

test_that("condition_message_linter blocks simple disallowed usages", {
  expect_lint(
    "stop(paste('a string', 'another'))",
    rex::rex("Don't use paste to build stop strings."),
    condition_message_linter()
  )

  expect_lint(
    "warning(paste0('a string ', 'another'))",
    rex::rex("Don't use paste0 to build warning strings."),
    condition_message_linter()
  )

  # not thrown off by named arguments
  expect_lint(
    "stop(paste('a', 'b'), call. = FALSE)",
    rex::rex("Don't use paste to build stop strings."),
    condition_message_linter()
  )

  expect_lint(
    "warning(paste0('a', 'b'), immediate. = TRUE)",
    rex::rex("Don't use paste0 to build warning strings."),
    condition_message_linter()
  )

  expect_lint(
    trim_some("
      tryCatch(
        foo(x),
        error = function(e) stop(paste(a, b)),
        warning = function(w) warning(paste(a, b, sep = '--')),
      )
    "),
    rex::rex("Don't use paste to build stop strings."),
    condition_message_linter()
  )

  # one with no sep, one with linted sep
  expect_lint(
    trim_some("
      tryCatch(
        foo(x),
        error = function(e) stop(paste(a, b)),
        warning = function(w) warning(paste(a, b, sep = '')),
      )
    "),
    list(
      list(message = rex::rex("Don't use paste to build stop strings.")),
      list(message = rex::rex("Don't use paste to build warning strings"))
    ),
    condition_message_linter()
  )
})

test_that("packageStartupMessage usages are also matched", {
  expect_lint(
    "packageStartupMessage(paste('a string', 'another'))",
    rex::rex("Don't use paste to build packageStartupMessage strings."),
    condition_message_linter()
  )

  expect_lint(
    "packageStartupMessage(paste0('a string ', 'another'))",
    rex::rex("Don't use paste0 to build packageStartupMessage strings."),
    condition_message_linter()
  )
})

test_that("R>=4.0.0 raw strings are handled", {
  skip_if_not_installed("base", "4.0.0")
  expect_lint(
    "warning(paste(a, b, sep = R'( )'))",
    rex::rex("Don't use paste to build warning strings."),
    condition_message_linter()
  )
  expect_lint(
    "warning(paste(a, b, sep = R'---[ ]---'))",
    rex::rex("Don't use paste to build warning strings."),
    condition_message_linter()
  )
})
