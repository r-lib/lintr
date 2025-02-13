test_that("condition_message_linter skips allowed usages", {
  linter <- condition_message_linter()

  expect_no_lint("stop('a string', 'another')", linter)
  expect_no_lint("warning('a string', 'another')", linter)
  expect_no_lint("message('a string', 'another')", linter)
  # extracted calls likely don't obey base::stop() semantics
  expect_no_lint("ctx$stop(paste('a', 'b'))", linter)
  expect_no_lint("ctx@stop(paste('a', 'b'))", linter)

  # sprintf is OK -- gettextf() enforcement is left to other linters
  expect_no_lint("stop(sprintf('A %s!', 'string'))", linter)

  # get multiple sep= in one expression
  expect_no_lint(
    trim_some("
      tryCatch(
        foo(x),
        error = function(e) stop(paste(a, b, sep = '-')),
        warning = function(w) warning(paste(a, b, sep = '--')),
      )
    "),
    linter
  )
})

skip_if_not_installed("tibble")
patrick::with_parameters_test_that(
  "paste/paste0 allowed by condition_message_linter when using other seps and/or collapse",
  expect_no_lint(
    sprintf("%s(%s(x, %s = '%s'))", condition, fun, parameter, arg),
    condition_message_linter()
  ),
  .cases = tibble::tribble(
    ~.test_name,                           ~condition, ~fun,     ~parameter, ~arg,
    "stop, paste and collapse = ''",       "stop",     "paste",  "collapse",  "",
    "warning, paste and collapse = '\n'",  "warning",  "paste",  "collapse",  "\n",
    "message, paste and collapse = '|'",   "message",  "paste",  "collapse",  "|",
    "stop, paste0 and collapse = ''",      "stop",     "paste0", "collapse",  "",
    "warning, paste0 and collapse = '\n'", "warning",  "paste0", "collapse",  "\n",
    "message, paste0 and collapse = '|'",  "message",  "paste0", "collapse",  "|",
    "stop, paste and sep = '-'",           "stop",     "paste",  "sep",       "-",
    "warning, paste and sep = '\n'",       "warning",  "paste",  "sep",       "\n",
    "message, paste and sep = '|'",        "message",  "paste",  "sep",       "|"
  )
)

test_that("condition_message_linter blocks simple disallowed usages", {
  linter <- condition_message_linter()
  lint_msg_paste_stop <- rex::rex("Don't use paste to build stop strings.")

  expect_lint("stop(paste('a string', 'another'))", lint_msg_paste_stop, linter)

  expect_lint(
    "warning(paste0('a string ', 'another'))",
    rex::rex("Don't use paste0 to build warning strings."),
    linter
  )

  # `sep` argument allowed, but only if it is different from default
  expect_lint("stop(paste(x, sep = ' '))", lint_msg_paste_stop, linter)

  # not thrown off by named arguments
  expect_lint("stop(paste('a', 'b'), call. = FALSE)", lint_msg_paste_stop, linter)

  expect_lint(
    "warning(paste0('a', 'b'), immediate. = TRUE)",
    rex::rex("Don't use paste0 to build warning strings."),
    linter
  )

  expect_lint(
    trim_some("
      tryCatch(
        foo(x),
        error = function(e) stop(paste(a, b)),
        warning = function(w) warning(paste(a, b, sep = '--')),
      )
    "),
    lint_msg_paste_stop,
    linter
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
      list(lint_msg_paste_stop, line_number = 3L),
      list(rex::rex("Don't use paste to build warning strings"), line_number = 4L)
    ),
    linter
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

test_that("raw strings are handled correctly", {
  linter <- condition_message_linter()
  lint_msg <- rex::rex("Don't use paste to build warning strings.")

  expect_lint("warning(paste(a, b, sep = R'( )'))", lint_msg, linter)
  expect_lint("warning(paste(a, b, sep = R'---[ ]---'))", lint_msg, linter)
})

test_that("message vectorization works", {
  expect_lint(
    trim_some("
      foo <- function() {
        warning(paste('uh oh!', 'spaghettios'))
        stop(paste0('look out ', 'below!'))
      }
    "),
    list(
      rex::rex("Don't use paste to build warning strings"),
      rex::rex("Don't use paste0 to build stop strings")
    ),
    condition_message_linter()
  )
})
