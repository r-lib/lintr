test_that("stop_paste_linter skips allowed usages", {
  expect_lint("stop('a string', 'another')", NULL, stop_paste_linter())
  expect_lint("warning('a string', 'another')", NULL, stop_paste_linter())
  expect_lint("message('a string', 'another')", NULL, stop_paste_linter())

  # paste/paste0 allowed when using other seps and/or collapse
  expect_lint("stop(paste(x, collapse = ''))", NULL, stop_paste_linter())
  expect_lint("message(paste(x, sep = '-'))", NULL, stop_paste_linter())

  # sprintf is OK (really should be gettextf but offering translations
  #   at google internally is not likely to happen any time soon)
  expect_lint("stop(sprintf('A %s!', 'string'))", NULL, stop_paste_linter())
})

test_that("stop_paste_linter blocks simple disallowed usages", {
  expect_lint(
    "stop(paste('a string', 'another'))",
    rex::rex("Don't use paste to build stop strings."),
    stop_paste_linter()
  )

  expect_lint(
    "warning(paste0('a string ', 'another'))",
    rex::rex("Don't use paste0 to build warning strings."),
    stop_paste_linter()
  )

  # not thrown off by named arguments
  expect_lint(
    "stop(paste('a', 'b'), call. = FALSE)",
    rex::rex("Don't use paste to build stop strings."),
    stop_paste_linter()
  )

  expect_lint(
    "warning(paste0('a', 'b'), immediate. = TRUE)",
    rex::rex("Don't use paste0 to build warning strings."),
    stop_paste_linter()
  )
})

test_that("packageStartupMessage usages are also matched", {
  expect_lint(
    "packageStartupMessage(paste('a string', 'another'))",
    rex::rex("Don't use paste to build packageStartupMessage strings."),
    stop_paste_linter()
  )

  expect_lint(
    "packageStartupMessage(paste0('a string ', 'another'))",
    rex::rex("Don't use paste0 to build packageStartupMessage strings."),
    stop_paste_linter()
  )
})
