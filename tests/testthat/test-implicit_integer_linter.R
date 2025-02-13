# styler: off
skip_if_not_installed("tibble")
local({
  # Note: cases indicated by "*" are whole numbers, but don't lint because the user has
  #   effectively declared "this is a double" much as adding '.0' is otherwise accepted.
  cases <- tibble::tribble(
    ~num_value_str,         ~lint_msg,
    "Inf",                  "",
    "NaN",                  "",
    "TRUE",                 "",
    "FALSE",                "",
    "NA",                   "",
    "NA_character_",        "",
    "2.000",                "",
    "2.",                   "",
    "2L",                   "",
    "2.0",                  "",
    "2.1",                  "",
    "2",                    "2L or 2.0",
    "1e3",                  "1000L or 1000.0",
    "1e3L",                 "",
    "1.0e3L",               "",
    "1.2e3",                "", # * ( = 1200)
    "1.2e-3",               "",
    "1e-3",                 "",
    "1e-33",                "",
    "1.2e0",                "",
    "0x1p+0",               "", # * ( = 1)
    "0x1.ecp+6L",           "",
    "0x1.ecp+6",            "", # * ( = 123)
    "0x1.ec66666666666p+6", "",
    "8i",                   "",
    "8.0i",                 ""
  )
  # for convenience of coercing these to string (since tribble doesn't support auto-conversion)
  int_max <- .Machine[["integer.max"]]  # largest number that R can represent as an integer
  cases_int_max <- tibble::tribble(
    ~num_value_str, ~lint_msg,
    -int_max - 1.0, "",
    -int_max,       sprintf("%1$dL or %1$d.0", -int_max),
    int_max,        sprintf("%1$dL or %1$d.0", int_max),
    int_max + 1.0,  ""
  )
  cases_int_max$num_value_str <- as.character(cases_int_max$num_value_str)
  cases <- rbind(cases, cases_int_max)

  linter <- implicit_integer_linter()
  patrick::with_parameters_test_that(
    "single numerical constants are properly identified ",
    expect_lint(num_value_str, if (nzchar(lint_msg)) lint_msg, linter),
    .cases = cases
  )
})
# styler: on

test_that("linter returns the correct linting", {
  linter <- implicit_integer_linter()
  lint_msg <- rex::rex("Use 1L or 1.0 to avoid implicit integers.")

  expect_lint("x <<- 1L", NULL, linter)
  expect_lint("1.0/-Inf -> y", NULL, linter)
  expect_lint(
    "y <- 1+i",
    list(message = lint_msg, line_number = 1L, column_number = 7L),
    linter
  )
  expect_lint(
    "z <- 1e5",
    list(message = rex::rex("100000L or 100000.0"), line_number = 1L, column_number = 9L),
    linter
  )
  expect_lint(
    "cat(1:n)",
    list(message = lint_msg, line_number = 1L, column_number = 6L),
    linter
  )
  expect_lint(
    "552^9",
    list(
      list(message = rex::rex("552L or 552.0"), line_number = 1L, column_number = 4L),
      list(message = rex::rex("9L or 9.0"), line_number = 1L, column_number = 6L)
    ),
    linter
  )
})

skip_if_not_installed("tibble")
patrick::with_parameters_test_that(
  "Under allow_colon={allow_colon}, {left}:{right} throws {n_lints} lints",
  expect_lint(
    paste0(left, ":", right),
    if (n_lints > 0L) rep(list(rex::rex("Use 1L or 1.0")), n_lints),
    implicit_integer_linter(allow_colon = allow_colon)
  ),
  .cases = tibble::tribble(
    ~left,  ~right, ~n_lints, ~allow_colon,
    "1",    "1",    2L,       FALSE,
    "1",    "1",    0L,       TRUE,
    "1",    "1L",   1L,       FALSE,
    "1",    "1L",   0L,       TRUE,
    "1L",   "1",    1L,       FALSE,
    "1L",   "1",    0L,       TRUE,
    "1L",   "1L",   0L,       FALSE,
    "1L",   "1L",   0L,       TRUE
  )
)
