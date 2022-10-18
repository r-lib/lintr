# styler: off
skip_if_not_installed("tibble")
local({
  # Note: cases indicated by "*" are whole numbers, but don't lint because the user has
  #   effectively declared "this is a double" much as adding '.0' is otherwise accepted.
  cases <- tibble::tribble(
    ~num_value_str,         ~should_lint,
    "Inf",                  FALSE,
    "NaN",                  FALSE,
    "TRUE",                 FALSE,
    "FALSE",                FALSE,
    "NA",                   FALSE,
    "NA_character_",        FALSE,
    "2.000",                FALSE,
    "2.",                   FALSE,
    "2L",                   FALSE,
    "2.0",                  FALSE,
    "2.1",                  FALSE,
    "2",                    TRUE,
    "1e3",                  TRUE,
    "1e3L",                 FALSE,
    "1.0e3L",               FALSE,
    "1.2e3",                FALSE, # * ( = 1200)
    "1.2e-3",               FALSE,
    "1e-3",                 FALSE,
    "1e-33",                FALSE,
    "1.2e0",                FALSE,
    "0x1p+0",               FALSE, # * ( = 1)
    "0x1.ecp+6L",           FALSE,
    "0x1.ecp+6",            FALSE, # * ( = 123)
    "0x1.ec66666666666p+6", FALSE,
    "8i",                   FALSE,
    "8.0i",                 FALSE
  )
  # for convenience of coercing these to string (since tribble doesn't support auto-conversion)
  int_max <- .Machine[["integer.max"]]  # largest number that R can represent as an integer
  cases_int_max <- tibble::tribble(
    ~num_value_str, ~should_lint,
    -int_max - 1.0, FALSE,
    -int_max,       TRUE,
    int_max,        TRUE,
    int_max + 1.0,  FALSE
  )
  cases_int_max$num_value_str <- as.character(cases_int_max$num_value_str)
  cases <- rbind(cases, cases_int_max)
  cases$.test_name <- sprintf("num_value_str=%s, should_lint=%s", cases$num_value_str, cases$should_lint)

  linter <- implicit_integer_linter()
  patrick::with_parameters_test_that(
    "single numerical constants are properly identified ",
    expect_lint(num_value_str, if (should_lint) "Integers should not be implicit", linter),
    .cases = cases
  )
})
# styler: on

test_that("linter returns the correct linting", {
  lint_msg <- "Integers should not be implicit. Use the form 1L for integers or 1.0 for doubles."
  linter <- implicit_integer_linter()

  expect_lint("x <<- 1L", NULL, linter)
  expect_lint("1.0/-Inf -> y", NULL, linter)
  expect_lint(
    "y <- 1+i",
    list(message = lint_msg, line_number = 1L, column_number = 7L),
    linter
  )
  expect_lint(
    "z <- 1e5",
    list(message = lint_msg, line_number = 1L, column_number = 9L),
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
      list(message = lint_msg, line_number = 1L, column_number = 4L),
      list(message = lint_msg, line_number = 1L, column_number = 6L)
    ),
    linter
  )
})

skip_if_not_installed("tibble")
patrick::with_parameters_test_that(
  "numbers in a:b input are optionally not linted",
  expect_lint(
    paste0(left, ":", right),
    if (n_lints > 0L) rep(list("Integers should not be implicit"), n_lints),
    implicit_integer_linter(allow_colon = allow_colon)
  ),
  .cases = tibble::tribble(
    ~left,  ~right, ~n_lints, ~allow_colon, ~.test_name,
    "1",    "1",    2L,       FALSE,        "1:1, !allow_colon",
    "1",    "1",    0L,       TRUE,         "1:1, allow_colon",
    "1",    "1L",   1L,       FALSE,        "1:1L, !allow_colon",
    "1",    "1L",   0L,       TRUE,         "1:1L, allow_colon",
    "1L",   "1",    1L,       FALSE,        "1L:1, !allow_colon",
    "1L",   "1",    0L,       TRUE,         "1L:1, allow_colon",
    "1L",   "1L",   0L,       FALSE,        "1L:1L, !allow_colon",
    "1L",   "1L",   0L,       TRUE,         "1L:1L, allow_colon"
  )
)
