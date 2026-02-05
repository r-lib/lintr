test_that("returns the correct linting", {
  msg_escape_char <- rex::rex("is an unrecognized escape in character string")
  expect_lint('"\\R"', msg_escape_char)
  expect_lint('"\\A"', msg_escape_char)
  expect_lint('"\\z"', msg_escape_char)
  placeholder_linter <- function(...) NULL
  class(placeholder_linter) <- "linter"
  attr(placeholder_linter, "name") <- "null"
  expect_lint(
    "a <- 1
    function() {
    b",
    rex::rex("unexpected end of input"),
    placeholder_linter
  )

  linter <- equals_na_linter()
  expect_lint("x=", rex::rex("unexpected end of input"), linter)
  expect_lint("x += 1", rex::rex("unexpected '='"), linter)
  expect_lint("{x = }", rex::rex("unexpected '}'"), linter)
  expect_lint("x = ;", rex::rex("unexpected ';'"), linter)

  # no parsing error is expected for the equals-assignment in this code
  expect_no_lint("purrr::partial(list, 1, ... = , 2)", linter)

  # trigger error with base only, and extract it to match against
  #   what comes out from expect_lint.
  get_base_message <- function(e) {
    rex::re_substitutes(
      data = conditionMessage(e),
      pattern = rex::rex(
        list(start, "<text>:", any_digits, ":", any_digits, ": ") %or%
          list(newline, anything, newline, anything, end)
      ),
      replacement = "",
      global = TRUE
    )
  }

  expected_message <- tryCatch(parse(text = "\\"), error = get_base_message)
  expect_lint("\\", rex::rex(expected_message))

  msg_zero_length_var <- rex::rex("attempt to use zero-length variable name")
  expect_lint("``", msg_zero_length_var)
  expect_lint("``()", msg_zero_length_var)
  expect_lint("''()", msg_zero_length_var)
  expect_lint('""()', msg_zero_length_var)
  expect_lint("fun(''=42)", msg_zero_length_var)
  expect_lint('fun(""=42)', msg_zero_length_var)
  expect_lint('fun(a=1,""=42)', msg_zero_length_var)
})
