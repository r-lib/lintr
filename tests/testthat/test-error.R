test_that("returns the correct linting", {
  msg_escape_char <- rex("is an unrecognized escape in character string starting")
  expect_lint("\"\\R\"", msg_escape_char)
  expect_lint("\"\\A\"", msg_escape_char)
  expect_lint("\"\\z\"", msg_escape_char)
  expect_lint(
    "a <- 1
    function() {
    b",
    rex("unexpected end of input"),
    structure(function(...) NULL, class = "linter", name = "null")
  )

  linter <- equals_na_linter()
  expect_lint("x=", rex("unexpected end of input"), linter)
  expect_lint("x += 1", rex("unexpected '='"), linter)
  expect_lint("{x = }", rex("unexpected '}'"), linter)
  expect_lint("x = ;", rex("unexpected ';'"), linter)

  # no parsing error is expected for the equals-assignment in this code
  expect_lint(
    "purrr::partial(list, 1, ... = , 2)",
    NULL,
    linter
  )

  # Error message changed in R-devel as of 2020/12
  old_lang <- lintr:::set_lang("en")
  on.exit(lintr:::reset_lang(old_lang), add = TRUE)

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
  expect_lint("\\", rex(expected_message))

  msg_zero_length_var <- rex("attempt to use zero-length variable name")
  expect_lint("``", msg_zero_length_var)
  expect_lint("``()", msg_zero_length_var)
  expect_lint("''()", msg_zero_length_var)
  expect_lint('""()', msg_zero_length_var)
  expect_lint("fun(''=42)", msg_zero_length_var)
  expect_lint('fun(""=42)', msg_zero_length_var)
  expect_lint('fun(a=1,""=42)', msg_zero_length_var)
})
