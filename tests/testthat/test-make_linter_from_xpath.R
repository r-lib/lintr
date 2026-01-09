test_that("basic usage of make_linter_from_xpath works", {
  linter <- make_linter_from_xpath("//NUM_CONST", "Number")
  expect_type(linter, "closure")
  expect_lint("1", list("Number", type = "warning"), linter())

  expect_lint("'a'", "Letter", make_linter_from_xpath("//STR_CONST", "Letter")())
  expect_lint("'a'", "Letter", make_linter_from_xpath("//STR_CONST", "Letter", level = "file")())
  expect_lint("'a'", list("Letter", type = "style"), make_linter_from_xpath("//STR_CONST", "Letter", type = "style")())
})

test_that("input validation works for make_linter_from_xpath", {
  expect_error(
    make_linter_from_xpath("//NUM_CONST", "Number", type = "x"),
    'one of "warning", "style", "error"',
    fixed = TRUE
  )

  expect_error(
    make_linter_from_xpath("//NUM_CONST", "Number", level = "x"),
    'one of "expression", "file"',
    fixed = TRUE
  )

  err_msg <- "`xpath` must be a character string"
  expect_error(make_linter_from_xpath(FALSE), err_msg, fixed = TRUE)
  expect_error(make_linter_from_xpath(letters), err_msg, fixed = TRUE)
  expect_error(make_linter_from_xpath(NA_character_), err_msg, fixed = TRUE)
  expect_error(make_linter_from_xpath(character()), err_msg, fixed = TRUE)

  expect_error(
    make_linter_from_xpath(""),
    "`lint_message` is required",
    fixed = TRUE
  )
})

test_that("basic usage of make_linter_from_function_xpath works", {
  linter <- make_linter_from_function_xpath(
    "sum",
    "following-sibling::SYMBOL_SUB[text() = 'na.rm']",
    "na.rm"
  )
  expect_type(linter, "closure")
  expect_no_lint("sum()", linter())
  expect_lint("sum(na.rm=TRUE)", "na.rm", linter())
})

test_that("input validation works for make_linter_from_function_xpath", {
  expect_error(
    make_linter_from_function_xpath(1L),
    "`function_names` must be a character vector",
    fixed = TRUE
  )
  expect_error(
    make_linter_from_function_xpath(character()),
    "`function_names` must be a character vector",
    fixed = TRUE
  )

  expect_error(
    make_linter_from_function_xpath("sum", 1L),
    "`xpath` must be a character string",
    fixed = TRUE
  )
  expect_error(
    make_linter_from_function_xpath("sum", character()),
    "`xpath` must be a character string",
    fixed = TRUE
  )
  expect_error(
    make_linter_from_function_xpath("sum", letters),
    "`xpath` must be a character string",
    fixed = TRUE
  )
  expect_error(
    make_linter_from_function_xpath("sum", NA_character_),
    "`xpath` must be a character string",
    fixed = TRUE
  )

  expect_error(
    make_linter_from_function_xpath("sum", "XP"),
    "`lint_message` is required",
    fixed = TRUE
  )
})
