test_that("basic usage works", {
  linter <- make_linter_from_xpath("//NUM_CONST", "Number")
  expect_type(linter, "closure")
  expect_lint("1", list("Number", type = "warning"), linter())

  expect_lint("'a'", "Letter", make_linter_from_xpath("//STR_CONST", "Letter")())
  expect_lint("'a'", "Letter", make_linter_from_xpath("//STR_CONST", "Letter", level = "file")())
  expect_lint("'a'", list("Letter", type = "style"), make_linter_from_xpath("//STR_CONST", "Letter", type = "style")())
})

test_that("input validation works", {
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

  err_msg <- "xpath should be a character string"
  expect_error(make_linter_from_xpath(FALSE), err_msg, fixed = TRUE)
  expect_error(make_linter_from_xpath(letters), err_msg, fixed = TRUE)
  expect_error(make_linter_from_xpath(NA_character_), err_msg, fixed = TRUE)
  expect_error(make_linter_from_xpath(character()), err_msg, fixed = TRUE)

  err_msg <- "lint_message is required"
  expect_error(make_linter_from_xpath(""), err_msg, fixed = TRUE)
})
