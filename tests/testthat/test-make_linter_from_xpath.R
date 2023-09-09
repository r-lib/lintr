test_that("basic usage works", {
  linter <- make_linter_from_xpath("//NUM_CONST", "Number")
  expect_true(is.function(linter))
  expect_lint("1", "Number", linter())
})

test_that("input validation works", {
  expect_error(
    make_linter_from_xpath("//NUM_CONST", "Number", type = "x"),
    'one of "style", "warning", "error"',
    fixed = TRUE
  )

  expect_error(
    make_linter_from_xpath("//NUM_CONST", "Number", level = "x"),
    'one of "expression", "file"',
  )

  expect_error(
    make_linter_from_xpath(FALSE),
    "xpath should be a character string",
    fixed = TRUE
  )

  expect_error(
    make_linter_from_xpath(letters),
    "xpath should be a character string",
    fixed = TRUE
  )

  expect_error(
    make_linter_from_xpath(NA_character_),
    "xpath should be a character string",
    fixed = TRUE
  )

  expect_error(
    make_linter_from_xpath(character()),
    "xpath should be a character string",
    fixed = TRUE
  )
})
