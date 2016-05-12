context("trailing_semicolons_linter")

test_that("finds trailing semicolons", {
  expect_lint("function() { 42 }\n",
    NULL,
    trailing_semicolons_linter)

  expect_lint("function() { \"But here\";\n\"  and here\"; }",
    rex("trailing semicolons"),
    trailing_semicolons_linter)
})

test_that("finds trailing semicolons even with trailing whitespace", {
  expect_lint("function() { \"Here, too!\";  \n }\n",
    rex("trailing semicolons"),
    trailing_semicolons_linter)
})

test_that("ignores non-trailing semicolons", {
  expect_lint("function() { 1 ; 2 ; \"is good\" }\n",
    NULL,
    trailing_semicolons_linter)
})
