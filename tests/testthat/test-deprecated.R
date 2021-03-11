expect_quiet_lint <- function(...) {
  suppressWarnings(expect_lint(...))
}

test_that("absolute_paths_linter() returns the correct linting", {
  expect_quiet_lint("blah", NULL, absolute_paths_linter)
  expect_quiet_lint("'/blah/file.txt'",
                    rex("Do not use absolute paths."),
                    absolute_paths_linter)
})

test_that("trailing_semicolons_linter() finds trailing semicolons", {
  expect_quiet_lint("function() { 1 ; 2 ; \"is good\" }\n",
                    NULL,
                    trailing_semicolons_linter)
  expect_quiet_lint("function() { \"But here\";\n }",
                    rex("Trailing semicolons"),
                    trailing_semicolons_linter)
})

test_that("camel_case_linter() returns the correct linting", {
  expect_quiet_lint("blah",
                    NULL,
                    camel_case_linter)
  expect_quiet_lint("camelCase <- t",
                    rex("Variable and function names should be all lowercase."),
                    camel_case_linter)
})

test_that("snake_case_linter() returns the correct linting", {
  expect_quiet_lint("blah",
                    NULL,
                    snake_case_linter)
  expect_quiet_lint("snake_case <- t",
                    rex("Variable and function names should not use underscores."),
                    snake_case_linter)
})

test_that("multiple_dots_linter() returns the correct linting", {
  expect_quiet_lint("blah",
                    NULL,
                    multiple_dots_linter)
  expect_quiet_lint("variable.name.test <- t",
                    rex("Words within variable and function names should be separated by '_'"),
                    multiple_dots_linter)
})
