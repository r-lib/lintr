context("r-linter-line_length")
test_that("returns the correct linting", {

  expect_lint("blah",
    NULL,
    line_length_linter(80))

  expect_lint("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    NULL,
    line_length_linter(80))

  expect_lint("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    "lines should not be more than 80 characters",
    line_length_linter(80))

  expect_lint(
    paste0("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n",
    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),

    list(
      rex("lines should not be more than 80 characters"),
      rex("lines should not be more than 80 characters")),

    line_length_linter(80))

  expect_lint("aaaaaaaaaaaaaaaaaaaa",
    NULL,
    line_length_linter(20))

  expect_lint("aaaaaaaaaaaaaaaaaaaab",
    rex("lines should not be more than 20 characters"),
    line_length_linter(20))

  expect_lint("'☂☂☂☂☂☂☂☂☂☂☂☂☂☂☂☂☂'",
    NULL,
    line_length_linter(20))

  expect_lint("'☂☂☂☂☂☂☂☂☂☂☂☂☂☂☂☂☂☂☂'",
    rex("lines should not be more than 20 characters"),
    line_length_linter(20))

})

context("r-linter-infix_spaces")
test_that("returns the correct linting", {
  ops <- c(
    "+",
    "-",
    "=",
    "==",
    "!=",
    "<=",
    ">=",
    "<-",
    "<",
    ">",
    "->",
    "%%",
    "/",
    "^",
    "*",
    "**",
    "|",
    "||",
    "&",
    "&&",
    "%>%",
    "%Anything%",
    "%+%",
    NULL)

  expect_lint("blah", NULL, infix_spaces_linter)

  for (op in ops) {
    expect_lint(paste0("1 ", op, " 2"), NULL, infix_spaces_linter)

    expect_lint(paste0("1", op, "2"),
      rex("Put spaces around all infix operators."),
      infix_spaces_linter)

    # unary plus and minus can have no space before them
    if (!op %in% ops[1:2]) {
      expect_lint(paste0("1 ", op, "2"),
        rex("Put spaces around all infix operators."),
        infix_spaces_linter)
    }

    expect_lint(paste0("1", op, " 2"),
      rex("Put spaces around all infix operators."),
      infix_spaces_linter)
  }

  expect_lint("b <- 2E+4", NULL, infix_spaces_linter)

  expect_lint("a <- 1e-3", NULL, infix_spaces_linter)

  expect_lint("a[-1]", NULL, infix_spaces_linter)

  expect_lint("a[-1 + 1]", NULL, infix_spaces_linter)

  expect_lint("a[1 + -1]", NULL, infix_spaces_linter)
})

context("r-linter-open_curly")
test_that("returns the correct linting", {

  expect_lint("blah", NULL, open_curly_linter)

  expect_lint("a <- function() {\n}", NULL, open_curly_linter)

  expect_lint("a <- function()\n{\n  1 \n}",
    rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
    open_curly_linter)

  expect_lint("a <- function()\n    {\n  1 \n}",
    rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
    open_curly_linter)

  expect_lint("a <- function()\n\t{\n  1 \n}",
    rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
    open_curly_linter)

  expect_lint("a <- function() {  \n}",
    rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
    open_curly_linter)

  expect_lint("a <- function() { 1 }",
    rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
    open_curly_linter)
})

context("r-linter-closed_curly")
test_that("returns the correct linting", {

  expect_lint("blah",
    NULL,
    closed_curly_linter)

  expect_lint("a <- function() {\n}",
    NULL,
    closed_curly_linter)

  expect_lint("a <- function() { 1 }",
    rex("Closing curly-braces should always be on their own line, unless it's followed by an else."),
    closed_curly_linter)

  expect_lint("a <- if(1) {\n 1} else {\n 2\n}",
    rex("Closing curly-braces should always be on their own line, unless it's followed by an else."),
    closed_curly_linter)

  expect_lint("a <- if(1) {\n 1\n} else {\n 2}",
    rex("Closing curly-braces should always be on their own line, unless it's followed by an else."),
    closed_curly_linter)

  expect_lint("a <- if(1) {\n 1} else {\n 2}",
    list(
      rex("Closing curly-braces should always be on their own line, unless it's followed by an else."),
      rex("Closing curly-braces should always be on their own line, unless it's followed by an else.")
      ),
    closed_curly_linter)
})

context("object_camel_case_linter")
test_that("returns the correct linting", {
  expect_lint("blah",
    NULL,
    object_camel_case_linter)

  expect_lint("invokeRestartInteractively",
    NULL,
    object_camel_case_linter)

  expect_lint("camelCase",
    rex("Variable and function names should be all lowercase."),
    object_camel_case_linter)

  expect_lint("camelCase()",
    rex("Variable and function names should be all lowercase."),
    object_camel_case_linter)

  expect_lint("pack::camelCase",
    NULL,
    object_camel_case_linter)

  expect_lint("pack:::camelCase",
    NULL,
    object_camel_case_linter)

  expect_lint("a(camelCase = 1)",
    NULL,
    object_camel_case_linter)
})

context("object_snake_case_linter")
test_that("returns the correct linting", {
  expect_lint("blah",
    NULL,
    object_snake_case_linter)

  expect_lint("seq_along",
    NULL,
    object_snake_case_linter)

  expect_lint("snake_case",
    rex("Variable and function names should not use underscores."),
    object_snake_case_linter)

  expect_lint("snake_case()",
    rex("Variable and function names should not use underscores."),
    object_snake_case_linter)


  expect_lint("pack::snake_case",
    NULL,
    object_snake_case_linter)

  expect_lint("pack:::snake_case",
    NULL,
    object_snake_case_linter)

  expect_lint("a(snake_case = 1)",
    NULL,
    object_snake_case_linter)
})

context("object_multiple_dots_linter")
test_that("returns the correct linting", {
  expect_lint("blah",
    NULL,
    object_multiple_dots_linter)

  expect_lint("variable.name.test",
     rex("Words within variable and function names should be separated by '_' rather than '.'."),
    object_multiple_dots_linter)
})

test_that("variables from attached external packages are ignored", {
  expect_lint("print.data.frame",
    NULL,
    object_multiple_dots_linter)

  expect_lint("row.names.data.frame",
    NULL,
    object_multiple_dots_linter)
})

context("object_length_linter")
test_that("returns the correct linting", {

  expect_lint("blah",
    NULL,
    object_length_linter())

  expect_lint("very_very_very_very_long_variable_names_are_not_ideal",
    rex("Variable and function names should not be longer than 20 characters."),
    object_length_linter())

  expect_lint("very_very_very_very_long_variable_names_are_not_ideal",
    rex("Variable and function names should not be longer than 40 characters."),
    object_length_linter(length = 40))

})

#context("r-linter-object_usage")
#test_that("returns the correct linting", {
  #expect_lint("blah",
    #NULL,
    #object_usage_linter)

  #expect_lint(
#"function() {
  #a <- 1
  #a
#}",
    #NULL,
    #object_usage_linter)

  #expect_lint(
#"fun <- function(x) {
  #fun(1)
#}
#fun2 <- function(x) {
  #fun(2)
#}",
    #NULL,
    #object_usage_linter)

  #expect_lint(
#"fun <- function() {
  #a <- 1
#}",
    #rex("local variable", anything, "assigned but may not be used"),
    #object_usage_linter)

  #expect_lint(
#"fun <- function() {
  #a <- 1
  #1
#}",
    #rex("local variable", anything, "assigned but may not be used"),
    #object_usage_linter)

  #expect_lint(
#"fun <- function() {
  #a <- 1
#}",
    #rex("local variable", anything, "assigned but may not be used"),
    #object_usage_linter)

  #expect_lint(
#"fun <- function() {
  #a2 <- 1
  #a3
#}",
    #list(
      #rex("local variable", anything, "assigned but may not be used"),
      #rex("no visible binding for global variable ", anything, ", Did you mean")
      #),
    #object_usage_linter)

  #expect_lint(
#"fun <- function() {
  #fnu(1)
#}",
    #rex("no visible global function definition for ", anything, ", Did you mean", anything),
    #object_usage_linter)

  #expect_lint(
#"fun <- function(x) {
  #n(1)
#}",
    #rex("no visible global function definition for ", anything, ", Did you mean", anything),
    #object_usage_linter)

  #test_that("replace_functions_stripped", {
    #expect_lint(
#"fun <- function(x) {
  #n(x) = 1
#}",
    #rex("no visible global function definition for ", anything, ", Did you mean", anything),
    #object_usage_linter)

    #expect_lint(
#"fun <- function(x) {
  #n(x) <- 1
#}",
    #rex("no visible global function definition for ", anything, ", Did you mean", anything),
    #object_usage_linter)
  #})
#})

context("r-linter-errors")
test_that("returns the correct linting", {
  expect_lint("\"\\R\"",
    rex("is an unrecognized escape in character string starting")
    )

  expect_lint("\"\\A\"",
    rex("is an unrecognized escape in character string starting")
    )

  expect_lint("\"\\z\"",
    rex("is an unrecognized escape in character string starting")
    )
})
