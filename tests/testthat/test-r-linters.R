context("r-linter-single_quotes")
test_that("returns the correct linting", {
  expect_lint("blah", NULL, single_quotes_linter)

  expect_lint("\"blah\"", NULL, single_quotes_linter)

  expect_lint("\"'blah\"", NULL, single_quotes_linter)

  expect_lint("\"blah'\"", NULL, single_quotes_linter)

  expect_lint("\"blah'\"", NULL, single_quotes_linter)

  expect_lint("\"'blah'\"", NULL, single_quotes_linter)

  expect_lint("'blah'",
    rex("Only use double-quotes."),
    single_quotes_linter)

  expect_lint("fun('blah')",
    rex("Only use double-quotes."),
    single_quotes_linter)

  expect_lint("{'blah'}",
    rex("Only use double-quotes."),
    single_quotes_linter)
})

context("r-linter-assignment")
test_that("returns the correct linting", {
  expect_lint("blah", NULL, assignment_linter)

  expect_lint("blah <- 1", NULL, assignment_linter)

  expect_lint("blah<-1", NULL, assignment_linter)

  expect_lint("fun(blah=1)", NULL, assignment_linter)

  expect_lint("blah=1",
    rex("Use <-, not =, for assignment."),
      assignment_linter)

    expect_lint("blah = 1",
      rex("Use <-, not =, for assignment."),
        assignment_linter)

  expect_lint("blah = fun(1)",
    rex("Use <-, not =, for assignment.")
    , assignment_linter)

  expect_lint("blah = fun(1) {",
    list(
      rex("Use <-, not =, for assignment."),
      c(type = "error", "unexpected")
      ),
      assignment_linter)

  expect_lint("fun((blah = fun(1)))",
    rex("Use <-, not =, for assignment."),
    assignment_linter)
})

context("r-linter-absolute_paths")
test_that("returns the correct linting", {
  expect_lint("blah", NULL, absolute_paths_linter)

  expect_lint("'blah'", NULL, absolute_paths_linter)

  expect_lint("'blah/file.txt'", NULL, absolute_paths_linter)

  expect_lint("'blah\\file.txt'", NULL, absolute_paths_linter)

  expect_lint("'../blah/file.txt'", NULL, absolute_paths_linter)

  expect_lint("'//blah/file.txt'", NULL, absolute_paths_linter)

  expect_lint("'~'", NULL, absolute_paths_linter)

  expect_lint("# 'C:/blah/file.txt'", NULL, absolute_paths_linter)

  expect_lint("'/blah/file.txt'",
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("\"/blah/file.txt\"",
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("'c:/blah/file.txt'",
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("'C:/blah/file.txt'",
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("'E:/blah/file.txt'",
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("'E:\\blah\\file.txt'",
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("'~/blah/file.txt'",
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("'~james.hester/blah/file.txt'",
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("'~/'",
    rex("Do not use absolute paths."),
    absolute_paths_linter)
})

context("r-linter-no_tabs")
test_that("returns the correct linting", {

  expect_lint("blah", NULL, no_tab_linter)

  expect_lint("  blah", NULL, no_tab_linter)

  expect_lint("  blah", NULL, no_tab_linter)

  expect_lint("#\tblah", NULL, no_tab_linter)

  expect_lint("\tblah",
    rex("Use two spaces to indent, never tabs."),
      no_tab_linter)

  expect_lint("\t\tblah",
    rex("Use two spaces to indent, never tabs."),
      no_tab_linter)

})

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

})

context("r-linter-commas")
test_that("returns the correct linting", {

  expect_lint("blah", NULL, commas_linter)

  expect_lint("fun(1, 1)", NULL, commas_linter)

  expect_lint("fun(1,\n  1)", NULL, commas_linter)

  expect_lint("fun(1,\n1)", NULL, commas_linter)

  expect_lint("fun(1\n,\n1)", NULL, commas_linter)

  expect_lint("fun(1\n  ,\n1)", NULL, commas_linter)

  expect_lint("fun(1\n,1)",
    rex("Commas should always have a space after."),
    commas_linter)

  expect_lint("fun(1,1)",
    rex("Commas should always have a space after."),
    commas_linter)

  expect_lint("fun(1 ,1)",
    list(
      rex("Commas should never have a space before."),
      rex("Commas should always have a space after.")
      ),
    commas_linter)
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

context("r-linter-spaces-left_parentheses")
test_that("returns the correct linting", {

  expect_lint("blah", NULL, spaces_left_parentheses_linter)

  expect_lint("print(blah)", NULL, spaces_left_parentheses_linter)

  expect_lint("base::print(blah)", NULL, spaces_left_parentheses_linter)

  expect_lint("base::print(blah, fun(1))",
    NULL,
    spaces_left_parentheses_linter)

  expect_lint("blah <- function(blah) { }",
    NULL,
    spaces_left_parentheses_linter)

  expect_lint("(1 + 1)", NULL, spaces_left_parentheses_linter)

  expect_lint("( (1 + 1) )", NULL, spaces_left_parentheses_linter)

  expect_lint("if (blah) { }", NULL, spaces_left_parentheses_linter)

  expect_lint("for (i in j) { }", NULL, spaces_left_parentheses_linter)

  expect_lint("1 * (1 + 1)", NULL, spaces_left_parentheses_linter)

  expect_lint("((1 + 1))",
    rex("Place a space before left parenthesis, except in a function call."),
    spaces_left_parentheses_linter)

  expect_lint("if(blah) { }",
    rex("Place a space before left parenthesis, except in a function call."),
    spaces_left_parentheses_linter)

  expect_lint("for(i in j) { }",
    rex("Place a space before left parenthesis, except in a function call."),
    spaces_left_parentheses_linter)

  expect_lint("1*(1 + 1)",
    rex("Place a space before left parenthesis, except in a function call."),
    spaces_left_parentheses_linter)

  expect_lint("test <- function(x) { if(1 + 1) 'hi' }",
    rex("Place a space before left parenthesis, except in a function call."),
    spaces_left_parentheses_linter)
})

context("r-linter-spaces-inside")
test_that("returns the correct linting", {

  expect_lint("blah", NULL, spaces_inside_linter)

  expect_lint("print(blah)", NULL, spaces_inside_linter)

  expect_lint("base::print(blah)", NULL, spaces_inside_linter)

  expect_lint("a[, ]", NULL, spaces_inside_linter)

  expect_lint("a[,]", NULL, spaces_inside_linter)

  expect_lint("a[1]", NULL, spaces_inside_linter)

  expect_lint("fun(\na[1]\n  )", NULL, spaces_inside_linter)

  expect_lint("a[1 ]",
    rex("Do not place spaces around code in parentheses or square brackets."),
    spaces_inside_linter)

  expect_lint("a[ 1]",
    rex("Do not place spaces around code in parentheses or square brackets."),
    spaces_inside_linter)

  expect_lint("a[ 1 ]",
    list("Do not place spaces around code in parentheses or square brackets.",
    rex("Do not place spaces around code in parentheses or square brackets.")),
    spaces_inside_linter)

  expect_lint("a(, )", NULL, spaces_inside_linter)

  expect_lint("a(,)", NULL, spaces_inside_linter)

  expect_lint("a(1)", NULL, spaces_inside_linter)

  expect_lint("a(1 )",
    rex("Do not place spaces around code in parentheses or square brackets."),
    spaces_inside_linter)

  expect_lint("a( 1)",
    rex("Do not place spaces around code in parentheses or square brackets."),
    spaces_inside_linter)

  expect_lint("a( 1 )",
    list("Do not place spaces around code in parentheses or square brackets.",
    rex("Do not place spaces around code in parentheses or square brackets.")),
    spaces_inside_linter)
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

context("r-linter-objects")
test_that("returns the correct linting", {

  expect_lint("blah",
    NULL,
    object_name_linter())

  #variables from attached external packages are ignored
  expect_lint("print.data.frame",
    NULL,
    object_name_linter())

  expect_lint("row.names.data.frame",
    NULL,
    object_name_linter())

  expect_lint("invokeRestartInteractively",
    NULL,
    object_name_linter())

  expect_lint("camelCase",
    rex("Variable and function names should be all lowercase."),
    object_name_linter())

  expect_lint("pack::camelCase",
    NULL,
    object_name_linter())

  expect_lint("pack:::camelCase",
    NULL,
    object_name_linter())

  expect_lint("a(camelCase = 1)",
    NULL,
    object_name_linter())

  expect_lint("variable.name.test",
     rex("Words within variable and function names should be separated by '_' rather than '.'."),
    object_name_linter())

  expect_lint("very_very_very_very_long_variable_names_are_not_ideal",
    rex("Variable and function names should not be longer than 20 characters."),
    object_name_linter())

  expect_lint("very_very_very_very_long_variable_names_are_not_ideal",
    rex("Variable and function names should not be longer than 40 characters."),
    object_name_linter(width = 40))
})

context("r-linter-object_usage")
test_that("returns the correct linting", {
  expect_lint("blah",
    NULL,
    object_usage_linter)

  expect_lint(
"function() {
  a <- 1
  a
}",
    NULL,
    object_usage_linter)

  expect_lint(
"fun <- function(x) {
  fun(1)
}
fun2 <- function(x) {
  fun(2)
}",
    NULL,
    object_usage_linter)

  expect_lint(
"fun <- function() {
  a <- 1
}",
    rex("local variable", anything, "assigned but may not be used"),
    object_usage_linter)

  expect_lint(
"fun <- function() {
  a <- 1
  1
}",
    rex("local variable", anything, "assigned but may not be used"),
    object_usage_linter)

  expect_lint(
"fun <- function() {
  a <- 1
}",
    rex("local variable", anything, "assigned but may not be used"),
    object_usage_linter)

  expect_lint(
"fun <- function() {
  a2 <- 1
  a3
}",
    list(
      rex("no visible binding for global variable ", anything, ", Did you mean"),
      rex("local variable", anything, "assigned but may not be used")
      ),
    object_usage_linter)

  expect_lint(
"fun <- function() {
  fnu(1)
}",
    rex("no visible global function definition for ", anything, ", Did you mean", anything),
    object_usage_linter)

  expect_lint(
"fun <- function(x) {
  n(1)
}",
    rex("no visible global function definition for ", anything, ", Did you mean", anything),
    object_usage_linter)
})
