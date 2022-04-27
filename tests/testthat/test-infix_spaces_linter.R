test_that("returns the correct linting", {
  ops <- c(
    "+",
    "-",
    "~",
    "=",
    "==",
    "!=",
    "<=",
    ">=",
    "<-",
    ":=",
    "<<-",
    "<",
    ">",
    "->",
    "->>",
    "%%",
    "/",
    "*",
    "|",
    "||",
    "&",
    "&&",
    "%>%",
    "%Anything%",
    "%+%",
    NULL
  )

  linter <- infix_spaces_linter()
  msg <- rex::rex("Put spaces around all infix operators.")

  expect_lint("blah", NULL, linter)

  for (op in ops) {
    expect_lint(paste0("1 ", op, " 2"), NULL, linter)
    expect_lint(paste0("1 ", op, "\n2"), NULL, linter)
    expect_lint(paste0("1 ", op, "\n 2"), NULL, linter)

    expect_lint(paste0("1", op, "2"), msg, linter)

    # unary plus and minus can have no space before them
    if (!op %in% ops[1:2]) {
      expect_lint(paste0("1 ", op, "2"), msg, linter)
    }

    expect_lint(paste0("1", op, " 2"), msg, linter)
  }

  expect_lint("b <- 2E+4", NULL, linter)
  expect_lint("a <- 1e-3", NULL, linter)
  expect_lint("a[-1]", NULL, linter)
  expect_lint("a[-1 + 1]", NULL, linter)
  expect_lint("a[1 + -1]", NULL, linter)

  expect_lint("fun(a=1)", msg, linter)
})

test_that("The three `=` are all linted", {
  linter <- infix_spaces_linter()
  msg <- rex::rex("Put spaces around all infix operators.")

  # EQ_ASSIGN in the parse data
  expect_lint("a=1", msg, linter)
  # EQ_FORMALS in the parse data
  expect_lint("foo <- function(x=1) {}", msg, linter)
  # EQ_SUB in the parse data
  expect_lint("foo(x=1)", msg, linter)
})

test_that("exclude_operators works", {
  expect_lint("a+b", NULL, infix_spaces_linter(exclude_operators = "+"))
  expect_lint(
    trim_some("
      a+b
      a-b
    "),
    NULL,
    infix_spaces_linter(exclude_operators = c("+", "-"))
  )

  # grouped operators
  expect_lint("a<<-1", NULL, infix_spaces_linter(exclude_operators = "<-"))
  expect_lint("a:=1", NULL, infix_spaces_linter(exclude_operators = "<-"))
  expect_lint("a->>1", NULL, infix_spaces_linter(exclude_operators = "->"))
  expect_lint("a%any%1", NULL, infix_spaces_linter(exclude_operators = "%%"))
  expect_lint("function(a=1) { }", NULL, infix_spaces_linter(exclude_operators = "="))
  expect_lint("foo(a=1)", NULL, infix_spaces_linter(exclude_operators = "="))
})

# more tests specifically for assignment
test_that("assignment cases return the correct linting", {
  linter <- infix_spaces_linter()
  msg <- rex::rex("Put spaces around all infix operators.")

  expect_lint("fun(blah =  1)", NULL, linter)

  expect_lint("blah <- 1", NULL, linter)
  expect_lint("blah = 1", NULL, linter)

  expect_lint("\"my  =  variable\" <- 42.0", NULL, linter)

  expect_lint("if (0 <  1) x <- 42L", NULL, linter)
  expect_lint(
    trim_some("
    if (0 < 1) {
      x <- 42L
    }"),
    NULL,
    linter
  )
  expect_lint("my = bad = variable = name <- 2.0", NULL, linter)

  expect_lint("blah<-  1", msg, linter)
  expect_lint("blah  <-1", msg, linter)
  expect_lint("blah=  1", msg, linter)
  expect_lint("blah  =1", msg, linter)
})

test_that("infix_spaces_linter can allow >1 spaces optionally", {
  expect_lint(
    "x  ~  1",
    rex::rex("Put exactly one space on each side of infix operators."),
    infix_spaces_linter(allow_multiple_spaces = FALSE)
  )
  expect_lint(
    "x  - 1",
    rex::rex("Put exactly one space on each side of infix operators."),
    infix_spaces_linter(allow_multiple_spaces = FALSE)
  )
  expect_lint(
    "x /  1",
    rex::rex("Put exactly one space on each side of infix operators."),
    infix_spaces_linter(allow_multiple_spaces = FALSE)
  )
})

test_that("exception for box::use()", {
  linter <- infix_spaces_linter()

  expect_lint("box::use(a/b)", NULL, linter)
  expect_lint("box::use(./a/b)", NULL, linter)
  expect_lint(
    trim_some("
      box::use(
        a,
        a/b,
        ../a,
        alias = a/b/c[xyz = abc, ...],
      )
    "),
    NULL,
    linter
  )
})
