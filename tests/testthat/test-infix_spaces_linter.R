# fuzzer disable: comment_injection
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
  lint_msg <- rex::rex("Put spaces around all infix operators.")

  expect_no_lint("blah", linter)

  for (op in ops) {
    expect_no_lint(paste0("1 ", op, " 2"), linter)
    expect_no_lint(paste0("1 ", op, "\n2"), linter)
    expect_no_lint(paste0("1 ", op, "\n 2"), linter)

    expect_lint(paste0("1", op, "2"), lint_msg, linter)

    # unary plus and minus can have no space before them
    if (!op %in% ops[1L:2L]) {
      expect_lint(paste0("1 ", op, "2"), lint_msg, linter)
    }

    expect_lint(paste0("1", op, " 2"), lint_msg, linter)
  }

  expect_no_lint("b <- 2E+4", linter)
  expect_no_lint("a <- 1e-3", linter)
  expect_no_lint("a[-1]", linter)
  expect_no_lint("a[-1 + 1]", linter)
  expect_no_lint("a[1 + -1]", linter)

  expect_lint("fun(a=1)", lint_msg, linter)
})

test_that("The three `=` are all linted", {
  linter <- infix_spaces_linter()
  lint_msg <- rex::rex("Put spaces around all infix operators.")

  # EQ_ASSIGN in the parse data
  expect_lint("a=1", lint_msg, linter)
  # EQ_FORMALS in the parse data
  expect_lint("foo <- function(x=1) {}", lint_msg, linter)
  # EQ_SUB in the parse data
  expect_lint("foo(x=1)", lint_msg, linter)
})

test_that("exclude_operators works", {
  lint_msg <- rex::rex("Put spaces around all infix operators.")

  expect_no_lint("a+b", infix_spaces_linter(exclude_operators = "+"))
  expect_no_lint(
    trim_some("
      a+b
      a-b
    "),
    infix_spaces_linter(exclude_operators = c("+", "-"))
  )

  # operators match on text, not hidden node
  expect_lint("a<<-1", lint_msg, infix_spaces_linter(exclude_operators = "<-"))
  expect_no_lint("a<<-1", infix_spaces_linter(exclude_operators = "<<-"))
  expect_lint("a:=1", lint_msg, infix_spaces_linter(exclude_operators = "<-"))
  expect_no_lint("a:=1", infix_spaces_linter(exclude_operators = ":="))
  expect_lint("a->>1", lint_msg, infix_spaces_linter(exclude_operators = "->"))
  expect_no_lint("a->>1", infix_spaces_linter(exclude_operators = "->>"))
  expect_no_lint("a%any%1", infix_spaces_linter(exclude_operators = "%%"))
  expect_no_lint("function(a=1) { }", infix_spaces_linter(exclude_operators = "="))
  expect_no_lint("foo(a=1)", infix_spaces_linter(exclude_operators = "="))
})

# more tests specifically for assignment
test_that("assignment cases return the correct linting", {
  linter <- infix_spaces_linter()
  lint_msg <- rex::rex("Put spaces around all infix operators.")

  expect_no_lint("fun(blah =  1)", linter)

  expect_no_lint("blah <- 1", linter)
  expect_no_lint("blah = 1", linter)

  expect_no_lint("\"my  =  variable\" <- 42.0", linter)

  expect_no_lint("if (0 <  1) x <- 42L", linter)
  expect_no_lint(
    trim_some("
    if (0 < 1) {
      x <- 42L
    }"),
    linter
  )
  expect_no_lint("my = bad = variable = name <- 2.0", linter)

  expect_lint("blah<-  1", lint_msg, linter)
  expect_lint("blah  <-1", lint_msg, linter)
  expect_lint("blah=  1", lint_msg, linter)
  expect_lint("blah  =1", lint_msg, linter)
})

test_that("infix_spaces_linter can allow >1 spaces optionally", {
  linter <- infix_spaces_linter(allow_multiple_spaces = FALSE)
  lint_msg <- rex::rex("Put exactly one space on each side of infix operators.")

  expect_lint("x  ~  1", lint_msg, linter)
  expect_lint("x  - 1", lint_msg, linter)
  expect_lint("x /  1", lint_msg, linter)
})

test_that("exception for box::use()", {
  linter <- infix_spaces_linter()

  expect_no_lint("box::use(a/b)", linter)
  expect_no_lint("box::use(./a/b)", linter)
  expect_no_lint(
    trim_some("
      box::use(
        a,
        a/b,
        ../a,
        alias = a/b/c[xyz = abc, ...],
      )
    "),
    linter
  )
})

test_that("multi-line, multi-expression case is caught", {
  expect_lint(
    trim_some("
      x +
        y+
        z
    "),
    rex::rex("Put spaces around all infix operators."),
    infix_spaces_linter()
  )
})

test_that("Rules around missing arguments are respected", {
  linter <- infix_spaces_linter()
  lint_msg <- rex::rex("Put spaces around all infix operators.")

  expect_no_lint("switch(a = , b = 2)", linter)
  expect_no_lint("alist(missing_arg = )", linter)

  expect_lint("switch(a =, b = 2)", lint_msg, linter)
  expect_lint("alist(missing_arg =)", lint_msg, linter)
})

test_that("native pipe is supported", {
  skip_if_not_r_version("4.1.0")
  linter <- infix_spaces_linter()

  expect_no_lint("a |> foo()", linter)
  expect_lint("a|>foo()", rex::rex("Put spaces around all infix operators."), linter)
})

test_that("mixed unary & binary operators aren't mis-lint", {
  expect_lint(
    "-1-1",
    list(
      message = rex::rex("Put spaces around all infix operators."),
      column_number = 3L
    ),
    infix_spaces_linter()
  )
})

test_that("parse tags are accepted by exclude_operators", { # nofuzz: assignment
  expect_no_lint("sum(x, na.rm=TRUE)", infix_spaces_linter(exclude_operators = "EQ_SUB"))
  expect_no_lint("function(x, na.rm=TRUE) { }", infix_spaces_linter(exclude_operators = "EQ_FORMALS"))
  expect_no_lint("x=1", infix_spaces_linter(exclude_operators = "EQ_ASSIGN"))

  # uses parse_tag
  expect_no_lint("1+1", infix_spaces_linter(exclude_operators = "'+'"))

  # mixing
  text <- "x=function(a=foo(bar=1)) { }"
  col_assign <- list(column_number = 2L)
  col_formals <- list(column_number = 13L)
  col_sub <- list(column_number = 21L)
  expect_no_lint(text, infix_spaces_linter(exclude_operators = c("EQ_SUB", "EQ_FORMALS", "EQ_ASSIGN")))
  expect_lint(text, col_assign, infix_spaces_linter(exclude_operators = c("EQ_SUB", "EQ_FORMALS")))
  expect_lint(text, col_formals, infix_spaces_linter(exclude_operators = c("EQ_SUB", "EQ_ASSIGN")))
  expect_lint(text, col_sub, infix_spaces_linter(exclude_operators = c("EQ_FORMALS", "EQ_ASSIGN")))
  expect_lint(text, list(col_assign, col_formals), infix_spaces_linter(exclude_operators = "EQ_SUB"))
  expect_lint(text, list(col_assign, col_sub), infix_spaces_linter(exclude_operators = "EQ_FORMALS"))
  expect_lint(text, list(col_formals, col_sub), infix_spaces_linter(exclude_operators = "EQ_ASSIGN"))
})

test_that("lints vectorize", { # nofuzz: assignment
  lint_msg <- rex::rex("Put spaces around all infix operators.")

  expect_lint(
    trim_some("{
      a<-1
      1/2
      b<-c<-2
      d+e+f+g/3
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L),
      list(lint_msg, line_number = 4L, column_number = 4L),
      list(lint_msg, line_number = 4L, column_number = 7L),
      list(lint_msg, line_number = 5L, column_number = 4L),
      list(lint_msg, line_number = 5L, column_number = 6L),
      list(lint_msg, line_number = 5L, column_number = 8L),
      list(lint_msg, line_number = 5L, column_number = 10L)
    ),
    infix_spaces_linter()
  )
})
# fuzzer enable: comment_injection
