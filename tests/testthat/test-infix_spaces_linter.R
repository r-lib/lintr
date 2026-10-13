# fuzzer disable: comment_injection
min_one_msg <- function(op) rex::rex("Put spaces around `", op, "`.")
one_msg <- function(op) rex::rex("Put exactly one space on each side of `", op, "`.")
none_msg <- function(op) rex::rex("Put no spaces around `", op, "`.")

# exclude_operators= is deprecated in favor of overrides = list(any = ...)
linter_excluding <- function(exclude_operators) {
  expect_warning(
    {
      linter <- infix_spaces_linter(exclude_operators = exclude_operators)
    },
    "exclude_operators.*deprecated"
  )
  linter
}

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

  expect_no_lint("blah", linter)

  for (op in ops) {
    expect_no_lint(paste0("1 ", op, " 2"), linter)
    expect_no_lint(paste0("1 ", op, "\n2"), linter)
    expect_no_lint(paste0("1 ", op, "\n 2"), linter)

    expect_lint(paste0("1", op, "2"), min_one_msg(op), linter)

    # unary plus and minus can have no space before them
    if (!op %in% ops[1L:2L]) {
      expect_lint(paste0("1 ", op, "2"), min_one_msg(op), linter)
    }

    expect_lint(paste0("1", op, " 2"), min_one_msg(op), linter)
  }

  expect_no_lint("b <- 2E+4", linter)
  expect_no_lint("a <- 1e-3", linter)
  expect_no_lint("a[-1]", linter)
  expect_no_lint("a[-1 + 1]", linter)
  expect_no_lint("a[1 + -1]", linter)

  expect_lint("fun(a=1)", min_one_msg("="), linter)
})

test_that("The three `=` are all linted", {
  linter <- infix_spaces_linter()

  # EQ_ASSIGN in the parse data
  expect_lint("a=1", min_one_msg("="), linter)
  # EQ_FORMALS in the parse data
  expect_lint("foo <- function(x=1) {}", min_one_msg("="), linter)
  # EQ_SUB in the parse data
  expect_lint("foo(x=1)", min_one_msg("="), linter)
})

test_that("exclude_operators works", {
  expect_no_lint("a+b", linter_excluding("+"))
  expect_no_lint(
    trim_some("
      a+b
      a-b
    "),
    linter_excluding(c("+", "-"))
  )

  # operators match on text, not hidden node
  expect_lint("a<<-1", min_one_msg("<<-"), linter_excluding("<-"))
  expect_no_lint("a<<-1", linter_excluding("<<-"))
  expect_lint("a:=1", min_one_msg(":="), linter_excluding("<-"))
  expect_no_lint("a:=1", linter_excluding(":="))
  expect_lint("a->>1", min_one_msg("->>"), linter_excluding("->"))
  expect_no_lint("a->>1", linter_excluding("->>"))
  expect_no_lint("a%any%1", linter_excluding("%%"))
  expect_no_lint("function(a=1) { }", linter_excluding("="))
  expect_no_lint("foo(a=1)", linter_excluding("="))
})

test_that("overrides = list(any = ...) works", {
  expect_no_lint("a+b", infix_spaces_linter(overrides = list(any = "+")))
  expect_no_lint(
    trim_some("
      a+b
      a-b
    "),
    infix_spaces_linter(overrides = list(any = c("+", "-")))
  )

  # operators match on text, not hidden node
  expect_lint("a<<-1", min_one_msg("<<-"), infix_spaces_linter(overrides = list(any = "<-")))
  expect_no_lint("a<<-1", infix_spaces_linter(overrides = list(any = "<<-")))
  expect_lint("a:=1", min_one_msg(":="), infix_spaces_linter(overrides = list(any = "<-")))
  expect_no_lint("a:=1", infix_spaces_linter(overrides = list(any = ":=")))
  expect_lint("a->>1", min_one_msg("->>"), infix_spaces_linter(overrides = list(any = "->")))
  expect_no_lint("a->>1", infix_spaces_linter(overrides = list(any = "->>")))
  expect_no_lint("a%any%1", infix_spaces_linter(overrides = list(any = "%%")))
  expect_no_lint("function(a=1) { }", infix_spaces_linter(overrides = list(any = "=")))
  expect_no_lint("foo(a=1)", infix_spaces_linter(overrides = list(any = "=")))
})

test_that("overrides = list(none = ...) works", { # nofuzz: assignment
  linter <- infix_spaces_linter(overrides = list(none = "EQ_SUB"))

  expect_no_lint("foo(a=1)", linter)
  expect_no_lint("foo(a=1, b=2)", linter)
  expect_lint("foo(a = 1)", list(none_msg("="), column_number = 7L), linter)
  expect_lint("foo(a= 1)", none_msg("="), linter)
  expect_lint("foo(a =1)", none_msg("="), linter)
  expect_lint("foo(a  =  1)", none_msg("="), linter)

  # other operators keep the default style
  expect_no_lint("x <- foo(a=1)", linter)
  expect_lint("x<-foo(a=1)", min_one_msg("<-"), linter)
  expect_lint("x<-foo(a = 1)", list(min_one_msg("<-"), none_msg("=")), linter)
  expect_lint("x=1", min_one_msg("="), linter)
  expect_lint("function(x=1) NULL", min_one_msg("="), linter)

  # newlines are not spaces
  expect_no_lint(
    trim_some("
      foo(a=
        1)
    "),
    linter
  )

  expect_no_lint("x<-1", infix_spaces_linter(overrides = list(none = "<-")))
  expect_lint("x <- 1", none_msg("<-"), infix_spaces_linter(overrides = list(none = "<-")))
  expect_lint("1 %in% 2", none_msg("%in%"), infix_spaces_linter(overrides = list(none = "%%")))

  # several operators, and several styles
  linter <- infix_spaces_linter(overrides = list(none = c("EQ_SUB", "EQ_FORMALS"), any = "%%"))
  expect_no_lint("function(a=1) foo(b=2, 1%in%2)", linter)
  expect_lint("function(a = 1) foo(b = 2)", list(none_msg("="), none_msg("=")), linter)
})

test_that("overrides = list(one = ...) and list(min_one = ...) work", { # nofuzz: assignment

  linter <- infix_spaces_linter(overrides = list(one = "<-"))
  expect_no_lint("x <- 1", linter)
  expect_lint("x  <-  1", one_msg("<-"), linter)
  expect_no_lint("x  ==  1", linter)

  linter <- infix_spaces_linter(default_style = "one", overrides = list(min_one = "<-"))
  expect_no_lint("x  <-  1", linter)
  expect_lint("x  ==  1", one_msg("=="), linter)
  expect_lint("x<-1", min_one_msg("<-"), linter)
})

test_that("overrides is validated", {
  expect_error(infix_spaces_linter(overrides = c(none = "+")), "must be a named list")
  expect_error(infix_spaces_linter(overrides = list("+")), "must be a named list")
  expect_error(infix_spaces_linter(overrides = list(foo = "+")), 'must be "min_one", "one", "none" or "any"')
  expect_error(infix_spaces_linter(overrides = list(none = 1L)), "Unknown operator 1")
  expect_error(infix_spaces_linter(overrides = list(none = "^")), 'Unknown operator "\\^"')
  expect_error(infix_spaces_linter(overrides = list(none = "+", any = "+")), "given more than once")
  # "=" covers all three parse tags
  expect_error(infix_spaces_linter(overrides = list(none = "=", any = "EQ_SUB")), "given more than once")
})

# more tests specifically for assignment
test_that("assignment cases return the correct linting", {
  linter <- infix_spaces_linter()

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

  expect_lint("blah<-  1", min_one_msg("<-"), linter)
  expect_lint("blah  <-1", min_one_msg("<-"), linter)
  expect_lint("blah=  1", min_one_msg("="), linter)
  expect_lint("blah  =1", min_one_msg("="), linter)
})

test_that("infix_spaces_linter can allow >1 spaces optionally", {
  expect_warning(
    {
      linter <- infix_spaces_linter(allow_multiple_spaces = FALSE)
    },
    "allow_multiple_spaces.*deprecated"
  )

  expect_lint("x  ~  1", one_msg("~"), linter)
  expect_lint("x  - 1", one_msg("-"), linter)
  expect_lint("x /  1", one_msg("/"), linter)

  expect_warning(
    {
      linter <- infix_spaces_linter(allow_multiple_spaces = TRUE)
    },
    "allow_multiple_spaces.*deprecated"
  )
  expect_no_lint("x  ~  1", linter)
})

test_that("infix_spaces_linter can require exactly one space", {
  linter <- infix_spaces_linter(default_style = "one")

  expect_lint("x  ~  1", one_msg("~"), linter)
  expect_lint("x  - 1", one_msg("-"), linter)
  expect_lint("x /  1", one_msg("/"), linter)
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
    min_one_msg("+"),
    infix_spaces_linter()
  )
})

test_that("Rules around missing arguments are respected", {
  linter <- infix_spaces_linter()

  expect_no_lint("switch(a = , b = 2)", linter)
  expect_no_lint("alist(missing_arg = )", linter)

  expect_lint("switch(a =, b = 2)", min_one_msg("="), linter)
  expect_lint("alist(missing_arg =)", min_one_msg("="), linter)
})

test_that("native pipe is supported", {
  linter <- infix_spaces_linter()

  expect_no_lint("a |> foo()", linter)
  expect_lint("a|>foo()", min_one_msg("|>"), linter)
})

test_that("mixed unary & binary operators aren't mis-lint", {
  expect_lint(
    "-1-1",
    list(
      message = min_one_msg("-"),
      column_number = 3L
    ),
    infix_spaces_linter()
  )
})

test_that("parse tags are accepted by exclude_operators", { # nofuzz: assignment
  expect_no_lint("sum(x, na.rm=TRUE)", linter_excluding("EQ_SUB"))
  expect_no_lint("function(x, na.rm=TRUE) { }", linter_excluding("EQ_FORMALS"))
  expect_no_lint("x=1", linter_excluding("EQ_ASSIGN"))

  # uses parse_tag
  expect_no_lint("1+1", linter_excluding("'+'"))

  # mixing
  text <- "x=function(a=foo(bar=1)) { }"
  col_assign <- list(column_number = 2L)
  col_formals <- list(column_number = 13L)
  col_sub <- list(column_number = 21L)
  expect_no_lint(text, linter_excluding(c("EQ_SUB", "EQ_FORMALS", "EQ_ASSIGN")))
  expect_lint(text, col_assign, linter_excluding(c("EQ_SUB", "EQ_FORMALS")))
  expect_lint(text, col_formals, linter_excluding(c("EQ_SUB", "EQ_ASSIGN")))
  expect_lint(text, col_sub, linter_excluding(c("EQ_FORMALS", "EQ_ASSIGN")))
  expect_lint(text, list(col_assign, col_formals), linter_excluding("EQ_SUB"))
  expect_lint(text, list(col_assign, col_sub), linter_excluding("EQ_FORMALS"))
  expect_lint(text, list(col_formals, col_sub), linter_excluding("EQ_ASSIGN"))
})

test_that("parse tags are accepted by overrides", { # nofuzz: assignment
  expect_no_lint("sum(x, na.rm=TRUE)", infix_spaces_linter(overrides = list(any = "EQ_SUB")))
  expect_no_lint("function(x, na.rm=TRUE) { }", infix_spaces_linter(overrides = list(any = "EQ_FORMALS")))
  expect_no_lint("x=1", infix_spaces_linter(overrides = list(any = "EQ_ASSIGN")))

  # uses parse_tag
  expect_no_lint("1+1", infix_spaces_linter(overrides = list(any = "'+'")))

  # mixing
  text <- "x=function(a=foo(bar=1)) { }"
  col_assign <- list(column_number = 2L)
  col_formals <- list(column_number = 13L)
  col_sub <- list(column_number = 21L)
  expect_no_lint(text, infix_spaces_linter(overrides = list(any = c("EQ_SUB", "EQ_FORMALS", "EQ_ASSIGN"))))
  expect_lint(text, col_assign, infix_spaces_linter(overrides = list(any = c("EQ_SUB", "EQ_FORMALS"))))
  expect_lint(text, col_formals, infix_spaces_linter(overrides = list(any = c("EQ_SUB", "EQ_ASSIGN"))))
  expect_lint(text, col_sub, infix_spaces_linter(overrides = list(any = c("EQ_FORMALS", "EQ_ASSIGN"))))
  expect_lint(text, list(col_assign, col_formals), infix_spaces_linter(overrides = list(any = "EQ_SUB")))
  expect_lint(text, list(col_assign, col_sub), infix_spaces_linter(overrides = list(any = "EQ_FORMALS")))
  expect_lint(text, list(col_formals, col_sub), infix_spaces_linter(overrides = list(any = "EQ_ASSIGN")))
})

test_that("lints vectorize", { # nofuzz: assignment
  expect_lint(
    trim_some("{
      a<-1
      1/2
      b<-c<-2
      d+e+f+g/3
    }"),
    list(
      list(min_one_msg("<-"), line_number = 2L),
      list(min_one_msg("/"), line_number = 3L),
      list(min_one_msg("<-"), line_number = 4L, column_number = 4L),
      list(min_one_msg("<-"), line_number = 4L, column_number = 7L),
      list(min_one_msg("+"), line_number = 5L, column_number = 4L),
      list(min_one_msg("+"), line_number = 5L, column_number = 6L),
      list(min_one_msg("+"), line_number = 5L, column_number = 8L),
      list(min_one_msg("/"), line_number = 5L, column_number = 10L)
    ),
    infix_spaces_linter()
  )
})
# fuzzer enable: comment_injection
