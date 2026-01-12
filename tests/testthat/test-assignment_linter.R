# nofuzz start
test_that("assignment_linter skips allowed usages", {
  linter <- assignment_linter()

  expect_no_lint("blah", linter)
  expect_no_lint("blah <- 1", linter)
  expect_no_lint("blah<-1", linter)
  expect_no_lint("fun(blah=1)", linter)
})

test_that("assignment_linter blocks disallowed usages", {
  linter <- assignment_linter()
  lint_msg <- rex::rex("Use one of <-, <<- for assignment, not =.")

  expect_lint("blah=1", lint_msg, linter)
  expect_lint("blah = 1", lint_msg, linter)
  expect_lint("blah = fun(1)", lint_msg, linter)
  expect_lint("fun((blah = fun(1)))", lint_msg, linter)

  expect_lint(
    "blah = fun(1) {",
    list(
      lint_msg,
      c(type = "error", "unexpected")
    ),
    linter
  )
})

test_that("arguments handle <<- and ->/->> correctly", {
  linter <- assignment_linter()
  linter_yes_right <- assignment_linter(operator = c("->", "->>"))
  lint_msg_right <- rex::rex("Replace ->> by assigning to a specific environment")

  expect_lint("1 -> blah", rex::rex("Use one of <-, <<- for assignment, not ->."), linter)
  expect_lint("1 ->> blah", lint_msg_right, assignment_linter(operator = "<-"))

  # <<- is only blocked optionally
  expect_no_lint("1 <<- blah", linter)
  expect_lint(
    "1 <<- blah",
    rex::rex("Replace <<- by assigning to a specific environment"),
    assignment_linter(operator = "<-")
  )

  # blocking -> can be disabled
  expect_no_lint("1 -> blah", linter_yes_right)
  expect_no_lint("1 ->> blah", linter_yes_right)
  # we can also differentiate '->' and '->>'
  expect_lint(
    "1 ->> blah",
    lint_msg_right,
    assignment_linter(operator = c("<-", "->"))
  )

  # when user allows _some_ cascading assignment, advice should not mention the
  #   problems with cascading assignment, but focus on the specific disallowed operator.
  expect_lint(
    "1 ->> blah",
    rex::rex("Use one of <-, <<- for assignment, not ->>."),
    assignment_linter(operator = c("<-", "<<-"))
  )
  expect_lint(
    "blah <<- 1",
    rex::rex("Use one of ->, ->> for assignment, not <<-."),
    assignment_linter(operator = c("->", "->>"))
  )
})

test_that("arguments handle trailing assignment operators correctly", { # nofuzz
  linter_default <- assignment_linter()
  linter_no_trailing <- assignment_linter(allow_trailing = FALSE)
  expect_no_lint("x <- y", linter_no_trailing)
  expect_no_lint("foo(bar = 1)", linter_no_trailing)

  expect_lint(
    trim_some("
      foo(bar =
        1)
    "),
    rex::rex("= should not be trailing at the end of a line."),
    linter_no_trailing
  )

  expect_lint(
    trim_some("
      x <<-
        y
    "),
    rex::rex("<<- should not be trailing"),
    linter_no_trailing
  )
  expect_lint(
    trim_some("
      x <<-
        y
    "),
    list(
      rex::rex("Replace <<- by assigning to a specific environment"),
      rex::rex("Assignment <<- should not be trailing")
    ),
    assignment_linter(operator = "<-", allow_trailing = FALSE)
  )

  expect_lint(
    trim_some("
      x <- #Test
        y
    "),
    rex::rex("<- should not be trailing"),
    linter_no_trailing
  )

  pipe_left_string <- trim_some("
    is_long <-
      is %>%
      gather(measure, value, -Species) %>%
      arrange(-value)
  ")
  expect_no_lint(pipe_left_string, linter_default)
  expect_lint(pipe_left_string, rex::rex("<- should not be trailing"), linter_no_trailing)

  pipe_right_string <- trim_some("
    is %>%
      gather(measure, value, -Species) %>%
      arrange(-value) ->
      is_long
  ")
  expect_lint(pipe_right_string, rex::rex("Use one of <-, <<- for assignment, not ->"), linter_default)
  expect_lint(
    pipe_right_string,
    list(
      rex::rex("Use one of <-, <<- for assignment, not ->"),
      rex::rex("Assignment -> should not be trailing")
    ),
    linter_no_trailing
  )
  expect_lint(
    pipe_right_string,
    rex::rex("-> should not be trailing"),
    assignment_linter(operator = "->", allow_trailing = FALSE)
  )

  expect_lint(
    trim_some("
      blah =
        42
      blh2 <-
        54
    "),
    list(
      list(message = "Use one of <-, <<- for assignment, not =.", line_number = 1L, column_number = 6L),
      list(message = "Assignment = should not be trailing at the end of a line", line_number = 1L, column_number = 6L),
      list(message = "Assignment <- should not be trailing at the end of a line", line_number = 3L, column_number = 6L)
    ),
    linter_no_trailing
  )

  expect_lint(
    trim_some("
      a =
        1
    "),
    "= should not be trailing",
    assignment_linter(operator = "=", allow_trailing = FALSE)
  )
})

test_that("allow_trailing interacts correctly with comments in braced expressions", { # nofuzz
  linter <- assignment_linter(allow_trailing = FALSE)
  expect_no_lint(
    trim_some("
    {
      x <- 1
      # blah
      y <- 2
    }
    "),
    linter
  )

  expect_no_lint(
    trim_some("
    {
      x <- '#x'
      y <- '#y'
    }
    "),
    linter
  )

  expect_lint(
    trim_some("
    {
      x <- # blah
        'x'
    }
    "),
    list(message = "<-", line_number = 2L),
    linter
  )

  expect_no_lint(
    trim_some("
    {
      x <- '
        a string
        with an assignment <-
        at the end of the line
      '
    }
    "),
    linter
  )
})

test_that("%<>% throws a lint", {
  expect_lint("x %<>% sum()", "Avoid the assignment pipe %<>%", assignment_linter())
  # regression test for #2850
  expect_lint("a <- 42", "Use %<>% for assignment", assignment_linter(operator = "%<>%"))

  expect_no_lint("x %<>% sum()", assignment_linter(operator = "%<>%"))

  # interaction with allow_trailing
  expect_lint(
    trim_some("
      x %<>%
        sum()
    "),
    list(
      "Avoid the assignment pipe %<>%",
      "Assignment %<>% should not be trailing"
    ),
    assignment_linter(allow_trailing = FALSE)
  )
})

test_that("multiple lints throw correct messages", {
  expect_lint(
    trim_some("{
      x <<- 1
      y ->> 2
      z -> 3
      x %<>% as.character()
    }"),
    list(
      list(message = "Replace <<- by assigning to a specific environment", line_number = 2L),
      list(message = "Replace ->> by assigning to a specific environment", line_number = 3L),
      list(message = "Use <- for assignment, not ->", line_number = 4L),
      list(message = "Avoid the assignment pipe %<>%", line_number = 5L)
    ),
    assignment_linter(operator = "<-")
  )
})

test_that("assignment operator can be toggled", {
  eq_linter <- assignment_linter(operator = "=")
  any_linter <- assignment_linter(operator = "any")
  lint_message <- rex("Use = for assignment, not")

  expect_no_lint("a = 1", eq_linter)
  expect_no_lint("a = 1", any_linter)

  expect_lint("a <- 1", lint_message, eq_linter)
  expect_no_lint("a <- 1", any_linter)

  expect_lint("a = 1; b <- 2", lint_message, eq_linter)
  expect_no_lint("a = 1; b <- 2", any_linter)

  expect_no_lint(
    trim_some("
      foo = function() {
        a = 1
      }
    "),
    eq_linter
  )
  expect_no_lint(
    trim_some("
      foo = function() {
        a = 1
      }
    "),
    any_linter
  )

  expect_lint(
    trim_some("
      foo = function() {
        a <- 1
      }
    "),
    list(lint_message, line_number = 2L),
    eq_linter
  )
  expect_no_lint(
    trim_some("
      foo = function() {
        a <- 1
      }
    "),
    any_linter
  )

  expect_no_lint("if ({a = TRUE}) 1", eq_linter)
  expect_no_lint("if ({a = TRUE}) 1", any_linter)

  expect_no_lint("if (a <- TRUE) 1", eq_linter)
  expect_no_lint("if (a <- TRUE) 1", any_linter)

  expect_no_lint("while ({a = TRUE}) 1", eq_linter)
  expect_no_lint("while ({a = TRUE}) 1", any_linter)

  expect_no_lint("while (a <- TRUE) 1", eq_linter)
  expect_no_lint("while (a <- TRUE) 1", any_linter)

  expect_no_lint("for (ii in {a = TRUE}) 1", eq_linter)
  expect_no_lint("for (ii in {a = TRUE}) 1", any_linter)

  expect_no_lint("for (ii in a <- TRUE) 1", eq_linter)
  expect_no_lint("for (ii in a <- TRUE) 1", any_linter)

  expect_lint(
    trim_some("
      x =
        2
      y <-
        3
    "),
    list(
      list("Assignment = should not be trailing", line_number = 1L),
      list("Assignment <- should not be trailing", line_number = 3L)
    ),
    assignment_linter(operator = "any", allow_trailing = FALSE)
  )
})

test_that("multiple lints throw correct messages when both = and <- are allowed", {
  expect_lint(
    trim_some("{
      x <<- 1
      y ->> 2
      z -> 3
      x %<>% as.character()
      foo <- 1
      bar = 2
    }"),
    list(
      list(message = "Replace <<- by assigning to a specific environment", line_number = 2L),
      list(message = "Replace ->> by assigning to a specific environment", line_number = 3L),
      list(message = "Use one of =, <- for assignment, not ->", line_number = 4L),
      list(message = "Avoid the assignment pipe %<>%", line_number = 5L)
    ),
    assignment_linter(operator = c("=", "<-"))
  )
})

test_that("multiple lints throw correct messages when = is required", {
  expect_lint(
    trim_some("{
      x <<- 1
      y ->> 2
      z -> 3
      x %<>% as.character()
      foo <- 1
      bar = 2
    }"),
    list(
      list(message = "Replace <<- by assigning to a specific environment", line_number = 2L),
      list(message = "Replace ->> by assigning to a specific environment", line_number = 3L),
      list(message = "Use = for assignment, not ->", line_number = 4L),
      list(message = "Avoid the assignment pipe %<>%", line_number = 5L),
      list(message = "Use = for assignment, not <-", line_number = 6L)
    ),
    assignment_linter(operator = "=")
  )
})

test_that("implicit '<-' assignments inside calls are ignored where top-level '<-' is disallowed", {
  linter <- assignment_linter(operator = "=")

  expect_no_lint("if (any(idx <- is.na(y))) which(idx)", linter)
  expect_no_lint("if (any(foo(idx <- is.na(y)))) which(idx)", linter)

  expect_no_lint("while (any(idx <- is.na(y))) break", linter)
  expect_no_lint("while (any(foo(idx <- is.na(y)))) break", linter)

  expect_no_lint("for (i in foo(idx <- is.na(y))) which(idx)", linter)
  expect_no_lint("for (i in foo(bar(idx <- is.na(y)))) which(idx)", linter)
})
# nofuzz end
