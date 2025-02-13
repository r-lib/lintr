test_that("assignment_linter skips allowed usages", {
  linter <- assignment_linter()

  expect_lint("blah", NULL, linter)
  expect_lint("blah <- 1", NULL, linter)
  expect_lint("blah<-1", NULL, linter)
  expect_lint("fun(blah=1)", NULL, linter)
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
  expect_lint("1 <<- blah", NULL, linter)
  expect_lint(
    "1 <<- blah",
    rex::rex("Replace <<- by assigning to a specific environment"),
    assignment_linter(operator = "<-")
  )

  # blocking -> can be disabled
  expect_lint("1 -> blah", NULL, linter_yes_right)
  expect_lint("1 ->> blah", NULL, linter_yes_right)
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

test_that("arguments handle trailing assignment operators correctly", {
  linter_default <- assignment_linter()
  linter_no_trailing <- assignment_linter(allow_trailing = FALSE)
  expect_lint("x <- y", NULL, linter_no_trailing)
  expect_lint("foo(bar = 1)", NULL, linter_no_trailing)

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
  expect_lint(pipe_left_string, NULL, linter_default)
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

test_that("allow_trailing interacts correctly with comments in braced expressions", {
  linter <- assignment_linter(allow_trailing = FALSE)
  expect_lint(
    trim_some("
    {
      x <- 1
      # blah
      y <- 2
    }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
    {
      x <- '#x'
      y <- '#y'
    }
    "),
    NULL,
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

  expect_lint(
    trim_some("
    {
      x <- '
        a string
        with an assignment <-
        at the end of the line
      '
    }
    "),
    NULL,
    linter
  )
})

test_that("%<>% throws a lint", {
  expect_lint("x %<>% sum()", "Avoid the assignment pipe %<>%", assignment_linter())
  expect_lint("x %<>% sum()", NULL, assignment_linter(operator = "%<>%"))

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

  expect_lint("a = 1", NULL, eq_linter)
  expect_lint("a = 1", NULL, any_linter)

  expect_lint("a <- 1", lint_message, eq_linter)
  expect_lint("a <- 1", NULL, any_linter)

  expect_lint("a = 1; b <- 2", lint_message, eq_linter)
  expect_lint("a = 1; b <- 2", NULL, any_linter)

  expect_lint(
    trim_some("
      foo = function() {
        a = 1
      }
    "),
    NULL,
    eq_linter
  )
  expect_lint(
    trim_some("
      foo = function() {
        a = 1
      }
    "),
    NULL,
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
  expect_lint(
    trim_some("
      foo = function() {
        a <- 1
      }
    "),
    NULL,
    any_linter
  )

  expect_lint("if ({a = TRUE}) 1", NULL, eq_linter)
  expect_lint("if ({a = TRUE}) 1", NULL, any_linter)

  expect_lint("if (a <- TRUE) 1", NULL, eq_linter)
  expect_lint("if (a <- TRUE) 1", NULL, any_linter)

  expect_lint("for (ii in {a = TRUE}) 1", NULL, eq_linter)
  expect_lint("for (ii in {a = TRUE}) 1", NULL, any_linter)

  expect_lint("for (ii in a <- TRUE) 1", NULL, eq_linter)
  expect_lint("for (ii in a <- TRUE) 1", NULL, any_linter)

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

test_that("Deprecated arguments error as intended", {
  expect_error(regexp = "allow_cascading_assign", assignment_linter(allow_cascading_assign = FALSE))
  expect_error(regexp = "allow_right_assign", assignment_linter(allow_right_assign = TRUE))
  expect_error(regexp = "allow_pipe_assign", assignment_linter(allow_pipe_assign = TRUE))
})
