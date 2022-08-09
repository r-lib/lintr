test_that("returns the correct linting", {
  linter <- assignment_linter()
  msg <- rex::rex("Use <-, not =, for assignment.")

  expect_lint("blah", NULL, linter)
  expect_lint("blah <- 1", NULL, linter)
  expect_lint("blah<-1", NULL, linter)
  expect_lint("fun(blah=1)", NULL, linter)

  expect_lint("blah=1", msg, linter)
  expect_lint("blah = 1", msg, linter)
  expect_lint("blah = fun(1)", msg, linter)
  expect_lint("fun((blah = fun(1)))", msg, linter)

  expect_lint(
    "blah = fun(1) {",
    list(
      msg,
      c(type = "error", "unexpected")
    ),
    linter
  )
})

test_that("arguments handle <<- and ->/->> correctly", {
  expect_lint("1 -> blah", rex::rex("Use <-, not ->, for assignment."), assignment_linter())
  expect_lint("1 ->> blah", rex::rex("->> can have hard-to-predict behavior;"), assignment_linter())

  # <<- is only blocked optionally
  expect_lint("1 <<- blah", NULL, assignment_linter())
  expect_lint(
    "1 <<- blah",
    rex::rex("<<- can have hard-to-predict behavior;"),
    assignment_linter(allow_cascading_assign = FALSE)
  )

  # blocking -> can be disabled
  expect_lint("1 -> blah", NULL, assignment_linter(allow_right_assign = TRUE))
  expect_lint("1 ->> blah", NULL, assignment_linter(allow_right_assign = TRUE))
  # blocked under cascading assign but not under right assign --> blocked
  expect_lint(
    "1 ->> blah",
    rex::rex("->> can have hard-to-predict behavior;"),
    assignment_linter(allow_cascading_assign = FALSE, allow_right_assign = TRUE)
  )
})

test_that("arguments handle trailing assignment operators correctly", {
  expect_lint("x <- y", NULL, assignment_linter(allow_trailing = FALSE))
  expect_lint("foo(bar = 1)", NULL, assignment_linter(allow_trailing = FALSE))

  expect_lint(
    "foo(bar =\n1)",
    rex::rex("= should not be trailing"),
    assignment_linter(allow_trailing = FALSE)
  )

  expect_lint(
    "x <<-\ny",
    rex::rex("<<- should not be trailing"),
    assignment_linter(allow_trailing = FALSE)
  )
  expect_lint(
    "x <<-\ny",
    rex::rex("<<- can have hard-to-predict behavior"),
    assignment_linter(allow_trailing = FALSE, allow_cascading_assign = FALSE)
  )

  expect_lint(
    "x <- #Test \ny",
    rex::rex("<- should not be trailing"),
    assignment_linter(allow_trailing = FALSE)
  )

  expect_lint(
    "is_long <-\nis %>%\ngather(measure, value, -Species) %>%\narrange(-value)",
    NULL,
    assignment_linter()
  )
  expect_lint(
    "is_long <-\nis %>%\ngather(measure, value, -Species) %>%\narrange(-value)",
    rex::rex("<- should not be trailing"),
    assignment_linter(allow_trailing = FALSE)
  )

  expect_lint(
    "is %>%\ngather(measure, value, -Species) %>%\narrange(-value) ->\nis_long",
    rex::rex("Use <-, not ->"),
    assignment_linter()
  )
  expect_lint(
    "is %>%\ngather(measure, value, -Species) %>%\narrange(-value) ->\nis_long",
    rex::rex("Use <-, not ->"),
    assignment_linter(allow_trailing = FALSE)
  )
  expect_lint(
    "is %>%\ngather(measure, value, -Species) %>%\narrange(-value) ->\nis_long",
    rex::rex("-> should not be trailing"),
    assignment_linter(allow_right_assign = TRUE, allow_trailing = FALSE)
  )

  expect_lint(
    "\n\nblah=\n42\nblh2<-\n54",
    list(
      list(message = "=", line_number = 3L, column_number = 5L),
      list(message = "<-", line_number = 5L, column_number = 5L)
    ),
    assignment_linter(allow_trailing = FALSE)
  )
})
