test_that("returns the correct linting (with default parameters)", {
  linter <- commas_linter()
  msg_after <- rex::rex("Put a space after a comma.")
  msg_before <- rex::rex("Remove spaces before a comma.")

  expect_no_lint("blah", linter)
  expect_no_lint("fun(1, 1)", linter)
  expect_no_lint("fun(1,\n  1)", linter)
  expect_no_lint("fun(1,\n1)", linter)
  expect_no_lint("fun(1\n,\n1)", linter)
  expect_no_lint("fun(1\n  ,\n1)", linter)

  expect_lint("fun(1\n,1)", msg_after, linter)
  expect_lint("fun(1,1)", msg_after, linter)
  expect_lint("\nfun(1,1)", msg_after, linter)
  expect_lint("a(1,)", msg_after, linter)
  expect_lint("a[1,]", msg_after, linter)
  expect_lint("a[[1,]]", msg_after, linter)
  expect_lint(
    "fun(1 ,1)",
    list(
      list(msg_before, column_number = 6L),
      list(msg_after, column_number = 8L)
    ),
    linter
  )

  expect_no_lint("\"fun(1 ,1)\"", linter)
  expect_no_lint("a[1, , 2]", linter)
  expect_no_lint("a[1, , 2, , 3]", linter)

  expect_no_lint("switch(op, x = foo, y = bar)", linter)
  expect_no_lint("switch(op, x = , y = bar)", linter)
  expect_no_lint("switch(op, \"x\" = , y = bar)", linter)
  expect_no_lint("switch(op, x = ,\ny = bar)", linter)

  expect_lint("switch(op, x = foo , y = bar)", msg_before, linter)
  expect_lint("switch(op, x = foo , y = bar)", msg_before, linter)
  expect_lint("switch(op , x = foo, y = bar)", msg_before, linter)
  expect_lint("switch(op, x = foo, y = bar(a = 4 , b = 5))", msg_before, linter)
  expect_lint("fun(op, x = foo , y = switch(bar, a = 4, b = 5))", msg_before, linter)
  expect_lint(
    trim_some("
      switch(op ,
        x = foo,y = bar
      )
    "),
    list(
      list(msg_before, line_number = 1L),
      list(msg_after, line_number = 2L)
    ),
    linter
  )

  expect_lint(
    "fun(op    ,bar)",
    list(
      list(message = msg_before, column_number = 7L, ranges = list(c(7L, 10L))),
      list(message = msg_after, column_number = 12L, ranges = list(c(12L, 12L)))
    ),
    linter
  )
})

test_that("returns the correct linting (with 'allow_trailing' set)", {
  linter <- commas_linter(allow_trailing = TRUE)
  msg_after <- rex::rex("Put a space after a comma.")
  msg_before <- rex::rex("Remove spaces before a comma.")

  expect_no_lint("blah", linter)
  expect_no_lint("fun(1, 1)", linter)
  expect_no_lint("fun(1,\n  1)", linter)
  expect_no_lint("fun(1,\n1)", linter)
  expect_no_lint("fun(1\n,\n1)", linter)
  expect_no_lint("fun(1\n  ,\n1)", linter)
  expect_no_lint("a[1,]", linter)
  expect_no_lint("a(1,)", linter)

  expect_lint("fun(1\n,1)", msg_after, linter)
  expect_lint("fun(1,1)", msg_after, linter)
  expect_lint("\nfun(1,1)", msg_after, linter)
  expect_lint(
    "fun(1 ,1)",
    list(
      msg_before,
      msg_after
    ),
    linter
  )

  expect_no_lint("\"fun(1 ,1)\"", linter)
  expect_no_lint("a[1, , 2]", linter)
  expect_no_lint("a[1, , 2, , 3]", linter)
  expect_no_lint("a[[1,]]", linter)

  expect_no_lint("switch(op, x = foo, y = bar)", linter)
  expect_no_lint("switch(op, x = , y = bar)", linter)
  expect_no_lint("switch(op, \"x\" = , y = bar)", linter)
  expect_no_lint("switch(op, x = ,\ny = bar)", linter)

  expect_lint("switch(op, x = foo , y = bar)", msg_before, linter)
  expect_lint("switch(op, x = foo , y = bar)", msg_before, linter)
  expect_lint("switch(op , x = foo, y = bar)", msg_before, linter)
  expect_lint("switch(op, x = foo, y = bar(a = 4 , b = 5))", msg_before, linter)
  expect_lint("fun(op, x = foo , y = switch(bar, a = 4, b = 5))", msg_before, linter)

  expect_lint(
    "fun(op    ,bar)",
    list(
      list(message = msg_before, column_number = 7L, ranges = list(c(7L, 10L))),
      list(message = msg_after, column_number = 12L, ranges = list(c(12L, 12L)))
    ),
    linter
  )
})
