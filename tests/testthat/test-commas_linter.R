# fuzzer disable: comment_injection
msg_after <- "Put a space after a comma."
msg_before <- "Remove spaces before a comma."

test_that("returns the correct linting (with default parameters)", {
  linter <- commas_linter()

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

  expect_no_lint('"fun(1 ,1)"', linter)
  expect_no_lint("a[1, , 2]", linter)
  expect_no_lint("a[1, , 2, , 3]", linter)

  expect_no_lint("switch(op, x = foo, y = bar)", linter)
  expect_no_lint("switch(op, x = , y = bar)", linter)
  expect_no_lint('switch(op, "x" = , y = bar)', linter)
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
      list(msg_before, column_number = 7L, ranges = list(c(7L, 10L))),
      list(msg_after, column_number = 12L, ranges = list(c(12L, 12L)))
    ),
    linter
  )
})

test_that("returns the correct linting (with 'allow_trailing' set)", {
  linter <- commas_linter(allow_trailing = TRUE)

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

  expect_no_lint('"fun(1 ,1)"', linter)
  expect_no_lint("a[1, , 2]", linter)
  expect_no_lint("a[1, , 2, , 3]", linter)
  expect_no_lint("a[[1,]]", linter)

  expect_no_lint("switch(op, x = foo, y = bar)", linter)
  expect_no_lint("switch(op, x = , y = bar)", linter)
  expect_no_lint('switch(op, "x" = , y = bar)', linter)
  expect_no_lint("switch(op, x = ,\ny = bar)", linter)

  expect_lint("switch(op, x = foo , y = bar)", msg_before, linter)
  expect_lint("switch(op, x = foo , y = bar)", msg_before, linter)
  expect_lint("switch(op , x = foo, y = bar)", msg_before, linter)
  expect_lint("switch(op, x = foo, y = bar(a = 4 , b = 5))", msg_before, linter)
  expect_lint("fun(op, x = foo , y = switch(bar, a = 4, b = 5))", msg_before, linter)

  expect_lint(
    "fun(op    ,bar)",
    list(
      list(msg_before, column_number = 7L, ranges = list(c(7L, 10L))),
      list(msg_after, column_number = 12L, ranges = list(c(12L, 12L)))
    ),
    linter
  )
})

test_that("returns the correct linting (with 'allow_alignment_calls' set)", {
  linter <- commas_linter()

  expect_no_lint(
    trim_some("
      sample_data <- tibble::tribble(
        ~religion  , ~fst_cat , ~snd_cat , ~trd_cat ,
        'Agnostic' ,       27 ,       34 ,       60 ,
        'Atheist'  ,       12 ,       27 ,       37
      )
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      sample_data <- tribble(
        ~a , ~b ,
         1 ,  2
      )
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      out <- fcase(
        x == 1 , 'a' ,
        x == 2 , 'b'
      )
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      out <- data.table::fcase(
        x == 1 , 'a' ,
        x == 2 , 'b'
      )
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      dt <- rowwiseDT(
        id = 1:2,
        x  , y  ,
        'a', 10 ,
        'b', 20
      )
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      dt <- data.table::rowwiseDT(
        id = 1:2,
        x  , y  ,
        'a', 10 ,
        'b', 20
      )
    "),
    linter
  )

  expect_lint("tribble(~a,~b)", msg_after, linter)

  expect_lint(
    trim_some("
      tribble(
        ~a, ~b,
        foo(x , y), 1
      )
    "),
    msg_before,
    linter
  )

  expect_lint("tribble(~a, ~b, 1, 2)[1 , 2]", msg_before, linter)
  expect_lint("tibble::tribble(~a, ~b, 1, 2)[1 , 2]", msg_before, linter)

  strict_linter <- commas_linter(allow_alignment_calls = character())
  expect_lint(
    trim_some("
      tribble(
        ~a , ~b
      )
    "),
    msg_before,
    strict_linter
  )

  custom_linter <- commas_linter(allow_alignment_calls = "my_tribble")
  expect_no_lint("my_tribble(1 , 2)", custom_linter)
  expect_lint("tribble(~a , ~b)", msg_before, custom_linter)
})
# fuzzer enable: comment_injection
