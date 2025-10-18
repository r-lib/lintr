test_that("if_switch_linter skips allowed usages", {
  linter <- if_switch_linter()

  # don't apply to simple if/else statements
  expect_no_lint("if (x == 'a') 1 else 2", linter)
  # don't apply to non-character conditions
  #   (NB: switch _could_ be used for integral input, but this
  #    interface is IMO a bit clunky / opaque)
  expect_no_lint("if (x == 1) 1 else 2", linter)
  # this also has a switch equivalent, but we don't both handling such
  #   complicated cases
  expect_no_lint("if (x == 'a') 1 else if (x != 'b') 2 else 3", linter)
  # multiple variables involved --> no clean change
  expect_no_lint("if (x == 'a') 1 else if (y == 'b') 2 else 3", linter)
  # multiple conditions --> no clean change
  expect_no_lint("if (is.character(x) && x == 'a') 1 else if (x == 'b') 2 else 3", linter)
  # simple cases with two conditions might be more natural
  #   without switch(); require at least three branches to trigger a lint
  expect_no_lint("if (x == 'a') 1 else if (x == 'b') 2", linter)
  # still no third if() clause
  expect_no_lint("if (x == 'a') 1 else if (x == 'b') 2 else 3", linter)
})

test_that("if_switch_linter blocks simple disallowed usages", {
  linter <- if_switch_linter()
  lint_msg <- rex::rex("Prefer switch() statements over repeated if/else equality tests")

  # anything with >= 2 equality statements is deemed switch()-worthy
  expect_lint("if (x == 'a') 1 else if (x == 'b') 2 else if (x == 'c') 3", lint_msg, linter)
  # expressions are also OK
  expect_lint("if (foo(x) == 'a') 1 else if (foo(x) == 'b') 2 else if (foo(x) == 'c') 3", lint_msg, linter)
})

test_that("if_switch_linter handles further nested if/else correctly", {
  linter <- if_switch_linter()

  # ensure that nested if() doesn't generate multiple lints;
  expect_lint(
    "if (x == 'a') 1 else if (x == 'b') 2 else if (x == 'c') 3 else if (x == 'd') 4",
    rex::rex("Prefer switch() statements over repeated if/else equality tests"),
    linter
  )
  # related to previous test -- if the first condition is non-`==`, the
  #   whole if/else chain is "tainted" / non-switch()-recommended.
  #   (technically, switch can work here, but the semantics are opaque)
  expect_no_lint(
    "if (x %in% c('a', 'e', 'f')) 1 else if (x == 'b') 2 else if (x == 'c') 3 else if (x == 'd') 4",
    linter
  )
})

test_that("multiple lints have right metadata", {
  lint_msg <- rex::rex("Prefer switch() statements over repeated if/else equality tests")

  expect_lint(
    trim_some("{
      if (x == 'a') {
        do_a()
      } else if (x == 'b') {
        do_b()
      } else if (x == 'c') {
        do_c()
      }
      if (y == 'A') {
        do_A()
      } else if (y == 'B') {
        do_B()
      } else if (y == 'C') {
        do_C()
      }
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 9L)
    ),
    if_switch_linter()
  )
})

test_that("max_branch_lines= and max_branch_expressions= arguments work", {
  max_lines2_linter <- if_switch_linter(max_branch_lines = 2L)
  max_lines4_linter <- if_switch_linter(max_branch_lines = 4L)
  max_expr2_linter <- if_switch_linter(max_branch_expressions = 2L)
  max_expr4_linter <- if_switch_linter(max_branch_expressions = 4L)
  lint_msg <- rex::rex("Prefer switch() statements over repeated if/else equality tests")

  one_per_branch_lines <- trim_some("
    if (x == 'a') {
      1
    } else if (x == 'b') {
      2
    } else if (x == 'c') {
      3
    }
  ")
  expect_lint(one_per_branch_lines, lint_msg, max_lines2_linter)
  expect_lint(one_per_branch_lines, lint_msg, max_lines4_linter)
  expect_lint(one_per_branch_lines, lint_msg, max_expr2_linter)
  expect_lint(one_per_branch_lines, lint_msg, max_expr4_linter)

  two_per_branch_lines <- trim_some("
    if (x == 'a') {
      1
      2
    } else if (x == 'b') {
      3
      4
    } else if (x == 'c') {
      5
      6
    }
  ")
  expect_lint(two_per_branch_lines, lint_msg, max_lines2_linter)
  expect_lint(two_per_branch_lines, lint_msg, max_lines4_linter)
  expect_lint(two_per_branch_lines, lint_msg, max_expr2_linter)
  expect_lint(two_per_branch_lines, lint_msg, max_expr4_linter)

  three_per_branch_lines <- trim_some("
    if (x == 'a') {
      1
      2
      3
    } else if (x == 'b') {
      4
      5
      6
    } else if (x == 'c') {
      7
      8
      9
    }
  ")
  expect_no_lint(three_per_branch_lines, max_lines2_linter)
  expect_lint(three_per_branch_lines, lint_msg, max_lines4_linter)
  expect_no_lint(three_per_branch_lines, max_expr2_linter)
  expect_lint(three_per_branch_lines, lint_msg, max_expr4_linter)

  five_per_branch_lines <- trim_some("
    if (x == 'a') {
      1
      2
      3
      4
      5
    } else if (x == 'b') {
      6
      7
      8
      9
      10
    } else if (x == 'c') {
      11
      12
      13
      14
      15
    }
  ")
  expect_no_lint(five_per_branch_lines, max_lines2_linter)
  expect_no_lint(five_per_branch_lines, max_lines4_linter)
  expect_no_lint(five_per_branch_lines, max_expr2_linter)
  expect_no_lint(five_per_branch_lines, max_expr4_linter)

  five_lines_three_expr_lines <- trim_some("
    if (x == 'a') {
      1
      2
      foo(
        x
      )
    } else if (x == 'b') {
      6
      7
      bar(
        y
      )
    } else if (x == 'c') {
      11
      12
      baz(
        z
      )
    }
  ")
  expect_no_lint(five_lines_three_expr_lines, max_lines2_linter)
  expect_no_lint(five_lines_three_expr_lines, max_lines4_linter)
  expect_no_lint(five_lines_three_expr_lines, max_expr2_linter)
  expect_lint(
    five_lines_three_expr_lines,
    list(lint_msg, line_number = 1L),
    max_expr4_linter
  )

  five_expr_three_lines_lines <- trim_some("
    if (x == 'a') {
      1
      2
      3; 4; 5
    } else if (x == 'b') {
      6
      7
      8; 9; 10
    } else if (x == 'c') {
      11
      12
      13; 14; 15
    }
  ")
  expect_no_lint(five_expr_three_lines_lines, max_lines2_linter)
  expect_lint(
    five_expr_three_lines_lines,
    list(lint_msg, line_number = 1L),
    max_lines4_linter
  )
  expect_no_lint(five_expr_three_lines_lines, max_expr2_linter)
  expect_no_lint(five_expr_three_lines_lines, max_expr4_linter)
})

test_that("max_branch_lines= and max_branch_expressions= block over-complex switch() too", {
  max_lines2_linter <- if_switch_linter(max_branch_lines = 2L)
  max_lines4_linter <- if_switch_linter(max_branch_lines = 4L)
  max_expr2_linter <- if_switch_linter(max_branch_expressions = 2L)
  max_expr4_linter <- if_switch_linter(max_branch_expressions = 4L)
  lint_msg <- rex::rex("Prefer repeated if/else statements over overly-complicated switch() statements.")

  one_per_branch_lines <- trim_some("
    switch(x,
      a = {
        1
      },
      b = {
        2
      },
      c = {
        3
      }
    )
  ")
  expect_no_lint(one_per_branch_lines, max_lines2_linter)
  expect_no_lint(one_per_branch_lines, max_lines4_linter)
  expect_no_lint(one_per_branch_lines, max_expr2_linter)
  expect_no_lint(one_per_branch_lines, max_expr4_linter)

  two_per_branch_lines <- trim_some("
    switch(x,
      a = {
        1
        2
      },
      b = {
        3
        4
      },
      c = {
        5
        6
      }
    )
  ")
  expect_no_lint(two_per_branch_lines, max_lines2_linter)
  expect_no_lint(two_per_branch_lines, max_lines4_linter)
  expect_no_lint(two_per_branch_lines, max_expr2_linter)
  expect_no_lint(two_per_branch_lines, max_expr4_linter)

  three_per_branch_lines <- trim_some("
    switch(x,
      a = {
        1
        2
        3
      },
      b = {
        4
        5
        6
      },
      c = {
        7
        8
        9
      }
    )
  ")
  expect_lint(
    three_per_branch_lines,
    list(lint_msg, line_number = 1L),
    max_lines2_linter
  )
  expect_no_lint(three_per_branch_lines, max_lines4_linter)
  expect_lint(
    three_per_branch_lines,
    list(lint_msg, line_number = 1L),
    max_expr2_linter
  )
  expect_no_lint(three_per_branch_lines, max_expr4_linter)

  five_per_branch_lines <- trim_some("
    switch(x,
      a = {
        1
        2
        3
        4
        5
      },
      b = {
        6
        7
        8
        9
        10
      },
      c = {
        11
        12
        13
        14
        15
      }
    )
  ")
  expect_lint(five_per_branch_lines, lint_msg, max_lines2_linter)
  expect_lint(five_per_branch_lines, lint_msg, max_lines4_linter)
  expect_lint(five_per_branch_lines, lint_msg, max_expr2_linter)
  expect_lint(five_per_branch_lines, lint_msg, max_expr4_linter)

  five_lines_three_expr_lines <- trim_some("
    switch(x,
      a = {
        1
        2
        foo(
          x
        )
      },
      b = {
        6
        7
        bar(
          y
        )
      },
      c = {
        11
        12
        baz(
          z
        )
      }
    )
  ")
  expect_lint(five_lines_three_expr_lines, lint_msg, max_lines2_linter)
  expect_lint(five_lines_three_expr_lines, lint_msg, max_lines4_linter)
  expect_lint(five_lines_three_expr_lines, lint_msg, max_expr2_linter)
  expect_no_lint(five_lines_three_expr_lines, max_expr4_linter)

  five_expr_three_lines_lines <- trim_some("
    switch(x,
      a = {
        1
        2
        3; 4; 5
      },
      b = {
        6
        7
        8; 9; 10
      },
      c = {
        11
        12
        13; 14; 15
      }
    )
  ")
  expect_lint(five_expr_three_lines_lines, lint_msg, max_lines2_linter)
  expect_no_lint(five_expr_three_lines_lines, max_lines4_linter)
  expect_lint(five_expr_three_lines_lines, lint_msg, max_expr2_linter)
  expect_lint(five_expr_three_lines_lines, lint_msg, max_expr4_linter)
})

test_that("max_branch_lines= and max_branch_expressions= interact correctly", {
  linter <- if_switch_linter(max_branch_lines = 5L, max_branch_expressions = 3L)
  lint_msg <- rex::rex("Prefer switch() statements over repeated if/else equality tests")

  expect_lint(
    trim_some("
      if (x == 'a') {
        1
      } else if (x == 'b') {
        2
      } else if (x == 'c') {
        3
      }
    "),
    lint_msg,
    linter
  )

  expect_no_lint(trim_some("
      if (x == 'a') {
        foo(
          x1,
          x2,
          x3,
          x4
        )
      } else if (x == 'b') {
        2
      } else if (x == 'c') {
        3
      }
    "), linter)

  expect_no_lint(trim_some("
      if (x == 'a') {
        1; 2; 3; 4
      } else if (x == 'b') {
        5
      } else if (x == 'c') {
        6
      }
    "), linter)
})

test_that("max_branch_lines= and max_branch_expressions= work for a terminal 'else' branch", {
  max_lines2_linter <- if_switch_linter(max_branch_lines = 2L)
  max_expr2_linter <- if_switch_linter(max_branch_expressions = 2L)
  lint_msg <- rex::rex("Prefer repeated if/else statements over overly-complicated switch() statements.")

  else_long_lines <- trim_some("
    if (x == 'a') {
      1
    } else if (x == 'b') {
      2
    } else if (x == 'c') {
      3
    } else {
      4
      5
      6
    }
  ")
  expect_no_lint(else_long_lines, max_lines2_linter)
  expect_no_lint(else_long_lines, max_expr2_linter)

  default_long_lines <- trim_some("
    switch(x,
      a = {
        1
      },
      b = {
        2
      },
      c = {
        3
      },
      {
        4
        5
        6
      }
    )
  ")
  expect_lint(default_long_lines, lint_msg, max_lines2_linter)
  expect_lint(default_long_lines, lint_msg, max_expr2_linter)
})

test_that("max_branch_lines= and max_branch_expressions= are guided by the most complex branch", {
  max_lines2_linter <- if_switch_linter(max_branch_lines = 2L)
  max_expr2_linter <- if_switch_linter(max_branch_expressions = 2L)
  lint_msg <- rex::rex("Prefer repeated if/else statements over overly-complicated switch() statements.")

  # no lint if _any_ branch is too complex
  if_else_one_branch_lines <- trim_some("
    if (x == 'a') {
      1
    } else if (x == 'b') {
      2
    } else if (x == 'c') {
      3
      4
      5
    }
  ")
  expect_no_lint(if_else_one_branch_lines, max_lines2_linter)
  expect_no_lint(if_else_one_branch_lines, max_expr2_linter)

  # lint if _any_ branch is too complex
  switch_one_branch_lines <- trim_some("
    switch(x,
      a = {
        1
      },
      b = {
        2
      },
      c = {
        3
        4
        5
      }
    )
  ")
  expect_lint(switch_one_branch_lines, lint_msg, max_lines2_linter)
  expect_lint(switch_one_branch_lines, lint_msg, max_expr2_linter)
})
