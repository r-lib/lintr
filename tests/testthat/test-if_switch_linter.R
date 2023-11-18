test_that("if_switch_linter skips allowed usages", {
  linter <- if_switch_linter()

  # don't apply to simple if/else statements
  expect_lint("if (x == 'a') 1 else 2", NULL, linter)
  # don't apply to non-character conditions
  #   (NB: switch _could_ be used for integral input, but this
  #    interface is IMO a bit clunky / opaque)
  expect_lint("if (x == 1) 1 else 2", NULL, linter)
  # this also has a switch equivalent, but we don't both handling such
  #   complicated cases
  expect_lint("if (x == 'a') 1 else if (x != 'b') 2 else 3", NULL, linter)
  # multiple variables involved --> no clean change
  expect_lint("if (x == 'a') 1 else if (y == 'b') 2 else 3", NULL, linter)
  # multiple conditions --> no clean change
  expect_lint("if (is.character(x) && x == 'a') 1 else if (x == 'b') 2 else 3", NULL, linter)
  # simple cases with two conditions might be more natural
  #   without switch(); require at least three branches to trigger a lint
  expect_lint("if (x == 'a') 1 else if (x == 'b') 2", NULL, linter)
  # still no third if() clause
  expect_lint("if (x == 'a') 1 else if (x == 'b') 2 else 3", NULL, linter)
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
  expect_lint(
    "if (x %in% c('a', 'e', 'f')) 1 else if (x == 'b') 2 else if (x == 'c') 3 else if (x == 'd') 4",
    NULL,
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

# TODO(michaelchirico): be more explicit/deliberate about nested `{}` cases like
#   if (x == 'a') 1 else { if (x == 'b') 2 else 3 }
