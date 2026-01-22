test_that("coalesce_linter skips allowed usage", {
  linter <- coalesce_linter()

  expect_no_lint("if (is.null(x)) y", linter)
  expect_no_lint("if (!is.null(x)) y", linter)
  expect_no_lint("if (!is.null(x)) x", linter)
  expect_no_lint("if (is.null(x)) x", linter)
  expect_no_lint("c(if (!is.null(E)) E)", linter)
  expect_no_lint("if (is.null(x)) y else z", linter)
  expect_no_lint("if (!is.null(x)) x[1] else y", linter)
  expect_no_lint("if (is.null(x[1])) y else x[2]", linter)
  expect_no_lint("if (is.null(x)) y else {x ; z}", linter)
  expect_no_lint("if (is.null(x)) y else {x \n z}", linter)
  expect_no_lint("if (!is.null(x)) {x ; z} else y", linter)
  expect_no_lint("if (!is.null(x)) {x \n z} else y", linter)

  expect_no_lint("if (is.null(s <- foo())) y else x", linter)
  expect_no_lint("if (!is.null(s <- foo())) x else y", linter)
})

test_that("coalesce_linter blocks simple disallowed usage", {
  linter <- coalesce_linter()
  lint_msg <- rex::rex("Use x %||% y instead of if (is.null(x))")
  lint_msg_not <- rex::rex("Use x %||% y instead of if (!is.null(x))")

  expect_lint("if (is.null(x)) y else x", lint_msg, linter)
  expect_lint("if (is.null(x)) { y } else x", lint_msg, linter)
  expect_lint("if (is.null(x)) y else { x }", lint_msg, linter)
  expect_lint("if (is.null(x)) { y } else { x }", lint_msg, linter)

  expect_lint("if (is.null(x[1])) y else x[1]", lint_msg, linter)
  expect_lint("if (is.null(foo(x))) y else foo(x)", lint_msg, linter)

  expect_lint("if (!is.null(x)) x else y", lint_msg_not, linter)
  expect_lint("if (!is.null(x)) { x } else y", lint_msg_not, linter)
  expect_lint("if (!is.null(x)) x else { y }", lint_msg_not, linter)
  expect_lint("if (!is.null(x)) { x } else { y }", lint_msg_not, linter)

  expect_lint("if (!is.null(x[1])) x[1] else y", lint_msg_not, linter)
  expect_lint("if (!is.null(foo(x))) foo(x) else y", lint_msg_not, linter)

  # adversarial comments
  expect_lint(
    trim_some("
      if (!is.null(x[1])) x[ # comment
      1] else y
    "),
    lint_msg_not,
    linter
  )
})

test_that("coalesce_linter blocks usage with implicit assignment", { # nofuzz: assignment
  linter <- coalesce_linter()
  lint_msg <- rex::rex("Use x %||% y instead of if (is.null(x))")
  lint_msg_not <- rex::rex("Use x %||% y instead of if (!is.null(x))")

  expect_lint("if (is.null(s <- foo(x))) y else s", lint_msg, linter)
  expect_lint("if (is.null(s <- foo(x))) { y } else s", lint_msg, linter)
  expect_lint("if (is.null(s <- foo(x))) y else { s }", lint_msg, linter)
  expect_lint("if (is.null(s <- foo(x))) { y } else { s }", lint_msg, linter)

  expect_lint("if (!is.null(s <- foo(x))) s else y", lint_msg_not, linter)
  expect_lint("if (!is.null(s <- foo(x))) { s } else y", lint_msg_not, linter)
  expect_lint("if (!is.null(s <- foo(x))) s else { y }", lint_msg_not, linter)
  expect_lint("if (!is.null(s <- foo(x))) { s } else { y }", lint_msg_not, linter)
})

test_that("lints vectorize", { # nofuzz: assignment
  expect_lint(
    trim_some("{
      if (is.null(x)) y else x
      if (!is.null(a)) a else b
      if (is.null(s <- foo(x))) y else s
      if (!is.null(t <- bar(a))) t else b
    }"),
    list(
      list(rex::rex("if (is.null(x))"), line_number = 2L, column_number = 3L),
      list(rex::rex("if (!is.null(x))"), line_number = 3L, column_number = 3L),
      list(rex::rex("if (is.null(x))"), line_number = 4L, column_number = 3L),
      list(rex::rex("if (!is.null(x))"), line_number = 5L, column_number = 3L)
    ),
    coalesce_linter()
  )
})
