test_that("expect_no_condition_linter skips allowed usages", {
  linter <- expect_no_condition_linter()

  expect_no_lint("expect_no_error(foo())", linter)
  expect_no_lint("expect_no_warning(foo())", linter)
  expect_no_lint("expect_no_message(foo())", linter)
  expect_no_lint("expect_no_condition(foo())", linter)

  expect_no_lint("expect_error(foo())", linter)
  expect_no_lint("expect_error(foo(), 'msg')", linter)
  expect_no_lint("expect_error(foo(), regexp = 'msg')", linter)
  expect_no_lint("expect_error(foo(), regexp = NULL)", linter)
  expect_no_lint("expect_error(foo(), class = 'my_error')", linter)
  expect_no_lint("expect_warning(foo(), 'warn')", linter)
  expect_no_lint("expect_message(foo(), 'msg')", linter)
  expect_no_lint("expect_condition(foo(), class = 'my_cond')", linter)

  # NA as the first argument (object), not regexp
  expect_no_lint("expect_error(NA)", linter)
  expect_no_lint("expect_error(NA, 'msg')", linter)

  # Typed NAs are not valid regexp values, _including_, "famously", NA_character_
  expect_no_lint("expect_error(foo(), NA_character_)", linter)
  expect_no_lint("expect_error(foo(), NA_integer_)", linter)
  expect_no_lint("expect_error(foo(), NA_real_)", linter)

  # expect_no_*() doesn't have info= or label= / expected.label= arguments
  expect_no_lint("expect_error(foo(), NA, info = 'foo should not error')", linter)
  expect_no_lint("expect_warning(foo(), NA, label = 'foo')", linter)
  expect_no_lint("expect_message(foo(), regexp = NA, expected.label = 'no message')", linter)
})

test_that("expect_no_condition_linter blocks simple disallowed usages", {
  linter <- expect_no_condition_linter()

  expect_lint(
    "expect_error(foo(), NA)",
    rex::rex("expect_no_error(x) is better than expect_error(x, NA)."),
    linter
  )
  expect_lint(
    "expect_warning(foo(), NA)",
    rex::rex("expect_no_warning(x) is better than expect_warning(x, NA)."),
    linter
  )
  expect_lint(
    "expect_message(foo(), NA)",
    rex::rex("expect_no_message(x) is better than expect_message(x, NA)."),
    linter
  )
  expect_lint(
    "expect_condition(foo(), NA)",
    rex::rex("expect_no_condition(x) is better than expect_condition(x, NA)."),
    linter
  )
})

test_that("expect_no_condition_linter handles named arguments, namespaces, and pipes", {
  linter <- expect_no_condition_linter()

  expect_lint(
    "expect_error(foo(), regexp = NA)",
    rex::rex("expect_no_error(x) is better than expect_error(x, regexp = NA)."),
    linter
  )
  expect_lint(
    "expect_error(regexp = NA, foo())",
    rex::rex("expect_no_error(x) is better than expect_error(x, regexp = NA)."),
    linter
  )
  expect_lint(
    "expect_error(object = foo(), NA)",
    rex::rex("expect_no_error(x) is better than expect_error(x, NA)."),
    linter
  )
  expect_lint(
    "expect_error(NA, object = foo())",
    rex::rex("expect_no_error(x) is better than expect_error(x, NA)."),
    linter
  )
  expect_lint(
    "expect_error(foo(), NA, class = 'my_error')",
    rex::rex("expect_no_error(x) is better than expect_error(x, NA)."),
    linter
  )
  expect_lint(
    "testthat::expect_error(foo(), NA)",
    rex::rex("expect_no_error(x) is better than expect_error(x, NA)."),
    linter
  )
  expect_lint(
    "foo() |> expect_error(NA)",
    rex::rex("expect_no_error(x) is better than expect_error(x, NA)."),
    linter
  )
  expect_lint(
    "foo() %>% expect_warning(NA)",
    rex::rex("expect_no_warning(x) is better than expect_warning(x, NA)."),
    linter
  )
  expect_lint(
    "foo() %>% expect_warning(., NA)",
    rex::rex("expect_no_warning(x) is better than expect_warning(x, NA)."),
    linter
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      expect_error(foo(), NA)
      expect_warning(bar(), regexp = NA)
    }"),
    list(
      list(
        rex::rex("expect_no_error(x) is better than expect_error(x, NA)."),
        line_number = 2L,
        column_number = 3L
      ),
      list(
        rex::rex("expect_no_warning(x) is better than expect_warning(x, regexp = NA)."),
        line_number = 3L,
        column_number = 3L
      )
    ),
    expect_no_condition_linter()
  )
})
