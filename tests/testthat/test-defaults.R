context("defaults")

test_that("linters", {
  # non-empty named list of functions
  x <- default_linters
  expect_is(x, "list")
  expect_gt(length(x), 0L)
  expect_true(all(names(x) != ""))
  expect_true(all(vapply(x, inherits, logical(1L), "function")))
})

test_that("undesirable functions and operators", {
  # non-empty named list of NAs and character strings
  vars <- list(all_undesirable_functions, default_undesirable_functions,
               all_undesirable_operators, default_undesirable_operators)

  for (x in vars) {
    expect_is(x, "list")
    expect_gt(length(x), 0L)
    expect_true(all(names(x) != ""))
    expect_true(all(vapply(x, function(x) {is.na(x) || is.character(x)}, logical(1L))))
    expect_true(all(vapply(x, length, integer(1L)) == 1L))
  }
})

test_that("settings", {
  # non-empty named list
  x <- default_settings
  expect_is(x, "list")
  expect_gt(length(x), 0L)
  expect_true(all(names(x) != ""))
})
