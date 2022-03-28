test_that("with_defaults works as expected with unnamed args", {
  # assignment_linter is in defaults, so output doesn't change
  expect_named(with_defaults(assignment_linter), names(with_defaults()))
})

test_that("all default linters are tagged default", {
  # TODO(michaelchirico): use plain expect_equal after waldo#133 makes it into a CRAN release
  # Here, the environment()s are different because factories use them.
  expect_true(all.equal(with_tags(), with_defaults()))

  # Check that above test also trips on default arguments.
  expect_equal(
    all.equal(with_tags(), with_defaults(line_length_linter(120))),
    'Component "line_length_linter": Component "length": Mean relative difference: 0.5'
  )
})

test_that("can instantiate all linters without arguments", {
  all_linters <- with_tags(tags = NULL)

  expect_type(all_linters, "list")
  expect_length(all_linters, nrow(available_linters()))
})
