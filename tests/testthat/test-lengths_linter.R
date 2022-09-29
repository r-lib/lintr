test_that("lengths_linter skips allowed usages", {
  # TODO(michaelchirico): this probably should be lengths(), but let it pass for now.
  #   Seen twice as a usage like lapply(x, length) > 0 which does as.integer()
  #   conversion and thus is equivalent to sapply(x, length). Check whether
  #   there are any real use cases for lapply(x, length)...
  expect_lint("lapply(x, length)", NULL, lengths_linter())
})

test_that("lengths_linter blocks simple disallowed base usages", {
  expect_lint(
    "sapply(x, length)",
    rex::rex("Use lengths() to find the length of each element in a list."),
    lengths_linter()
  )

  expect_lint(
    "vapply(x, length, integer(1L))",
    rex::rex("Use lengths() to find the length of each element in a list."),
    lengths_linter()
  )
})

test_that("lengths_linter blocks simple disallowed purrr usages", {
  expect_lint(
    "purrr::map_dbl(x, length)",
    rex::rex("Use lengths() to find the length of each element in a list."),
    lengths_linter()
  )

  expect_lint(
    "map_int(x, length)",
    rex::rex("Use lengths() to find the length of each element in a list."),
    lengths_linter()
  )
})
