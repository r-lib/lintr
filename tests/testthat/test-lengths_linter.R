test_that("lengths_linter skips allowed usages", {
  # TODO(#1570): also throw a lint here, and for map(x, length)
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
