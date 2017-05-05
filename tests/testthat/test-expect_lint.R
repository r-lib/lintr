context("expect_lint")


# testthat's expect_success() and expect_failure() can only handle the first expectation and are
# thus less than ideal to test expect_lint(), which can process multiple lints. If you want to test
# for failure, always put the lint check or lint field that must fail first.

linter <- assignment_linter
msg <- "Use <-, not ="

test_that("no checks", {
  expect_success(expect_lint("a", NULL, linter))
  expect_success(expect_lint("a=1", NULL, list()))
  expect_failure(expect_lint("a=1", NULL, linter))
})

test_that("single check", {
  expect_failure(expect_lint(character(), msg, linter))
  expect_failure(expect_lint("", msg, linter))

  expect_success(expect_lint(content="a=1", checks=msg, linters=linter))
  expect_success(expect_lint("a=1", msg, linter))
  expect_failure(expect_lint("a=1", "asdf", linter))
  expect_success(expect_lint("a=1", c(message=msg), linter))
  expect_success(expect_lint("a=1", c(message=msg, line_number=1L), linter))
  expect_failure(expect_lint("a=1", c(line_number=2L, message=msg), linter))

  expect_error(expect_lint("a=1", c(message=msg, lineXXX=1L), linter), "invalid field")

  expect_success(expect_lint("a=1", list(message=msg, line_number=1L), linter))
  expect_success(expect_lint("a=1", list(message=msg, ranges=list(c(3L, 3L))), linter))
  expect_failure(expect_lint("a=1", list(2L, msg), linter))
})

test_that("multiple checks", {
  expect_success(expect_lint(file="exclusions-test", checks=as.list(rep(msg, 6L)), linters=linter))

  expect_success(expect_lint("a=1; b=2", list(msg, msg), linter))
  expect_success(expect_lint("a=1; b=2", list(c(message=msg), c(message=msg)), linter))
  expect_success(expect_lint("a=1; b=2", list(c(line_number=1L), c(linter="assignment_linter")), linter))
  expect_success(expect_lint("a=1; b=2", list(msg, c(line="a=1; b=2", type="warning")), linter))
  expect_success(expect_lint(c("a=1", "b=2"), list(c(line_number=1L), c(line_number=2L)), linter))
  expect_failure(expect_lint(c("a=1", "b=2"), list(c(line_number=2L), c(line_number=2L)), linter))

  expect_success(expect_lint("a=1; b=2", list(msg, list(line="a=1; b=2", range=list(c(3L, 3L)))), linter))
  expect_success(expect_lint("a=1; b=2", list(list(line_number=1L), list(line_number=2L)), linter))
  expect_failure(expect_lint("a=1; b=2", list(list(line_number=2L), list(line_number=2L)), linter))
})

