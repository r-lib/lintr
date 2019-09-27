context("trim_output")
test_that("it returns the input if less than the max", {
  expect_equal(trim_output(character(0)), character(0))

  expect_equal(trim_output("test", max = 10), "test")
})

test_that("it returns the input trimmed strictly to max if no lints found", {
  expect_equal(trim_output("testing a longer non_lint string", max = 7), "testing")
})

test_that("it returns the input trimmed to the last full lint if one exists within the max", {
  t1 <- readChar(test_path("lints"), file.size(test_path("lints")))
  expect_equal(trim_output(t1, max = 200), substr(t1, 1, 195))

  expect_equal(trim_output(t1, max = 400), substr(t1, 1, 380))

  expect_equal(trim_output(t1, max = 2000), substr(t1, 1, 1930))
})

test_that("as.data.frame.lints", {
  # A minimum lint
  expect_is(
    l1 <- Lint("dummy.R",
         line_number = 1L,
         type = "style",
         message = "",
         line = ""),
    "lint"
  )

  # A larger lint
  expect_is(
    l2 <- Lint("dummy.R",
              line_number = 2L,
              column_number = 6L,
              type = "error",
              message = "Under no circumstances is the use of foobar allowed.",
              line = "a <- 1",
              ranges = list(c(1, 2), c(10, 20)),
              linter = "custom_linter"),
    "lint"
  )

  # Convert lints to data.frame
  lints <- structure(list(l1, l2), class = "lints")
  expect_is(
    df <- as.data.frame.lints(lints),
    "data.frame"
  )

  exp <- data.frame(
    filename = rep("dummy.R", 2),
    line_number = c(1, 2),
    column_number = c(1, 6),
    type = c("style", "error"),
    message = c("", "Under no circumstances is the use of foobar allowed."),
    line = c("", "a <- 1"),
    linter = c("", "custom_linter"),
    stringsAsFactors = FALSE)

  expect_equal(
    df,
    exp
  )
})

test_that("summary.lints() works (no lints)", {
  no_lints <- lint(
    "x <- 1\n",
    linters = assignment_linter)
  no_lint_summary <- summary(no_lints)
  expect_true(is.data.frame(no_lint_summary))
  expect_equal(nrow(no_lint_summary), 0)
})

test_that("summary.lints() works (lints found)", {
  has_lints <- lint(
    "x = 1\n",
    linters = assignment_linter)
  has_lint_summary <- summary(has_lints)
  expect_true(is.data.frame(has_lint_summary))
  expect_equal(nrow(has_lint_summary), 1)
  expect_true(has_lint_summary$style > 0)
  expect_equal(has_lint_summary$warning, 0)
  expect_equal(has_lint_summary$error, 0)
})

