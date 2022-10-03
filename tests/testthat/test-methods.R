test_that("it returns the input if less than the max", {
  expect_equal(trim_output(character()), character())

  expect_equal(trim_output("test", max = 10L), "test")
})

test_that("it returns the input trimmed strictly to max if no lints found", {
  expect_equal(trim_output("testing a longer non_lint string", max = 7L), "testing")
})

test_that("it returns the input trimmed to the last full lint if one exists within the max", {
  t1 <- readChar(test_path("lints"), file.size(test_path("lints")))
  if (.Platform$OS.type == "windows") {
    # Magic numbers expect newlines to be 1 character
    t1 <- gsub("\r\n", "\n", t1, fixed = TRUE)
  }
  expect_equal(trim_output(t1, max = 200L), substr(t1, 1L, 195L))
  expect_equal(trim_output(t1, max = 400L), substr(t1, 1L, 380L))
  expect_equal(trim_output(t1, max = 2000L), substr(t1, 1L, 1930L))
})

test_that("as.data.frame.lints", {
  # A minimum lint
  expect_s3_class(
    l1 <- Lint(
      "dummy.R",
      line_number = 1L,
      type = "style",
      message = "",
      line = ""
    ),
    "lint"
  )

  # A larger lint
  expect_s3_class(
    l2 <- Lint(
      "dummy.R",
      line_number = 2L,
      column_number = 6L,
      type = "error",
      message = "Under no circumstances is the use of foobar allowed.",
      line = "a <- 1",
      ranges = list(c(1L, 2L), c(10L, 20L))
    ),
    "lint"
  )

  expect_warning(
    Lint("dummy.R", linter = "deprecated"),
    regexp = "deprecated",
    fixed = TRUE
  )

  # Convert lints to data.frame
  lints <- structure(list(l1, l2), class = "lints")
  expect_s3_class(
    df <- as.data.frame.lints(lints),
    "data.frame"
  )

  exp <- data.frame(
    filename = rep("dummy.R", 2L),
    line_number = c(1L, 2L),
    column_number = c(1L, 6L),
    type = c("style", "error"),
    message = c("", "Under no circumstances is the use of foobar allowed."),
    line = c("", "a <- 1"),
    linter = c(NA_character_, NA_character_), # These are assigned in lint() now.
    stringsAsFactors = FALSE
  )

  expect_equal(
    df,
    exp
  )
})

test_that("summary.lints() works (no lints)", {
  no_lints <- lint(
    "x <- 1\n",
    linters = assignment_linter()
  )
  no_lint_summary <- summary(no_lints)
  expect_s3_class(no_lint_summary, "data.frame")
  expect_equal(nrow(no_lint_summary), 0L)
})

test_that("summary.lints() works (lints found)", {
  has_lints <- lint(
    "x = 1\n",
    linters = assignment_linter()
  )
  has_lint_summary <- summary(has_lints)
  expect_s3_class(has_lint_summary, "data.frame")
  expect_equal(nrow(has_lint_summary), 1L)
  expect_gt(has_lint_summary$style, 0L)
  expect_equal(has_lint_summary$warning, 0L)
  expect_equal(has_lint_summary$error, 0L)
})

test_that("print.lint works", {
  # don't treat \t as width-1, #528
  l <- Lint(
    filename = "tmp", line_number = 1L, column_number = 3L,
    type = "warning", message = "this is a lint",
    line = c(`1` = "\t\t1:length(x)"), ranges = list(c(3L, 3L))
  )
  expect_output(print(l), "  1:length(x)", fixed = TRUE)
})

test_that("print.lint works for inline data, even in RStudio", {
  l <- lint("x = 1\n")

  # Make sure lints print to console.
  # The full output is:
  #
  # > <text>:1:3: style: Use <-, not =, for assignment.
  # > x = 1
  # >   ^

  withr::with_options(
    list("lintr.rstudio_source_markers" = FALSE),
    expect_output(print(l), "not =")
  )

  mockery::stub(print.lints, "rstudioapi::hasFun", function(...) FALSE)
  withr::with_options(
    list("lintr.rstudio_source_markers" = TRUE),
    expect_output(print(l), "not =")
  )
})

test_that("print.lints works", {
  withr::local_options(lintr.rstudio_source_markers = FALSE)
  tmp <- tempfile()
  file.create(tmp)
  on.exit(unlink(tmp))

  expect_invisible(print(lint(tmp)))
})

test_that("split.lint works as intended", {
  writeLines("1:nrow(x)\n1:ncol(x)", tmp <- tempfile())
  on.exit(unlink(tmp))

  l <- lint(tmp, seq_linter())
  expect_true(all(vapply(split(l), inherits, logical(1L), "lints")))
})
