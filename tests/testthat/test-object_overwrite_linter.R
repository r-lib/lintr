test_that("object_overwrite_linter skips allowed usages", {
  linter <- object_overwrite_linter()

  expect_lint("function() DT <- data.frame(a = 1)", NULL, linter)

  # don't block names subassigned e.g. as columns or list elements
  expect_lint("function() x$sd <- sd(rnorm(100))", NULL, linter)

  # These virtual names are ignored to slightly reduce the search space
  expect_lint("function() .__C__logical <- TRUE", NULL, linter)
})

test_that("object_overwrite_linter blocks simple disallowed usages", {
  linter <- object_overwrite_linter()

  expect_lint(
    "foo <- function() data <- mtcars",
    rex::rex("'data' is an exported object from package 'utils'."),
    linter
  )

  expect_lint(
    trim_some("
      foo <- function() {
        sigma <- sd(rnorm(100))
      }
    "),
    rex::rex("'sigma' is an exported object from package 'stats'."),
    linter
  )

  # base and graphics both export 'plot' (in recent R); ensure this is no issue
  plot_pkg <- environmentName(environment(plot))
  expect_lint(
    "function() plot <- 1",
    rex::rex("'plot' is an exported object from package '", plot_pkg, "'."),
    linter
  )

  # not just the top level of the function
  expect_lint(
    trim_some("
      foo <- function() {
        if (TRUE) {
          sigma <- sd(rnorm(100))
        } else {
          all <- FALSE
        }
      }
    "),
    list(
      rex::rex("'sigma' is an exported object from package 'stats'."),
      rex::rex("'all' is an exported object from package 'base'.")
    ),
    linter
  )
})

test_that("Non-syntactic names are matched & linted (#2346)", {
  linter <- object_overwrite_linter()
  lint_msg <- rex::rex("'+' is an exported object from package 'base'.")

  expect_lint(
    trim_some("
      foo <- function() {
        `+` <- 2L
      }
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some('
      foo <- function() {
        "+" <- 2L
      }
    '),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function() {
        '+' <- 2L
      }
    "),
    lint_msg,
    linter
  )
})

test_that("object_overwrite_linter skips any name assigned at the top level", {
  linter <- object_overwrite_linter()

  expect_lint("data <- mtcars", NULL, linter)
  expect_lint("sigma <- sd(rnorm(100))", NULL, linter)
})

test_that("object_overwrite_linter skips argument names", {
  linter <- object_overwrite_linter()

  expect_lint("foo <- function(data) data <- data + 1", NULL, linter)

  expect_lint(
    trim_some("
      bar <- function(a, b, c, sigma) {
        sigma <- a * b * c ^ sigma
      }
    "),
    NULL,
    linter
  )
})

test_that("object_overwrite_linter skips data.table assignments with :=", {
  expect_lint("foo <- function() x[, title := 4]", NULL, object_overwrite_linter())
})

test_that("object_overwrite_linter optionally accepts package names", {
  expect_lint("function() data <- 1", NULL, object_overwrite_linter(packages = "base"))

  expect_lint(
    "function() lint <- TRUE",
    rex::rex("'lint' is an exported object from package 'lintr'."),
    object_overwrite_linter(packages = "lintr")
  )
})

test_that("non-<- assignments are detected", {
  linter <- object_overwrite_linter()
  lint_msg <- rex::rex("'sum' is an exported object from package 'base'.")

  expect_lint("function(x) sum(x) -> sum", lint_msg, linter)
  expect_lint("function(x) sum <<- sum(x)", lint_msg, linter)
  expect_lint("function(x) sum(x) ->> sum", lint_msg, linter)
  expect_lint("function(x) sum = sum(x)", lint_msg, linter)
})

test_that("shorthand lambda is detected", {
  skip_if_not_r_version("4.1.0")

  expect_lint("\\() data <- 1", "'data' is an exported object", object_overwrite_linter())
})

test_that("allow_names= works to ignore certain symbols", {
  expect_lint("function() data <- 1", NULL, object_overwrite_linter(allow_names = "data"))
})

test_that("lints vectorize", {
  lines <- trim_some("{
    foo <- function() {
      data <- 1
      var <- 2
    }
    bar <- function(data) {
      data <- data + 3
      sum <- 4
    }
  }")
  expect_lint(
    lines,
    list(
      list(rex::rex("'data' is an exported object from package 'utils'."), line_number = 3L),
      list(rex::rex("'var' is an exported object from package 'stats'."), line_number = 4L),
      list(rex::rex("'sum' is an exported object from package 'base'."), line_number = 8L)
    ),
    object_overwrite_linter()
  )

  expect_lint(
    lines,
    list(rex::rex("'var' is an exported object from package 'stats'."), line_number = 4L),
    object_overwrite_linter(packages = c("stats", "base"), allow_names = "sum")
  )
})

test_that("Validation works", {
  expect_error(object_overwrite_linter(packages = "invalid_"), "required, but not available", fixed = TRUE)
})
