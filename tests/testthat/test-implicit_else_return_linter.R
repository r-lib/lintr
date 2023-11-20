test_that("implicit_else_return_linter skips functions with non-if returns", {
  lines <- c(
    "foo <- function(bar) {",
    "  return(bar)",
    "}"
  )
  expect_lint(lines, NULL, implicit_else_return_linter())
})

test_that("implicit_else_return_linter skips functions with if/else returns", {
  lines <- c(
    "foo <- function(bar) {",
    "  if (TRUE) {",
    "    return(bar)",
    "  } else {",
    "    return(NULL)",
    "  }",
    "}"
  )
  expect_lint(lines, NULL, implicit_else_return_linter())
})

test_that("implicit_else_return_linter skips if statements outside of functions", {
  expect_lint("if(TRUE) TRUE", NULL, implicit_else_return_linter())
})

test_that("implicit_else_return_linter identifies a simple implicit else", {
  lines <- c(
    "foo <- function(bar) {",
    "  if (TRUE) {",
    "    return(bar)",
    "  }",
    "}"
  )
  expect_lint(
    lines,
    rex::rex("All functions with terminal if statements must"),
    implicit_else_return_linter()
  )
})

test_that("implicit_else_return_linter finds implicit else with nested if", {
  lines <- c(
    "foo <- function() {",
    "  if (TRUE) {",
    "    if (TRUE) {",
    "      return(FALSE)",
    "    } else {",
    "      return(TRUE)",
    "    }",
    "  }",
    "}"
  )
  expect_lint(
    lines,
    rex::rex("All functions with terminal if statements must"),
    implicit_else_return_linter()
  )
})

test_that("implicit_else_return_linter works on anonymous/inline functions", {
  expect_lint(
    "lapply(rnorm(10), function(x) if (TRUE) x + 1)",
    rex::rex("All functions with terminal if statements must"),
    implicit_else_return_linter()
  )
})

test_that("implicit_else_return_linter skips side-effect functions like .onLoad", {
  lines <- c(
    ".onAttach <- function(libname, pkgname) {",
    "  if (TRUE) nativesupport::LoadNativeExtension()",
    "}"
  )
  expect_lint(lines, NULL, implicit_else_return_linter())
})
