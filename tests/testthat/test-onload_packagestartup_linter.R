test_that("onload_packagestartup_linter skips allowed usages", {
  expect_lint(
    ".onAttach <- function(a, b) packageStartupMessage('hi')",
    NULL,
    onload_packagestartup_linter
  )
})

test_that("onload_packagestartup_linter blocks simple disallowed usages", {
  # inline version
  expect_lint(
    ".onLoad <- function(x, y) packageStartupMessage('hi')",
    rex::rex("Put packageStartupMessage() calls in .onAttach()"),
    onload_packagestartup_linter
  )

  # multiline version
  expect_lint(
    trim_some("
      .onLoad <- function(libname, pkgname) {
        packageStartupMessage('hi')
      }
    "),
    rex::rex("Put packageStartupMessage() calls in .onAttach()"),
    onload_packagestartup_linter
  )

  # found at deeper nesting too
  expect_lint(
    trim_some("
      .onLoad <- function(libname, pkgname) {
        foo(bar(baz(packageStartupMessage('hi'))))
      }
    "),
    rex::rex("Put packageStartupMessage() calls in .onAttach()"),
    onload_packagestartup_linter
  )
})
