test_that("package_hooks_linter skips allowed usages of packageStartupMessage() & library.dynam()", {
  # allowed in .onAttach, not .onLoad
  expect_lint(
    ".onAttach <- function(lib, pkg) packageStartupMessage('hi')",
    NULL,
    package_hooks_linter()
  )

  # allowed in .onLoad, not .onAttach
  expect_lint(
    ".onLoad <- function(lib, pkg) library.dynam()",
    NULL,
    package_hooks_linter()
  )
})

test_that("package_hooks_linter blocks simple disallowed usages of packageStartupMessage() & library.dynam()", {
  # inline version
  expect_lint(
    ".onLoad <- function(lib, pkg) packageStartupMessage('hi')",
    rex::rex("Put packageStartupMessage() calls in .onAttach()"),
    package_hooks_linter()
  )

  # multiline version
  expect_lint(
    trim_some("
      .onAttach <- function(libname, pkgname) {
        library.dynam()
      }
    "),
    rex::rex("Put library.dynam() calls in .onLoad, not .onAttach()."),
    package_hooks_linter()
  )

  # found at deeper nesting too
  expect_lint(
    trim_some("
      .onLoad <- function(libname, pkgname) {
        foo(bar(baz(packageStartupMessage('hi'))))
      }
    "),
    rex::rex("Put packageStartupMessage() calls in .onAttach()"),
    package_hooks_linter()
  )
})

test_that("package_hooks_linter blocks simple disallowed usages of other blocked messaging functions", {
  # inline version
  expect_lint(
    ".onLoad <- function(lib, pkg) cat('hi')",
    rex::rex("Don't use cat() in .onLoad()"),
    package_hooks_linter()
  )

  # multiline version
  expect_lint(
    trim_some("
      .onAttach <- function(libname, pkgname) {
        writeLines('hi')
      }
    "),
    rex::rex("Don't use writeLines() in .onAttach()"),
    package_hooks_linter()
  )

  expect_lint(
    trim_some("
      .onLoad <- function(libname, pkgname) {
        print('hi')
      }
    "),
    rex::rex("Don't use print() in .onLoad()"),
    package_hooks_linter()
  )

  # found at deeper nesting too
  expect_lint(
    trim_some("
      .onAttach <- function(libname, pkgname) {
        foo(bar(baz(message('hi'))))
      }
    "),
    rex::rex("Don't use message() in .onAttach()"),
    package_hooks_linter()
  )
})

test_that("package_hooks_linter skips valid .onLoad() and .onAttach() arguments", {
  expect_lint(".onAttach <- function(lib, pkg) { }", NULL, package_hooks_linter())
  expect_lint(".onLoad <- function(lib, pkg) { }", NULL, package_hooks_linter())

  # args only need to start with those characters
  expect_lint(".onAttach <- function(libname, pkgpath) { }", NULL, package_hooks_linter())
  expect_lint(".onLoad <- function(libXXXX, pkgYYYY) { }", NULL, package_hooks_linter())
})

test_that("package_hooks_linter blocks invalid .onLoad() / .onAttach() arguments", {
  expect_lint(
    ".onAttach <- function(xxx, pkg) { }",
    rex::rex(".onAttach() and .onLoad() should take two arguments"),
    package_hooks_linter()
  )
  expect_lint(
    ".onLoad <- function(lib, yyy) { }",
    rex::rex(".onAttach() and .onLoad() should take two arguments"),
    package_hooks_linter()
  )
  # only one lint if both are wrong
  expect_lint(
    ".onLoad <- function(xxx, yyy) { }",
    rex::rex(".onAttach() and .onLoad() should take two arguments"),
    package_hooks_linter()
  )

  # exactly two arguments required.
  # NB: QC.R allows ... arguments to be passed, but disallow this flexibility in the linter.
  expect_lint(
    ".onLoad <- function() { }",
    rex::rex(".onAttach() and .onLoad() should take two arguments"),
    package_hooks_linter()
  )
  expect_lint(
    ".onLoad <- function(lib) { }",
    rex::rex(".onAttach() and .onLoad() should take two arguments"),
    package_hooks_linter()
  )
  expect_lint(
    ".onLoad <- function(lib, pkg, third) { }",
    rex::rex(".onAttach() and .onLoad() should take two arguments"),
    package_hooks_linter()
  )
  expect_lint(
    ".onLoad <- function(lib, ...) { }",
    rex::rex(".onAttach() and .onLoad() should take two arguments"),
    package_hooks_linter()
  )
})

test_that("package_hooks_linter skips valid namespace loading", {
  expect_lint(".onAttach <- function(lib, pkg) { requireNamespace('foo') }", NULL, package_hooks_linter())
  expect_lint(".onLoad <- function(lib, pkg) {  requireNamespace('foo') }", NULL, package_hooks_linter())
})

test_that("package_hooks_linter blocks attaching namespaces", {
  expect_lint(
    ".onAttach <- function(lib, pkg) { require(foo) }",
    rex::rex("Don't alter the search() path in .onLoad() or .onAttach()"),
    package_hooks_linter()
  )
  expect_lint(
    ".onLoad <- function(lib, pkg) { library(foo) }",
    rex::rex("Don't alter the search() path in .onLoad() or .onAttach()"),
    package_hooks_linter()
  )
  expect_lint(
    ".onLoad <- function(lib, pkg) { installed.packages() }",
    rex::rex("slow down package load by running installed.packages()."),
    package_hooks_linter()
  )

  # find at further nesting too
  expect_lint(
    ".onAttach <- function(lib, pkg) { a(b(c(require(foo)))) }",
    rex::rex("Don't alter the search() path in .onLoad() or .onAttach()"),
    package_hooks_linter()
  )
  expect_lint(
    ".onLoad <- function(lib, pkg) { d(e(f(library(foo)))) }",
    rex::rex("Don't alter the search() path in .onLoad() or .onAttach()"),
    package_hooks_linter()
  )
  expect_lint(
    ".onLoad <- function(lib, pkg) { g(h(i(installed.packages()))) }",
    rex::rex("slow down package load by running installed.packages()."),
    package_hooks_linter()
  )

  # also find when used as names
  expect_lint(
    ".onAttach <- function(lib, pkg) { sapply(c('a', 'b', 'c'), require, character.only = TRUE) }",
    rex::rex("Don't alter the search() path in .onLoad() or .onAttach()"),
    package_hooks_linter()
  )
  expect_lint(
    ".onAttach <- function(lib, pkg) { lapply(c('a', 'b', 'c'), library, character.only = TRUE) }",
    rex::rex("Don't alter the search() path in .onLoad() or .onAttach()"),
    package_hooks_linter()
  )
})

test_that("package_hooks_linter skips valid .onDetach() and .Last.lib()", {
  expect_lint(".onDetach <- function(lib) { }", NULL, package_hooks_linter())
  expect_lint(".onDetach <- function(libname) { }", NULL, package_hooks_linter())

  expect_lint(".Last.lib <- function(lib) { }", NULL, package_hooks_linter())
  expect_lint(".Last.lib <- function(libname) { }", NULL, package_hooks_linter())
})

test_that("package_hooks_linter catches usage of library.dynam.unload()", {
  expect_lint(
    ".onDetach <- function(lib) { library.dynam.unload() }",
    rex::rex("Use library.dynam() calls in .onUnload, not .onDetach() or .Last.lib()."),
    package_hooks_linter()
  )
  expect_lint(
    ".Last.lib <- function(lib) { library.dynam.unload() }",
    rex::rex("Use library.dynam() calls in .onUnload, not .onDetach() or .Last.lib()."),
    package_hooks_linter()
  )
  # expected usage is in .onUnload
  expect_lint(
    ".onUnload <- function(lib) { library.dynam.unload() }",
    NULL,
    package_hooks_linter()
  )
})

test_that("package_hooks_linter detects bad argument names in .onDetach()/.Last.lib()", {
  expect_lint(
    ".onDetach <- function(xxx) { }",
    rex::rex(".onDetach() and .Last.lib() should take one argument starting with 'lib'."),
    package_hooks_linter()
  )
  expect_lint(
    ".Last.lib <- function(yyy) { }",
    rex::rex(".onDetach() and .Last.lib() should take one argument starting with 'lib'."),
    package_hooks_linter()
  )

  # exactly one argument required.
  # NB: QC.R allows ... arguments to be passed, but disallow this flexibility in the linter.
  expect_lint(
    ".onDetach <- function() { }",
    rex::rex(".onDetach() and .Last.lib() should take one argument starting with 'lib'."),
    package_hooks_linter()
  )
  expect_lint(
    ".Last.lib <- function(lib, pkg) { }",
    rex::rex(".onDetach() and .Last.lib() should take one argument starting with 'lib'."),
    package_hooks_linter()
  )
  expect_lint(
    ".onDetach <- function(...) { }",
    rex::rex(".onDetach() and .Last.lib() should take one argument starting with 'lib'."),
    package_hooks_linter()
  )
})
