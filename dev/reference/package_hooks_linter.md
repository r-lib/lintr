# Package hooks linter

Check various common "gotchas" in
[`.onLoad()`](https://rdrr.io/r/base/ns-hooks.html),
[`.onAttach()`](https://rdrr.io/r/base/ns-hooks.html),
[`.Last.lib()`](https://rdrr.io/r/base/ns-hooks.html),
[`.onDetach()`](https://rdrr.io/r/base/ns-hooks.html), and
[`.onUnload()`](https://rdrr.io/r/base/ns-hooks.html) namespace hooks
that will cause `R CMD check` issues. See Writing R Extensions for
details.

## Usage

``` r
package_hooks_linter()
```

## Details

1.  `.onLoad()` shouldn't call
    [`cat()`](https://rdrr.io/r/base/cat.html),
    [`message()`](https://rdrr.io/r/base/message.html),
    [`print()`](https://rdrr.io/r/base/print.html),
    [`writeLines()`](https://rdrr.io/r/base/writeLines.html),
    [`packageStartupMessage()`](https://rdrr.io/r/base/message.html),
    [`require()`](https://rdrr.io/r/base/library.html),
    [`library()`](https://rdrr.io/r/base/library.html), or
    [`installed.packages()`](https://rdrr.io/r/utils/installed.packages.html).

2.  `.onAttach()` shouldn't call
    [`cat()`](https://rdrr.io/r/base/cat.html),
    [`message()`](https://rdrr.io/r/base/message.html),
    [`print()`](https://rdrr.io/r/base/print.html),
    [`writeLines()`](https://rdrr.io/r/base/writeLines.html),
    [`library.dynam()`](https://rdrr.io/r/base/library.dynam.html),
    [`require()`](https://rdrr.io/r/base/library.html),
    [`library()`](https://rdrr.io/r/base/library.html), or
    [`installed.packages()`](https://rdrr.io/r/utils/installed.packages.html).

3.  `.Last.lib()` and `.onDetach()` shouldn't call
    [`library.dynam.unload()`](https://rdrr.io/r/base/library.dynam.html).

4.  `.onLoad()` and `.onAttach()` should take two arguments, with names
    matching `^lib` and `^pkg`; `.Last.lib()`, `.onDetach()`, and
    `.onUnload()` should take one argument with name matching `^lib`.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[correctness](https://lintr.r-lib.org/dev/reference/correctness_linters.md),
[package_development](https://lintr.r-lib.org/dev/reference/package_development_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = ".onLoad <- function(lib, ...) { }",
  linters = package_hooks_linter()
)
#> <text>:1:12: warning: [package_hooks_linter] .onLoad() should take two arguments, with the first starting with 'lib' and the second starting with 'pkg'.
#> .onLoad <- function(lib, ...) { }
#>            ^~~~~~~~~~~~~~~~~~~~~~

lint(
  text = ".onAttach <- function(lib, pkg) { require(foo) }",
  linters = package_hooks_linter()
)
#> <text>:1:35: warning: [package_hooks_linter] Don't alter the search() path in .onAttach() by calling require().
#> .onAttach <- function(lib, pkg) { require(foo) }
#>                                   ^~~~~~~

lint(
  text = ".onDetach <- function(pkg) { }",
  linters = package_hooks_linter()
)
#> <text>:1:14: warning: [package_hooks_linter] .onDetach() should take one argument starting with 'lib'.
#> .onDetach <- function(pkg) { }
#>              ^~~~~~~~~~~~~~~~~

lint(
  text = ".onUnload <- function() { }",
  linters = package_hooks_linter()
)
#> <text>:1:14: warning: [package_hooks_linter] .onUnload() should take one argument starting with 'lib'.
#> .onUnload <- function() { }
#>              ^~~~~~~~~~~~~~

# okay
lint(
  text = ".onLoad <- function(lib, pkg) { }",
  linters = package_hooks_linter()
)
#> ℹ No lints found.

lint(
  text = '.onAttach <- function(lib, pkg) { loadNamespace("foo") }',
  linters = package_hooks_linter()
)
#> ℹ No lints found.

lint(
  text = ".onDetach <- function(lib) { }",
  linters = package_hooks_linter()
)
#> ℹ No lints found.

lint(
  text = ".onUnload <- function(libpath) { }",
  linters = package_hooks_linter()
)
#> ℹ No lints found.
```
