# Avoid implicit assignment in function calls

Assigning inside function calls makes the code difficult to read, and
should be avoided, except for functions that capture side-effects (e.g.
[`utils::capture.output()`](https://rdrr.io/r/utils/capture.output.html)).

## Usage

``` r
implicit_assignment_linter(
  except = c("bquote", "expression", "expr", "quo", "quos", "quote"),
  allow_lazy = FALSE,
  allow_scoped = FALSE,
  allow_paren_print = FALSE
)
```

## Arguments

- except:

  A character vector of functions to be excluded from linting.

- allow_lazy:

  logical, default `FALSE`. If `TRUE`, assignments that only trigger
  conditionally (e.g. in the RHS of `&&` or `||` expressions) are
  skipped.

- allow_scoped:

  Logical, default `FALSE`. If `TRUE`, "scoped assignments", where the
  object is assigned in the statement beginning a branch and used only
  within that branch, are skipped.

- allow_paren_print:

  Logical, default `FALSE`. If `TRUE`, assignments using `(` for
  auto-printing at the top-level are not linted.

## See also

- [linters](https://lintr.r-lib.org/reference/linters.md) for a complete
  list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#assignment>

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md),
[style](https://lintr.r-lib.org/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "if (x <- 1L) TRUE",
  linters = implicit_assignment_linter()
)
#> <text>:1:5: warning: [implicit_assignment_linter] Avoid implicit assignments in function calls. For example, instead of `if (x <- 1L) { ... }`, write `x <- 1L; if (x) { ... }`.
#> if (x <- 1L) TRUE
#>     ^~~~~~~

lint(
  text = "mean(x <- 1:4)",
  linters = implicit_assignment_linter()
)
#> <text>:1:6: warning: [implicit_assignment_linter] Avoid implicit assignments in function calls. For example, instead of `if (x <- 1L) { ... }`, write `x <- 1L; if (x) { ... }`.
#> mean(x <- 1:4)
#>      ^~~~~~~~

lint(
  text = "(x <- 1)",
  linters = implicit_assignment_linter()
)
#> <text>:1:2: warning: [implicit_assignment_linter] Call print() explicitly instead of relying on implicit printing behavior via '('.
#> (x <- 1)
#>  ^~~~~~


# okay
lines <- "x <- 1L\nif (x) TRUE"
writeLines(lines)
#> x <- 1L
#> if (x) TRUE
lint(
  text = lines,
  linters = implicit_assignment_linter()
)
#> ℹ No lints found.

lines <- "x <- 1:4\nmean(x)"
writeLines(lines)
#> x <- 1:4
#> mean(x)
lint(
  text = lines,
  linters = implicit_assignment_linter()
)
#> ℹ No lints found.

lint(
  text = "A && (B <- foo(A))",
  linters = implicit_assignment_linter(allow_lazy = TRUE)
)
#> ℹ No lints found.

lines <- c(
  "if (any(idx <- x < 0)) {",
  "  stop('negative elements: ', toString(which(idx)))",
  "}"
)
writeLines(lines)
#> if (any(idx <- x < 0)) {
#>   stop('negative elements: ', toString(which(idx)))
#> }
lint(
  text = lines,
  linters = implicit_assignment_linter(allow_scoped = TRUE)
)
#> ℹ No lints found.

lint(
  text = "(x <- 1)",
  linters = implicit_assignment_linter(allow_paren_print = TRUE)
)
#> ℹ No lints found.
```
