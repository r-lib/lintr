# Block instances of unnecessary nesting

Excessive nesting harms readability. Use helper functions or early
returns to reduce nesting wherever possible.

## Usage

``` r
unnecessary_nesting_linter(
  allow_assignment = TRUE,
  allow_functions = c("switch", "try", "tryCatch", "withCallingHandlers", "quote",
    "expression", "bquote", "substitute", "with_parameters_test_that", "reactive",
    "observe", "observeEvent", "renderCachedPlot", "renderDataTable", "renderImage",
    "renderPlot", "renderPrint", "renderTable", "renderText", "renderUI"),
  branch_exit_calls = character()
)
```

## Arguments

- allow_assignment:

  Logical, default `TRUE`, in which case braced expressions consisting
  only of a single assignment are skipped. if `FALSE`, all braced
  expressions with only one child expression are linted. The `TRUE` case
  facilitates interaction with
  [`implicit_assignment_linter()`](https://lintr.r-lib.org/reference/implicit_assignment_linter.md)
  for certain cases where an implicit assignment is necessary, so a
  braced assignment is used to further distinguish the assignment. See
  examples.

- allow_functions:

  Character vector of functions which always allow one-child braced
  expressions.
  [`testthat::test_that()`](https://testthat.r-lib.org/reference/test_that.html)
  is always allowed because testthat requires a braced expression in its
  `code` argument. The other defaults similarly compute on expressions
  in a way which is worth highlighting by em-bracing them, even if there
  is only one expression, while
  [`switch()`](https://rdrr.io/r/base/switch.html) is allowed for its
  use as a control flow analogous to `if`/`else`.\]

- branch_exit_calls:

  Character vector of functions which are considered as "exiting" a
  branch for the purpose of recommending removing nesting in a branch
  *lacking* an exit call when the other branch terminates with one.
  Calls which always interrupt or quit the current call or R session,
  e.g. [`stop()`](https://rdrr.io/r/base/stop.html) and
  [`q()`](https://rdrr.io/r/base/quit.html), are always included.

## See also

- [`cyclocomp_linter()`](https://lintr.r-lib.org/reference/cyclocomp_linter.md)
  for another linter that penalizes overly complex code.

- [linters](https://lintr.r-lib.org/reference/linters.md) for a complete
  list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[consistency](https://lintr.r-lib.org/reference/consistency_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md)

## Examples

``` r
# will produce lints
code <- "if (A) {\n  stop('A is bad!')\n} else {\n  do_good()\n}"
writeLines(code)
#> if (A) {
#>   stop('A is bad!')
#> } else {
#>   do_good()
#> }
lint(
  text = code,
  linters = unnecessary_nesting_linter()
)
#> <text>:1:1: warning: [unnecessary_nesting_linter] Reduce the nesting of this if/else statement by unnesting the portion without an exit clause, i.e., stop().
#> if (A) {
#> ^~~~~~~~

code <- "tryCatch(\n  {\n    foo()\n  },\n  error = identity\n)"
writeLines(code)
#> tryCatch(
#>   {
#>     foo()
#>   },
#>   error = identity
#> )
lint(
  text = code,
  linters = unnecessary_nesting_linter()
)
#> ℹ No lints found.

code <- "expect_warning(\n  {\n    x <- foo()\n  },\n  'warned'\n)"
writeLines(code)
#> expect_warning(
#>   {
#>     x <- foo()
#>   },
#>   'warned'
#> )
lint(
  text = code,
  linters = unnecessary_nesting_linter(allow_assignment = FALSE)
)
#> <text>:2:3: warning: [unnecessary_nesting_linter] Reduce the nesting of this statement by removing the braces {}.
#>   {
#>   ^

code <- "if (x) { \n  if (y) { \n   return(1L) \n  } \n}"
writeLines(code)
#> if (x) { 
#>   if (y) { 
#>    return(1L) 
#>   } 
#> }
lint(
  text = code,
  linters = unnecessary_nesting_linter()
)
#> <text>:2:3: warning: [unnecessary_nesting_linter] Combine this `if` statement with the one found at line 1, column 1 to reduce nesting.
#>   if (y) { 
#>   ^~~~~~~~~

lint(
  text = "my_quote({x})",
  linters = unnecessary_nesting_linter()
)
#> ℹ No lints found.

code <- paste(
  "if (A) {",
  "  stop('A is bad because a.')",
  "} else {",
  "  warning('!A requires caution.')",
  "}",
  sep = "\n"
)
writeLines(code)
#> if (A) {
#>   stop('A is bad because a.')
#> } else {
#>   warning('!A requires caution.')
#> }
lint(
  text = code,
  linters = unnecessary_nesting_linter()
)
#> <text>:1:1: warning: [unnecessary_nesting_linter] Reduce the nesting of this if/else statement by unnesting the portion without an exit clause, i.e., stop().
#> if (A) {
#> ^~~~~~~~

# okay
code <- "if (A) {\n  stop('A is bad because a.')\n} else {\n  stop('!A is bad too.')\n}"
writeLines(code)
#> if (A) {
#>   stop('A is bad because a.')
#> } else {
#>   stop('!A is bad too.')
#> }
lint(
  text = code,
  linters = unnecessary_nesting_linter()
)
#> ℹ No lints found.

code <- "capture.output({\n  foo()\n})"
writeLines(code)
#> capture.output({
#>   foo()
#> })
lint(
  text = code,
  linters = unnecessary_nesting_linter()
)
#> ℹ No lints found.

code <- "expect_warning(\n  {\n    x <- foo()\n  },\n  'warned'\n)"
writeLines(code)
#> expect_warning(
#>   {
#>     x <- foo()
#>   },
#>   'warned'
#> )
lint(
  text = code,
  linters = unnecessary_nesting_linter()
)
#> ℹ No lints found.

code <- "if (x && y) { \n  return(1L) \n}"
writeLines(code)
#> if (x && y) { 
#>   return(1L) 
#> }
lint(
  text = code,
  linters = unnecessary_nesting_linter()
)
#> ℹ No lints found.

code <- "if (x) { \n  y <- x + 1L\n  if (y) { \n   return(1L) \n  } \n}"
writeLines(code)
#> if (x) { 
#>   y <- x + 1L
#>   if (y) { 
#>    return(1L) 
#>   } 
#> }
lint(
  text = code,
  linters = unnecessary_nesting_linter()
)
#> ℹ No lints found.

lint(
  text = "my_quote({x})",
  linters = unnecessary_nesting_linter(allow_functions = "my_quote")
)
#> ℹ No lints found.

code <- paste(
  "if (A) {",
  "  stop('A is bad because a.')",
  "} else {",
  "  warning('!A requires caution.')",
  "}",
  sep = "\n"
)
writeLines(code)
#> if (A) {
#>   stop('A is bad because a.')
#> } else {
#>   warning('!A requires caution.')
#> }
lint(
  text = code,
  linters = unnecessary_nesting_linter(branch_exit_calls = c("stop", "warning"))
)
#> ℹ No lints found.
```
