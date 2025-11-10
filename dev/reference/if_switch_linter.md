# Require usage of switch() over repeated if/else blocks

[`switch()`](https://rdrr.io/r/base/switch.html) statements in R are
used to delegate behavior based on the value of some input scalar
string, e.g. `switch(x, a = 1, b = 3, c = 7, d = 8)` will be one of `1`,
`3`, `7`, or `8`, depending on the value of `x`.

## Usage

``` r
if_switch_linter(max_branch_lines = 0L, max_branch_expressions = 0L)
```

## Arguments

- max_branch_lines, max_branch_expressions:

  Integer, default 0 indicates "no maximum". If set any
  `if`/`else if`/.../`else` chain where any branch occupies more than
  this number of lines (resp. expressions) will not be linted. The
  conjugate applies to [`switch()`](https://rdrr.io/r/base/switch.html)
  statements – if these parameters are set, any
  [`switch()`](https://rdrr.io/r/base/switch.html) statement with any
  overly-complicated branches will be linted. See examples.

## Details

This can also be accomplished by repeated `if`/`else` statements like
so: `if (x == "a") 1 else if (x == "b") 2 else if (x == "c") 7 else 8`
(implicitly, the last `else` assumes x only takes 4 possible values),
but this is more cluttered and slower (note that
[`switch()`](https://rdrr.io/r/base/switch.html) takes the same time to
evaluate regardless of the value of `x`, and is faster even when `x`
takes the first value (here `a`), and that the `if`/`else` approach is
roughly linear in the number of conditions that need to be evaluated,
here up to 3 times).

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "if (x == 'a') 1 else if (x == 'b') 2 else 3",
  linters = if_switch_linter()
)
#> ℹ No lints found.

code <- paste(
  "if (x == 'a') {",
  "  1",
  "} else if (x == 'b') {",
  "  2",
  "} else if (x == 'c') {",
  "  y <- x",
  "  z <- sqrt(match(y, letters))",
  "  z",
  "}",
  sep = "\n"
)
writeLines(code)
#> if (x == 'a') {
#>   1
#> } else if (x == 'b') {
#>   2
#> } else if (x == 'c') {
#>   y <- x
#>   z <- sqrt(match(y, letters))
#>   z
#> }
lint(
  text = code,
  linters = if_switch_linter()
)
#> <text>:1:1: warning: [if_switch_linter] Prefer switch() statements over repeated if/else equality tests, e.g., switch(x, a = 1, b = 2) over if (x == "a") 1 else if (x == "b") 2.
#> if (x == 'a') {
#> ^~~~~~~~~~~~~~~

code <- paste(
  "if (x == 'a') {",
  "  1",
  "} else if (x == 'b') {",
  "  2",
  "} else if (x == 'c') {",
  "  y <- x",
  "  z <- sqrt(",
  "    match(y, letters)",
  "  )",
  "  z",
  "}",
  sep = "\n"
)
writeLines(code)
#> if (x == 'a') {
#>   1
#> } else if (x == 'b') {
#>   2
#> } else if (x == 'c') {
#>   y <- x
#>   z <- sqrt(
#>     match(y, letters)
#>   )
#>   z
#> }
lint(
  text = code,
  linters = if_switch_linter()
)
#> <text>:1:1: warning: [if_switch_linter] Prefer switch() statements over repeated if/else equality tests, e.g., switch(x, a = 1, b = 2) over if (x == "a") 1 else if (x == "b") 2.
#> if (x == 'a') {
#> ^~~~~~~~~~~~~~~

code <- paste(
  "switch(x,",
  "  a = {",
  "    1",
  "    2",
  "    3",
  "  },",
  "  b = {",
  "    1",
  "    2",
  "  }",
  ")",
  sep = "\n"
)
writeLines(code)
#> switch(x,
#>   a = {
#>     1
#>     2
#>     3
#>   },
#>   b = {
#>     1
#>     2
#>   }
#> )
lint(
  text = code,
  linters = if_switch_linter(max_branch_lines = 2L)
)
#> <text>:1:1: warning: [if_switch_linter] Prefer repeated if/else statements over overly-complicated switch() statements.
#> switch(x,
#> ^~~~~~~~~

# okay
lint(
  text = "switch(x, a = 1, b = 2, 3)",
  linters = if_switch_linter()
)
#> ℹ No lints found.

# switch() version not as clear
lint(
  text = "if (x == 'a') 1 else if (x == 'b' & y == 2) 2 else 3",
  linters = if_switch_linter()
)
#> ℹ No lints found.

code <- paste(
  "if (x == 'a') {",
  "  1",
  "} else if (x == 'b') {",
  "  2",
  "} else if (x == 'c') {",
  "  y <- x",
  "  z <- sqrt(match(y, letters))",
  "  z",
  "}",
  sep = "\n"
)
writeLines(code)
#> if (x == 'a') {
#>   1
#> } else if (x == 'b') {
#>   2
#> } else if (x == 'c') {
#>   y <- x
#>   z <- sqrt(match(y, letters))
#>   z
#> }
lint(
  text = code,
  linters = if_switch_linter(max_branch_lines = 2L)
)
#> ℹ No lints found.

code <- paste(
  "if (x == 'a') {",
  "  1",
  "} else if (x == 'b') {",
  "  2",
  "} else if (x == 'c') {",
  "  y <- x",
  "  z <- sqrt(",
  "    match(y, letters)",
  "  )",
  "  z",
  "}",
  sep = "\n"
)
writeLines(code)
#> if (x == 'a') {
#>   1
#> } else if (x == 'b') {
#>   2
#> } else if (x == 'c') {
#>   y <- x
#>   z <- sqrt(
#>     match(y, letters)
#>   )
#>   z
#> }
lint(
  text = code,
  linters = if_switch_linter(max_branch_expressions = 2L)
)
#> ℹ No lints found.

code <- paste(
  "switch(x,",
  "  a = {",
  "    1",
  "    2",
  "    3",
  "  },",
  "  b = {",
  "    1",
  "    2",
  "  }",
  ")",
  sep = "\n"
)
writeLines(code)
#> switch(x,
#>   a = {
#>     1
#>     2
#>     3
#>   },
#>   b = {
#>     1
#>     2
#>   }
#> )
lint(
  text = code,
  linters = if_switch_linter(max_branch_lines = 3L)
)
#> ℹ No lints found.
```
