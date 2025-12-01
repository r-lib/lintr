# Assignment linter

Check that the specified operator is used for assignment.

## Usage

``` r
assignment_linter(operator = c("<-", "<<-"), allow_trailing = TRUE)
```

## Arguments

- operator:

  Character vector of valid assignment operators. Defaults to allowing
  `<-` and `<<-`; other valid options are `=`, `->`, `->>`, `%<>%`; use
  `"any"` to denote "allow all operators", in which case this linter
  only considers `allow_trailing` for generating lints.

- allow_trailing:

  Logical, default `TRUE`. If `FALSE` then assignments aren't allowed at
  end of lines.

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#assignment-1>

- <https://style.tidyverse.org/pipes.html#assignment-2>

## Tags

[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "x = mean(x)",
  linters = assignment_linter()
)
#> <text>:1:3: style: [assignment_linter] Use one of <-, <<- for assignment, not =.
#> x = mean(x)
#>   ^

code_lines <- "1 -> x\n2 ->> y"
writeLines(code_lines)
#> 1 -> x
#> 2 ->> y
lint(
  text = code_lines,
  linters = assignment_linter()
)
#> <text>:1:3: style: [assignment_linter] Use one of <-, <<- for assignment, not ->.
#> 1 -> x
#>   ^~
#> <text>:2:3: style: [assignment_linter] Use one of <-, <<- for assignment, not ->>.
#> 2 ->> y
#>   ^~~

lint(
  text = "x %<>% as.character()",
  linters = assignment_linter()
)
#> <text>:1:3: style: [assignment_linter] Avoid the assignment pipe %<>%; prefer pipes and assignment in separate steps.
#> x %<>% as.character()
#>   ^~~~

lint(
  text = "x <- 1",
  linters = assignment_linter(operator = "=")
)
#> <text>:1:3: style: [assignment_linter] Use = for assignment, not <-.
#> x <- 1
#>   ^~

# okay
lint(
  text = "x <- mean(x)",
  linters = assignment_linter()
)
#> ℹ No lints found.

code_lines <- "x <- 1\ny <<- 2"
writeLines(code_lines)
#> x <- 1
#> y <<- 2
lint(
  text = code_lines,
  linters = assignment_linter()
)
#> ℹ No lints found.

# customizing using arguments
code_lines <- "1 -> x\n2 ->> y"
writeLines(code_lines)
#> 1 -> x
#> 2 ->> y
lint(
  text = code_lines,
  linters = assignment_linter(operator = "->")
)
#> <text>:2:3: style: [assignment_linter] Replace ->> by assigning to a specific environment (with assign() or <-) to avoid hard-to-predict behavior.
#> 2 ->> y
#>   ^~~

lint(
  text = "x <<- 1",
  linters = assignment_linter(operator = "<-")
)
#> <text>:1:3: style: [assignment_linter] Replace <<- by assigning to a specific environment (with assign() or <-) to avoid hard-to-predict behavior.
#> x <<- 1
#>   ^~~

writeLines("foo(bar = \n 1)")
#> foo(bar = 
#>  1)
lint(
  text = "foo(bar = \n 1)",
  linters = assignment_linter(allow_trailing = FALSE)
)
#> <text>:1:9: style: [assignment_linter] Assignment = should not be trailing at the end of a line.
#> foo(bar = 
#>         ^

lint(
  text = "x %<>% as.character()",
  linters = assignment_linter(operator = "%<>%")
)
#> ℹ No lints found.

lint(
  text = "x = 1",
  linters = assignment_linter(operator = "=")
)
#> ℹ No lints found.
```
