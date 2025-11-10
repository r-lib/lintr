# Undesirable operator linter

Report the use of undesirable operators, e.g. `:::` or
[`<<-`](https://rdrr.io/r/base/assignOps.html) and suggest an
alternative.

## Usage

``` r
undesirable_operator_linter(
  op = default_undesirable_operators,
  call_is_undesirable = TRUE
)
```

## Arguments

- op:

  Character vector of undesirable operators. Input can be any of three
  types:

  - Unnamed entries must be a character string specifying an undesirable
    operator.

  - For named entries, the name specifies the undesirable operator.

    - If the entry is a character string, it is used as a description of
      why a given operator is undesirable

    - Otherwise, entries should be missing (`NA`) A generic message that
      the named operator is undesirable is used if no specific
      description is provided. Input can also be a list of character
      strings for convenience.

  Defaults to
  [default_undesirable_operators](https://lintr.r-lib.org/dev/reference/default_undesirable_functions.md).
  To make small customizations to this list, use
  [`modify_defaults()`](https://lintr.r-lib.org/dev/reference/modify_defaults.md).

- call_is_undesirable:

  Logical, default `TRUE`. Should lints also be produced for
  prefix-style usage of the operators provided in `op`?

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[robustness](https://lintr.r-lib.org/dev/reference/robustness_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# defaults for which functions are considered undesirable
names(default_undesirable_operators)
#> [1] "->>" ":::" "<<-"

# will produce lints
lint(
  text = "a <<- log(10)",
  linters = undesirable_operator_linter()
)
#> <text>:1:3: warning: [undesirable_operator_linter] Avoid undesirable operator `<<-`. It assigns outside the current environment in a way that can be hard to reason about. Prefer fully-encapsulated functions wherever possible, or, if necessary, assign to a specific environment with assign(). Recall that you can create an environment at the desired scope with new.env().
#> a <<- log(10)
#>   ^~~

lint(
  text = "mtcars$wt",
  linters = undesirable_operator_linter(op = c("$" = "As an alternative, use the `[[` accessor."))
)
#> <text>:1:7: warning: [undesirable_operator_linter] Avoid undesirable operator `$`. As an alternative, use the `[[` accessor.
#> mtcars$wt
#>       ^

lint(
  text = "`:::`(utils, hasName)",
  linters = undesirable_operator_linter()
)
#> <text>:1:1: warning: [undesirable_operator_linter] Avoid undesirable operator `:::`. It accesses non-exported functions inside packages. Code relying on these is likely to break in future versions of the package because the functions are not part of the public interface and may be changed or removed by the maintainers without notice. Use public functions via `::` instead.
#> `:::`(utils, hasName)
#> ^~~~~

lint(
  text = "mtcars$wt",
  linters = undesirable_operator_linter("$")
)
#> <text>:1:7: warning: [undesirable_operator_linter] Avoid undesirable operator `$`.
#> mtcars$wt
#>       ^

# okay
lint(
  text = "a <- log(10)",
  linters = undesirable_operator_linter()
)
#> ℹ No lints found.
lint(
  text = 'mtcars[["wt"]]',
  linters = undesirable_operator_linter(op = c("$" = NA))
)
#> ℹ No lints found.

lint(
  text = 'mtcars[["wt"]]',
  linters = undesirable_operator_linter(op = c("$" = "As an alternative, use the `[[` accessor."))
)
#> ℹ No lints found.

lint(
  text = "`:::`(utils, hasName)",
  linters = undesirable_operator_linter(call_is_undesirable = FALSE)
)
#> <text>:1:1: warning: [undesirable_operator_linter] Avoid undesirable operator `:::`. It accesses non-exported functions inside packages. Code relying on these is likely to break in future versions of the package because the functions are not part of the public interface and may be changed or removed by the maintainers without notice. Use public functions via `::` instead.
#> `:::`(utils, hasName)
#> ^~~~~

lint(
  text = 'mtcars[["wt"]]',
  linters = undesirable_operator_linter("$")
)
#> ℹ No lints found.
```
