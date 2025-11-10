# TODO comment linter

Check that the source contains no TODO comments (case-insensitive).

## Usage

``` r
todo_comment_linter(todo = c("todo", "fixme"), except_regex = NULL)
```

## Arguments

- todo:

  Vector of case-insensitive strings that identify TODO comments.

- except_regex:

  Vector of case-sensitive regular expressions that identify *valid*
  TODO comments.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "x + y # TOODOO",
  linters = todo_comment_linter(todo = "toodoo")
)
#> <text>:1:7: style: [todo_comment_linter] Remove TODO comments.
#> x + y # TOODOO
#>       ^~~~~~~~

lint(
  text = "pi <- 1.0 # FIIXMEE",
  linters = todo_comment_linter(todo = "fiixmee")
)
#> <text>:1:11: style: [todo_comment_linter] Remove TODO comments.
#> pi <- 1.0 # FIIXMEE
#>           ^~~~~~~~~

lint(
  text = "x <- TRUE # TOODOO(#1234): Fix this hack.",
  linters = todo_comment_linter()
)
#> ℹ No lints found.

# okay
lint(
  text = "x + y # my informative comment",
  linters = todo_comment_linter()
)
#> ℹ No lints found.

lint(
  text = "pi <- 3.14",
  linters = todo_comment_linter()
)
#> ℹ No lints found.

lint(
  text = "x <- TRUE",
  linters = todo_comment_linter()
)
#> ℹ No lints found.

lint(
  text = "x <- TRUE # TODO(#1234): Fix this hack.",
  linters = todo_comment_linter(except_regex = "TODO\\(#[0-9]+\\):")
)
#> ℹ No lints found.
```
