# Lint a file, directory, or package

- `lint()` lints a single file.

- `lint_dir()` lints all files in a directory.

- `lint_package()` lints all likely locations for R files in a package,
  i.e. `R/`, `tests/`, `inst/`, `vignettes/`, `data-raw/`, `demo/`, and
  `exec/`.

## Usage

``` r
lint(
  filename,
  linters = NULL,
  ...,
  cache = FALSE,
  parse_settings = TRUE,
  text = NULL
)

lint_dir(
  path = ".",
  ...,
  relative_path = TRUE,
  exclusions = list("renv", "packrat"),
  pattern = "(?i)[.](r|rmd|qmd|rnw|rhtml|rrst|rtex|rtxt)$",
  parse_settings = TRUE,
  show_progress = NULL
)

lint_package(
  path = ".",
  ...,
  relative_path = TRUE,
  exclusions = list("R/RcppExports.R"),
  parse_settings = TRUE,
  show_progress = NULL
)
```

## Arguments

- filename:

  Either the filename for a file to lint, or a character string of
  inline R code for linting. The latter (inline data) applies whenever
  `filename` has a newline character (\n).

- linters:

  A named list of linter functions to apply. See
  [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a full
  list of default and available linters.

- ...:

  Provide additional arguments to be passed to:

  - [`exclude()`](https://lintr.r-lib.org/dev/reference/exclude.md) (in
    case of `lint()`; e.g. `lints` or `exclusions`)

  - `lint()` (in case of `lint_dir()` and `lint_package()`; e.g.
    `linters` or `cache`)

- cache:

  When logical, toggle caching of lint results. If passed a character
  string, store the cache in this directory.

- parse_settings:

  Logical, default `TRUE`. Whether to try and parse the
  [settings](https://lintr.r-lib.org/dev/reference/read_settings.md).
  Otherwise, the
  [`default_settings()`](https://lintr.r-lib.org/dev/reference/default_settings.md)
  are used.

- text:

  Optional argument for supplying a string or lines directly, e.g. if
  the file is already in memory or linting is being done ad hoc.

- path:

  For the base directory of the project (for `lint_dir()`) or package
  (for `lint_package()`).

- relative_path:

  if `TRUE`, file paths are printed using their path relative to the
  base directory. If `FALSE`, use the full absolute path.

- exclusions:

  exclusions for
  [`exclude()`](https://lintr.r-lib.org/dev/reference/exclude.md),
  relative to the package path.

- pattern:

  pattern for files, by default it will take files with any of the
  extensions .R, .Rmd, .qmd, .Rnw, .Rhtml, .Rrst, .Rtex, .Rtxt allowing
  for lowercase r (.r, ...).

- show_progress:

  Logical controlling whether to show linting progress with
  [`cli::cli_progress_along()`](https://cli.r-lib.org/reference/cli_progress_along.html).
  The default behavior is to show progress in
  [`interactive()`](https://rdrr.io/r/base/interactive.html) sessions
  not running a testthat suite.

## Value

An object of class `c("lints", "list")`, each element of which is a
`"list"` object.

## Details

Read
[`vignette("lintr")`](https://lintr.r-lib.org/dev/articles/lintr.md) to
learn how to configure which linters are run by default. Note that if
files contain unparseable encoding problems, only the encoding problem
will be linted to avoid unintelligible error messages from other
linters.

## Examples

``` r
# linting inline-code
lint("a = 123\n")
#> <text>:1:3: style: [assignment_linter] Use one of <-, <<- for assignment, not =.
#> a = 123
#>   ^
lint(text = "a = 123")
#> <text>:1:3: style: [assignment_linter] Use one of <-, <<- for assignment, not =.
#> a = 123
#>   ^

# linting a file
f <- tempfile()
writeLines("a=1", f)
lint(f)
#> /tmp/RtmpXofplz/file19294b3f06ee:1:2: style: [assignment_linter] Use one of <-, <<- for assignment, not =.
#> a=1
#>  ^
#> /tmp/RtmpXofplz/file19294b3f06ee:1:2: style: [infix_spaces_linter] Put spaces around all infix operators.
#> a=1
#>  ^
unlink(f)

if (FALSE) {
  lint_dir()

  lint_dir(
    linters = list(semicolon_linter()),
    exclusions = list(
      "inst/doc/creating_linters.R" = 1,
      "inst/example/bad.R",
      "renv"
    )
  )
}
if (FALSE) {
  lint_package()

  lint_package(
    linters = linters_with_defaults(semicolon_linter = semicolon_linter()),
    exclusions = list("inst/doc/creating_linters.R" = 1, "inst/example/bad.R")
  )
}
```
