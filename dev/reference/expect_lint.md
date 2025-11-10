# Lint expectation

These are expectation functions to test specified linters on sample code
in the `testthat` testing framework.

- `expect_lint` asserts that specified lints are generated.

- `expect_no_lint` asserts that no lints are generated.

## Usage

``` r
expect_lint(
  content,
  checks,
  ...,
  file = NULL,
  language = "en",
  ignore_order = FALSE
)

expect_no_lint(content, ..., file = NULL, language = "en")
```

## Arguments

- content:

  A character vector for the file content to be linted, each vector
  element representing a line of text.

- checks:

  Checks to be performed:

  NULL

  :   check that no lints are returned.

  single string or regex object

  :   check that the single lint returned has a matching message.

  named list

  :   check that the single lint returned has fields that match.
      Accepted fields are the same as those taken by
      [`Lint()`](https://lintr.r-lib.org/dev/reference/lint-s3.md).

  list of named lists

  :   for each of the multiple lints returned, check that it matches the
      checks in the corresponding named list (as described in the point
      above).

  Named vectors are also accepted instead of named lists, but this is a
  compatibility feature that is not recommended for new code.

- ...:

  Arguments passed to
  [`lint()`](https://lintr.r-lib.org/dev/reference/lint.md), e.g. the
  linters or cache to use.

- file:

  If not `NULL`, read content from the specified file rather than from
  `content`.

- language:

  Temporarily override Rs `LANGUAGE` envvar, controlling localization of
  base R error messages. This makes testing them reproducible on all
  systems irrespective of their native R language setting.

- ignore_order:

  Logical, default `FALSE`. If `TRUE`, the order of the `checks` does
  not matter, e.g. lints with higher line numbers can come before those
  with lower line numbers, and the order of linters affecting the same
  line is also irrelevant.

## Value

`NULL`, invisibly.

## Examples

``` r
# no expected lint
expect_no_lint("a", trailing_blank_lines_linter())

# one expected lint
expect_lint("a\n", "trailing blank", trailing_blank_lines_linter())
expect_lint("a\n", list(message = "trailing blank", line_number = 2), trailing_blank_lines_linter())

# several expected lints
expect_lint("a\n\n", list("trailing blank", "trailing blank"), trailing_blank_lines_linter())
expect_lint(
  "a\n\n",
  list(
    list(message = "trailing blank", line_number = 2),
    list(message = "trailing blank", line_number = 3)
  ),
  trailing_blank_lines_linter()
)
```
