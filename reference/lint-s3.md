# Create a `lint` object

Create a `lint` object

## Usage

``` r
Lint(
  filename,
  line_number = 1L,
  column_number = 1L,
  type = c("style", "warning", "error"),
  message = "",
  line = "",
  ranges = NULL
)
```

## Arguments

- filename:

  path to the source file that was linted.

- line_number:

  line number where the lint occurred.

- column_number:

  column number where the lint occurred.

- type:

  type of lint.

- message:

  message used to describe the lint error

- line:

  code source where the lint occurred

- ranges:

  a list of ranges on the line that should be emphasized.

## Value

an object of class `c("lint", "list")`.
