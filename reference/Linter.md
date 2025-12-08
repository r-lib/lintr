# Create a `linter` closure

Create a `linter` closure

## Usage

``` r
Linter(
  fun,
  name = linter_auto_name(),
  linter_level = c(NA_character_, "file", "expression")
)
```

## Arguments

- fun:

  A function that takes a source file and returns `lint` objects.

- name:

  Default name of the Linter. Lints produced by the linter will be
  labelled with `name` by default.

- linter_level:

  Which level of expression is the linter working with? `"expression"`
  means an individual expression in `xml_parsed_content`, while `"file"`
  means all expressions in the current file are available in
  `full_xml_parsed_content`. `NA` means the linter will be run with
  both, expression-level and file-level source expressions.

## Value

The same function with its class set to 'linter'.
