# read a source file and parse all the excluded lines from it

read a source file and parse all the excluded lines from it

## Usage

``` r
parse_exclusions(
  file,
  exclude = settings$exclude,
  exclude_next = settings$exclude_next,
  exclude_start = settings$exclude_start,
  exclude_end = settings$exclude_end,
  exclude_linter = settings$exclude_linter,
  exclude_linter_sep = settings$exclude_linter_sep,
  lines = NULL,
  linter_names = NULL
)
```

## Arguments

- file:

  R source file

- exclude:

  Regular expression used to mark lines to exclude.

- exclude_next:

  Regular expression used to mark lines immediately preceding excluded
  lines.

- exclude_start:

  Regular expression used to mark the start of an excluded range.

- exclude_end:

  Regular expression used to mark the end of an excluded range.

- exclude_linter:

  Regular expression used to capture a list of to-be-excluded linters
  immediately following a `exclude` or `exclude_start` marker.

- exclude_linter_sep:

  Regular expression used to split a linter list into individual linter
  names for exclusion.

- lines:

  A character vector of the content lines of `file`.

- linter_names:

  Names of active linters.

## Value

A possibly named list of excluded lines, possibly for specific linters.
