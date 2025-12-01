# Read lintr settings

Lintr searches for settings for a given source file in the following
order:

1.  options defined as `linter.setting`.

2.  `linter_file` in the same directory

3.  `linter_file` in the project directory

4.  `linter_file` in the user home directory

5.  [`default_settings()`](https://lintr.r-lib.org/reference/default_settings.md)

## Usage

``` r
read_settings(filename, call = parent.frame())
```

## Arguments

- filename:

  Source file to be linted.

- call:

  Passed to malformed to ensure linear trace.

## Details

The default linter_file name is `.lintr` but it can be changed with
option `lintr.linter_file` or the environment variable
`R_LINTR_LINTER_FILE` This file is a DCF file, see
[`base::read.dcf()`](https://rdrr.io/r/base/dcf.html) for details. Here
is an example of a `.lintr` file:

    linters: linters_with_defaults(
        any_duplicated_linter(),
        any_is_na_linter(),
        backport_linter("oldrel-4", except = c("R_user_dir", "str2lang")),
        line_length_linter(120L),
        missing_argument_linter(),
        unnecessary_concatenation_linter(allow_single_expression = FALSE),
        yoda_test_linter()
      )
    exclusions: list(
        "inst/doc/creating_linters.R" = 1,
        "inst/example/bad.R",
        "tests/testthat/default_linter_testcode.R",
        "tests/testthat/dummy_packages"
      )

Experimentally, we also support keeping the config in a plain R file. By
default we look for a file named `.lintr.R` (in the same directories
where we search for `.lintr`). We are still deciding the future of
config support in lintr, so user feedback is welcome. The advantage of R
is that it maps more closely to how the configs are actually stored,
whereas the DCF approach requires somewhat awkward formatting of
parseable R code within valid DCF key-value pairs. The main disadvantage
of the R file is it might be *too* flexible, with users tempted to write
configs with side effects causing hard-to-detect bugs or like YAML could
work, but require new dependencies and are harder to parse both
programmatically and visually. Here is an example of a `.lintr.R` file:

    linters <- linters_with_defaults(
        any_duplicated_linter(),
        any_is_na_linter(),
        backport_linter("oldrel-4", except = c("R_user_dir", "str2lang")),
        line_length_linter(120L),
        missing_argument_linter(),
        unnecessary_concatenation_linter(allow_single_expression = FALSE),
        yoda_test_linter()
      )
    exclusions <- list(
        "inst/doc/creating_linters.R" = 1,
        "inst/example/bad.R",
        "tests/testthat/default_linter_testcode.R",
        "tests/testthat/dummy_packages"
      )
