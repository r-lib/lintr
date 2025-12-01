# Exclude lines or files from linting

Exclude lines or files from linting

## Usage

``` r
exclude(lints, exclusions = settings$exclusions, linter_names = NULL, ...)
```

## Arguments

- lints:

  that need to be filtered.

- exclusions:

  manually specified exclusions

- linter_names:

  character vector of names of the active linters, used for parsing
  inline exclusions.

- ...:

  additional arguments passed to
  [`parse_exclusions()`](https://lintr.r-lib.org/reference/parse_exclusions.md)

## Details

Exclusions can be specified in three different ways.

1.  Single line in the source file. default: `# nolint`, possibly
    followed by a listing of linters to exclude. If the listing is
    missing, all linters are excluded on that line. The default listing
    format is `# nolint: linter_name, linter2_name.`. There may not be
    anything between the colon and the line exclusion tag and the
    listing must be terminated with a full stop (`.`) for the linter
    list to be respected.

2.  Line range in the source file. default: `# nolint start`,
    `# nolint end`. `# nolint start` accepts linter lists in the same
    form as `# nolint`.

3.  Exclusions parameter, a list with named and/or unnamed entries.
    Outer elements have the following characteristics:

    1.  Unnamed elements specify filenames or directories.

    2.  Named elements are a vector or list of line numbers, with `Inf`
        indicating 'all lines'. The name gives a path relative to the
        config.

        1.  Unnamed elements denote exclusion of all linters in the
            given path or directory.

        2.  Named elements, where the name specifies a linter, denote
            exclusion for that linter. For convenience, a vector can be
            used in place of a list whenever it would not introduce
            ambiguity, e.g. a character vector of files to exclude or a
            vector of lines to exclude.

    Note also that:

    1.  All paths are interpreted as globs
        ([`Sys.glob()`](https://rdrr.io/r/base/Sys.glob.html)), so that
        e.g. `*` does pattern expansion.

    2.  For `exclusions` provided *via* a config file, all paths are
        taken as relative *to the config file itself*, not the path
        where [`lint()`](https://lintr.r-lib.org/reference/lint.md) is
        run.
