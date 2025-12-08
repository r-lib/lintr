# Normalize lint exclusions

Normalize lint exclusions

## Usage

``` r
normalize_exclusions(x, normalize_path = TRUE, root = getwd(), pattern = NULL)
```

## Arguments

- x:

  Exclusion specification

  - A character vector of filenames or directories relative to `root`.
    Interpreted as globs, see
    [`Sys.glob()`](https://rdrr.io/r/base/Sys.glob.html).

  - A named list of integers specifying lines to be excluded per file

  - A named list of named lists specifying linters and lines to be
    excluded for the linters per file.

- normalize_path:

  Should the names of the returned exclusion list be normalized paths?
  If `FALSE`, they will be relative to `root`.

- root:

  Base directory for relative filename resolution.

- pattern:

  If non-NULL, only exclude files in excluded directories if they match
  `pattern`. Passed to
  [`list.files()`](https://rdrr.io/r/base/list.files.html) if a
  directory is excluded.

## Value

A named list of file exclusions. The names of the list specify the
filenames to be excluded.

Each file exclusion is a possibly named list containing line numbers to
exclude, or the sentinel `Inf` for completely excluded files. If the an
entry is named, the exclusions only take effect for the linter with the
same name.

If `normalize_path` is `TRUE`, file names will be normalized relative to
`root`. Otherwise the paths are left as provided (relative to `root` or
absolute). That also means existence is not checked.
