# Raise lints for several common poor usages of `paste()`

The following issues are linted by default by this linter (see arguments
for which can be de-activated optionally):

1.  Block usage of [`base::paste()`](https://rdrr.io/r/base/paste.html)
    with `sep = ""`.
    [`base::paste0()`](https://rdrr.io/r/base/paste.html) is a faster,
    more concise alternative.

2.  Block usage of [`paste()`](https://rdrr.io/r/base/paste.html) or
    [`paste0()`](https://rdrr.io/r/base/paste.html) with
    `collapse = ", "`.
    [`toString()`](https://rdrr.io/r/base/toString.html) is a direct
    wrapper for this, and alternatives like
    [`glue::glue_collapse()`](https://glue.tidyverse.org/reference/glue_collapse.html)
    might give better messages for humans.

3.  Block usage of [`paste0()`](https://rdrr.io/r/base/paste.html) that
    supplies `sep=` – this is not a formal argument to `paste0`, and is
    likely to be a mistake.

4.  Block usage of [`paste()`](https://rdrr.io/r/base/paste.html) /
    [`paste0()`](https://rdrr.io/r/base/paste.html) combined with
    [`rep()`](https://rdrr.io/r/base/rep.html) that could be replaced by
    [`base::strrep()`](https://rdrr.io/r/base/strrep.html).
    [`strrep()`](https://rdrr.io/r/base/strrep.html) can handle the task
    of building a block of repeated strings (e.g. often used to build
    "horizontal lines" for messages). This is both more readable and
    skips the (likely small) overhead of putting two strings into the
    global string cache when only one is needed.

    Only target scalar usages – `strrep` can handle more complicated
    cases (e.g. `strrep(letters, 26:1)`, but those aren't as easily
    translated from a `paste(collapse=)` call.

## Usage

``` r
paste_linter(
  allow_empty_sep = FALSE,
  allow_to_string = FALSE,
  allow_file_path = c("double_slash", "always", "never")
)
```

## Arguments

- allow_empty_sep:

  Logical, default `FALSE`. If `TRUE`, usage of
  [`paste()`](https://rdrr.io/r/base/paste.html) with `sep = ""` is not
  linted.

- allow_to_string:

  Logical, default `FALSE`. If `TRUE`, usage of
  [`paste()`](https://rdrr.io/r/base/paste.html) and
  [`paste0()`](https://rdrr.io/r/base/paste.html) with `collapse = ", "`
  is not linted.

- allow_file_path:

  String, one of `"never"`, `"double_slash"`, or `"always"`;
  `"double_slash"` by default. If `"always"`, usage of
  [`paste()`](https://rdrr.io/r/base/paste.html) and
  [`paste0()`](https://rdrr.io/r/base/paste.html) to construct file
  paths is not linted. If `"double_slash"`, strings containing
  consecutive forward slashes will not lint. The main use case here is
  for URLs – "paths" like `"https://"` will not induce lints, since
  constructing them with
  [`file.path()`](https://rdrr.io/r/base/file.path.html) might be deemed
  unnatural. Lastly, if `"never"`, strings with consecutive forward
  slashes will also lint. Note that `"//"` is never linted when it comes
  at the beginning or end of the input, to avoid requiring empty inputs
  like `file.path("", ...)` or `file.path(..., "")`.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = 'paste("a", "b", sep = "")',
  linters = paste_linter()
)
#> <text>:1:1: warning: [paste_linter] paste0(...) is better than paste(..., sep = "").
#> paste("a", "b", sep = "")
#> ^~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = 'paste(c("a", "b"), collapse = ", ")',
  linters = paste_linter()
)
#> <text>:1:1: warning: [paste_linter] toString(.) is more expressive than paste(., collapse = ", "). Note also glue::glue_collapse() and and::and() for constructing human-readable / translation-friendly lists
#> paste(c("a", "b"), collapse = ", ")
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = 'paste0(c("a", "b"), sep = " ")',
  linters = paste_linter()
)
#> <text>:1:1: warning: [paste_linter] sep= is not a formal argument to paste0(); did you mean to use paste(), or collapse=?
#> paste0(c("a", "b"), sep = " ")
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = 'paste0(rep("*", 10L), collapse = "")',
  linters = paste_linter()
)
#> <text>:1:1: warning: [paste_linter] strrep(x, times) is better than paste0(rep(x, times), collapse = "").
#> paste0(rep("*", 10L), collapse = "")
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> <text>:1:1: warning: [paste_linter] Use paste(), not paste0(), to collapse a character vector when sep= is not used.
#> paste0(rep("*", 10L), collapse = "")
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = 'paste0("http://site.com/", path)',
  linters = paste_linter(allow_file_path = "never")
)
#> <text>:1:1: warning: [paste_linter] Construct file paths with file.path(...) instead of paste0(x, "/", y, "/", z). Note that paste() converts empty inputs to "", whereas file.path() leaves it empty.
#> paste0("http://site.com/", path)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = 'paste0(x, collapse = "")',
  linters = paste_linter()
)
#> <text>:1:1: warning: [paste_linter] Use paste(), not paste0(), to collapse a character vector when sep= is not used.
#> paste0(x, collapse = "")
#> ^~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = 'paste0("a", "b")',
  linters = paste_linter()
)
#> ℹ No lints found.

lint(
  text = 'paste("a", "b", sep = "")',
  linters = paste_linter(allow_empty_sep = TRUE)
)
#> ℹ No lints found.

lint(
  text = 'toString(c("a", "b"))',
  linters = paste_linter()
)
#> ℹ No lints found.

lint(
  text = 'paste(c("a", "b"), collapse = ", ")',
  linters = paste_linter(allow_to_string = TRUE)
)
#> ℹ No lints found.

lint(
  text = 'paste(c("a", "b"))',
  linters = paste_linter()
)
#> ℹ No lints found.

lint(
  text = 'strrep("*", 10L)',
  linters = paste_linter()
)
#> ℹ No lints found.

lint(
  text = 'paste0(year, "/", month, "/", day)',
  linters = paste_linter(allow_file_path = "always")
)
#> ℹ No lints found.

lint(
  text = 'paste0("http://site.com/", path)',
  linters = paste_linter()
)
#> ℹ No lints found.

lint(
  text = 'paste(x, collapse = "")',
  linters = paste_linter()
)
#> ℹ No lints found.
```
