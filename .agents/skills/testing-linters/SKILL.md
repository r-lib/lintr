---
name: testing-linters
description: Guidelines and conventions for writing unit tests for linters in the r-lib/lintr package using testthat. Use this skill whenever creating or modifying linter test files under tests/testthat/.
---

# Testing Linters in `r-lib/lintr`

When writing or updating unit tests in `tests/testthat/test-*.R`, adhere to the following testing conventions and production-readiness standards:

## 1. Constructing Package Fixtures (`DESCRIPTION` & `NAMESPACE`)

- **Use `write.dcf()` for `DESCRIPTION` files:** When creating temporary package structures (e.g., using `withr::local_tempdir()`), always construct `DESCRIPTION` files using base R's `write.dcf()` rather than manual string concatenation:

  ```r
  write.dcf(
    list(Package = "testpkg", Version = "1.0.0"),
    file.path(pkg_dir, "DESCRIPTION")
  )
  ```

- **Pass character vectors to `writeLines()`:** Never pass newline-delimited single strings (`"line1\nline2\n"`) to `writeLines()`, as `writeLines()` appends its own trailing newline and expects a character vector where each element is a line:

  ```r
  writeLines(
    c("importFrom(stats, median)", "importFrom(utils, head)"),
    file.path(pkg_dir, "NAMESPACE")
  )
  ```

## 2. Fuzz Testing & Syntax Permutations (`# nofuzz`)
- **Mark multi-file package fixtures with `# nofuzz`:** `lintr` includes an automated test consistency and fuzz-testing harness that mutates and runs `expect_lint()` expressions across various permutations. Tests that create temporary directories (`withr::local_tempdir()`) or multi-file package fixtures (`DESCRIPTION`, `NAMESPACE`) on disk will fail or behave unpredictably under automated fuzzing. Explicitly mark any such block with `# nofuzz` on the test definition line:
  ```r
  test_that("namespace_linter detects functions already imported in the NAMESPACE", { # nofuzz
    pkg_dir <- withr::local_tempdir("testpkg")
    ...
  })
  ```
- **Trust the fuzzing suite for standard syntax permutations:** Because the automated fuzzing harness exercises interchangeable R syntax structures across tests (such as mutating `function(...)` definitions into lambda shorthand `\(...)`), **do not demand separate positive/negative unit tests for interchangeable syntax variations** (`\()` vs `function()`) unless the individual linter implementation has custom code explicitly handling `OP-LAMBDA` vs `FUNCTION` differently.

## 3. Writing `expect_lint()` Assertions
- **Explicit argument names:** When asserting lints against a file path (`file = test_file`), explicitly name the `checks` and `linters` arguments rather than relying on positional ordering:
  ```r
  expect_lint(
    file = test_file,
    checks = list(
      list("Don't use `::` to access median.*already imported", line_number = 1L),
      list("Don't use `:::` to access head.*already imported", line_number = 2L)
    ),
    linters = namespace_linter()
  )
  ```
- **Avoid `rex::rex()` around un-escaped strings to reduce visual noise:** `expect_lint()` natively interprets check strings as regular expressions. Do not wrap plain target strings in `rex::rex("...")` when no regex metacharacters require escaping (`()`, `[]`, etc.) or advanced composition. The specific purpose of omitting `rex::rex()` on clean literal matches is to directly eliminate repetitive visual noise in test suites.
- **Pass file paths using the `file = ` keyword argument:** When testing a file on disk (`tmp_file`), always pass it using `file = tmp_file`. Passing a file path as the first positional argument (`expect_lint(tmp_file, ...)`) passes it as `content`, causing `expect_lint()` to treat the file path string itself as R code (resulting in false-positive syntax errors like `unexpected '/'`).

## 4. Concise File Fixtures (`withr::local_tempfile`)
- **Create and populate tempfiles in one step:** When creating single-file test fixtures for `expect_lint()` or `expect_no_lint()`, use `withr::local_tempfile(fileext = ".Rmd", lines = c(...))` to create and write the file cleanly:
  ```r
  test_that("chunkless files are fine", {
    tmp <- withr::local_tempfile(fileext = ".Rmd", lines = c(
      "---",
      "some_option: true",
      "---",
      "Some text!"
    ))
    expect_no_lint(file = tmp, linters = assignment_linter())
  })
  ```

## 5. Testing Document Structure & Literate Formats
- **Keep literate (`.Rmd`/`.qmd`) boundary tests out of individual linter suites:** It is exceedingly rare for an individual linter to warrant having `.Rmd`, `.qmd`, or other literate document format tests within its own dedicated test file (`tests/testthat/test-<linter_name>.R`). Almost exclusively, tests targeting literate formats, `NA_character_` masked line extraction, zero-chunk, or multi-chunk boundary parsing belong in `tests/testthat/test-knitr_formats.R` (or extraction suites). **Do not request or add `.Rmd`/`.qmd` unit tests when reviewing individual linter implementations**, unless the linter executes custom format-specific logic directly dependent on those extensions.
