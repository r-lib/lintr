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

## 2. Fuzz Testing Compatibility (`# nofuzz`)

- **Mark multi-file package fixtures with `# nofuzz`:** `lintr` includes an automated test consistency and fuzz-testing harness that mutates and runs `expect_lint()` expressions across various permutations. Tests that create temporary directories (`withr::local_tempdir()`) or multi-file package fixtures (`DESCRIPTION`, `NAMESPACE`) on disk will fail or behave unpredictably under automated fuzzing.
- **Add `# nofuzz` comments:** Explicitly mark any `test_that()` block that creates package files or relies on disk state with `# nofuzz` on the test definition line:

  ```r
  test_that("namespace_linter detects functions already imported in the NAMESPACE", { # nofuzz
    pkg_dir <- withr::local_tempdir("testpkg")
    ...
  })
  ```

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

- **Minimize `rex::rex()` for `lint_messsage` checks:** `expect_lint()` automatically treats string literals in check lists as regular expressions. Avoid the clutter of `rex::rex()` if no regex escapes are required.
- **Pass file paths using the `file = ` keyword argument:** When testing a file on disk (`tmp_file`), always pass it using `file = tmp_file`. Passing a file path as the first positional argument (`expect_lint(tmp_file, ...)`) passes it as `content`, causing `expect_lint()` to treat the file path string itself as R code (resulting in false-positive syntax errors like `unexpected '/'`).

## 4. Concise File Fixtures (`withr::local_tempfile`)

- **Create and populate tempfiles in one step:** When creating single-file test fixtures for `expect_lint()` or `expect_no_lint()` (such as `.Rmd` or `.qmd` documents), use `withr::local_tempfile(fileext = ".Rmd", lines = c(...))` to create and write the file in a single, clean step:

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

## 5. Testing Document Structure & Boundary States

- **Test zero-chunk and edge-case document structures:** When modifying file parsing, source extraction, or chunk boundary detection (`get_chunk_positions`), explicitly test boundary conditions. Always include test cases for:
  - Files with zero code chunks (e.g., plain markdown or YAML frontmatter only).
  - Multi-chunk documents with varied evaluation flags (`eval=FALSE`, `eval=TRUE`, and `#| eval: false`).
  - Different literate formats (`.Rmd`, `.qmd`, `.Rnw`) when format-specific logic is involved.
  - Typically, new such tests belong in tests/testthat/test-knitr_formats.R. Individual linter logic changes typically needn't generate new tests here.
