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
- **No `rex::rex()` wrapper required for pattern strings:** `expect_lint()` automatically treats string literals in check lists as regular expressions. You do not need to wrap strings in `rex::rex(...)` unless constructing complex `rex` grammar objects.
