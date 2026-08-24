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
- **Avoid `rex::rex()` around plain strings to reduce visual noise:** `expect_lint()` natively interprets check strings as regular expressions. Do not wrap plain target strings in `rex::rex("...")` when no regex metacharacters require escaping (`()`, `[]`, `.`, `$`, etc.) or advanced composition. The specific purpose of omitting `rex::rex()` on clean literal matches is to directly eliminate repetitive visual noise in test suites, but it _should_ be used to reduce the visual noise of '\\' used to escape regex specials, especially repeated ones.
- **Pass file paths using the `file = ` keyword argument:** When testing a file on disk (`tmp_file`), always pass it using `file = tmp_file`. Passing a file path as the first positional argument (`expect_lint(tmp_file, ...)`) passes it as `content`, causing `expect_lint()` to treat the file path string itself as R code (resulting in false-positive syntax errors like `unexpected '/'`).
- **Selective use of raw string literals (`R'{...}'`):** Reserve R raw string literals (`R'{...}'` or `R"{...}"`) exclusively for code strings that genuinely require backslash escaping (`\n`, `\\.`, `\1`, etc.). Do not reflexively wrap clean, unescaped target strings (`'list.files("foo")'` or `'dir(pattern = "abc")'`) inside `R'{...}'`. Utilizing standard quotes (`'...'` or `"..."`) when backslash escapes are absent keeps test assertions clean and readable.
- **Avoid repetitive assertion comments:** When writing sequential assertions exercising related cases, prefer grouping them under a single high-level summary comment (e.g. `# position= inferred positionally` or `# other behavior of the linter continues WAI`) instead of repeating redundant comments above every single `expect_lint()` or `expect_no_lint()` call.
- **Positional message parameter in `expect_lint()` check lists:** In `expect_lint()` check lists, do not explicitly write `message = rex::rex(...)` or `message = "..."`. Pass the message regex or string positionally as the first element of the check list: `list(rex::rex("..."), column_number = 1L, ranges = list(c(1L, 18L)))`.
- **Prefer native pipe syntax (`|>`) in test snippets:** In unit test code snippets and code examples, prefer R 4.1+ native pipe syntax (`|>`) over `magrittr` pipe syntax (`%>%`) unless explicitly testing `%>%`-specific linter handling.
- **Assert specific dynamic substitutions across vectorized lint tests (`# lints vectorize`):** When testing that a linter properly vectorizes across multiple distinct violations within a multi-line source expression (`test_that("lints vectorize", ...)`), ensure each check pattern in the `checks` list explicitly asserts the specific dynamic properties or parameter substitutions corresponding to that exact line (`list("nrow = n.*expect_equal", line_number = 2L)` vs `list("dim = d.*expect_identical", line_number = 3L)`). Testing only common substrings or generalized prefixes (`"expect_equal"` vs `"expect_identical"`) fails to verify that vectorized extraction (`shape_function`, `shape_arg_var`) dynamically maps placeholders cleanly to each individual line's AST structure. Include newly added syntactic variants alongside established forms in the vectorized test block.
- **Exhaustive range and column highlighting assertions across syntax variants:** When testing lint highlighting, column numbers, or range bounds (`ranges = list(c(start, end))`), do not limit explicit range assertions to a single syntax variation (e.g. only `data.table` `.N`). Plain string regex matching in `expect_lint(code, "regex", linter)` only validates the message string, not the highlight range bounds. Always include explicit `list(message = ..., column_number = ..., ranges = list(c(...)))` assertions across ALL syntax variations supported by the linter (base R expressions, table row count variants like `nrow()`, `dplyr` functions like `n_distinct()` / `n()`, `data.table` expressions like `uniqueN()` / `.N`, and function calls like `any(duplicated())`).
- **Exact regexes & punctuation precision:** When verifying expected lint messages, carefully ensure check regexes do not contain accidental trailing punctuation or stray characters (e.g. `lint_msg("seq_len(n())", "1:n()")` rather than `lint_msg("seq_len(n())", "1:n(),")`).
- **Deduplicate `lint_msg` helper functions:** When a custom message constructor (e.g. `lint_msg <- function(want, got) rex::rex("Use ", want, " instead of ", got)`) is used across multiple `test_that()` blocks in a test file, define it once at the top of the file rather than copy-pasting it inside multiple test blocks.



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

## 6. Rules Governing `# nocov` and Coverage Patches
- **Exhaust public reachability before annotating:** Never add `# nocov` or `# nocov start/end` to unreached lines until you have verified through public interface boundaries (`lint()`, `lint_package()`, `read_settings()`) that no realistic code pattern, malformed file format, or configuration structure can execute that path.
- **Eliminate dead parameters and impossible branches:** If an unreached branch (`op_xpath <- NULL`) or function parameter (`normalize_path = FALSE` across internal utilities) is never triggered in production, **remove or simplify the dead logic outright** rather than masking it under `# nocov`.
- **Convert impossible states to explicit internal errors if appropriate:** If defensive checks guard against violations of foundational R grammatical rules (such as zero-child `<expr>` AST nodes), raise an intentional internal error (`cli_abort_internal("Invalid state encountered...")`) rather than silently returning empty outputs.

## 7. All tests must use the public API
- **Test unexported helpers exclusively through public entry points:** Even though `{testthat}` injects package private namespaces (`pkgload::load_all()`), always route argument validations and error assertions across public functions (`lint()`, `lint_dir()`, `lint_package()`) to prevent unit tests from coupling directly to internal private mechanics. Don't mention implementation details when describing/commenting tests, either.
- **Use authentic parsed structures over mock lists:** Never generate simplified artificial mock lists (`list(full_parsed_content = 1L)`) to satisfy internal type verification. Always generate real `source_expression` objects using `get_source_expressions()` and extract real configuration files (`write.dcf()`) to verify production structures accurately.

## 8. Robust Filesystem Paths & Cache Hashing on Windows
- **Normalize paths before hashing or comparison:** Whenever writing unit tests that read, corrupt, or assert specific filesystem locations (`get_cache_file_path(file, path)` or temporary file checking), always pass file strings through `normalize_path(file)` before calculating SHA1 digests or checking outputs. Un-normalized paths from `withr::local_tempfile()` retain Windows backslashes (`\`) and unexpanded short segment names (`~1.TMP`), which alter SHA1 digests (`digest::digest(..., algo = "sha1")`) and cause false-positive failures on Windows platforms.

## 9. Empirical Test Case Synthesis & Modular Grouping
- **Distill real-world, empirical usage over abstract toy examples:** When verifying complex functions or new options (`list.files()`, `dir()`, pipelines), research and synthesize actual pattern combinations used across open-source and real-world codebases. Exercise realistic flags (`full.names = TRUE`, `recursive = TRUE`, `ignore.case = TRUE`), piped invocations (`getwd() |> list.files(...)`), dynamic combinations (`paste0(...)`), exact regex anchors (`"csv$"`), and literal string escapes (`"_bmarks\\.csv"` vs `"_bmarks.csv"`).
- **Group assertions into modular, focused `test_that()` blocks:** Avoid stuffing all variations into one monolithic test block. Separate tests into clearly labeled blocks by testing concern:
  1. Standard keyword invocations (`pattern = ...`) and empirical configuration options.
  2. Positional parameter lookups and positional pipelines (`list.files("path", "pattern")` and `"dir" |> list.files("pattern")`).
  3. Option toggles and behavior overrides (`check_file_listing = FALSE`).
- **Comprehensive test matrix for linter extensions:** Whenever extending a linter to check new functions or syntax variations (e.g. 2-argument `seq()`), systematically test all dimensions of the empirical matrix:
  1. **Positional arguments:** `seq(1, 10)`
  2. **Standard named arguments:** `seq(from = 1, to = 10)`
  3. **Inverted named arguments:** `seq(to = 10, from = 1)`
  4. **Mixed positional/named arguments:** `seq(1, to = 10)`, `seq(to = 10, 1)`, `seq(10, from = 1)`, `seq(from = 1, 10)`
  5. **Literal types:** `1L`, `10L`, `1`
  6. **Boundary / single-element calls:** `seq(1, 1)`, `seq(from = 1, to = 1)`
  7. **Decreasing sequence calls:** `seq(10, 1)`, `seq(from = 10, to = 1)`, `seq(to = 1, from = 10)`, `seq(n, 1)`, `seq(length(x), 1)`, `seq(nrow(x), 1)`
  8. **Excluded / non-lintable negative cases:** `seq(0, 10)`, `seq(0, 1)`, `seq(-1, 10)`, `seq(-1, 1)`, `seq(2, 10)`, `seq(from = 2, to = 10)`
  9. **Extra argument exclusions:** `seq(1, 10, by = 2)`, `seq(1, 10, length.out = 5)`, `seq(1, 10, along.with = x)`
  10. **Namespace-prefixed calls:** `base::seq(1, 10)`, `dplyr::n()`
  11. **Multi-line expressions with comments:** `seq(1, # comment\n 10)`
  12. **Vectorization tests:** combining new and existing syntactic variants inside `test_that("lints vectorize", ...)`.
- **Verify non-targeted functions remain WAI when toggling options:** When testing an option that disables checking for specific targets (like passing `check_file_listing = FALSE` to skip `list.files()`/`dir()`), explicitly include positive assertions proving that **all other positional or named target functions monitored by the linter (`grepl()`, `str_detect()`, `strsplit()`) continue functioning exactly as expected (working-as-intended/WAI)**.

## 10. Failing Regression Tests for Reported Bugs & Review Findings
- **No failing test = no bug claim:** In all but exceptional cases, any claim of a bug, unhandled warning, unhandled edge case, parser crash, or regression in a PR or issue must be substantiated by a concrete, executable regression test (e.g. `expect_lint(...)`, `expect_no_lint(...)`, or a standalone test snippet) that fails on the current code and succeeds with the proposed fix.
- **Empirical proof over theoretical hazards:** Do not claim a function or chunk option is vulnerable to unhandled conditions (e.g. warnings, invalid vector lengths, type coercion errors) without first writing a reprex that actually triggers the failure in an R session. If no valid or invalid input can trigger the issue, the hazard is theoretical and must not be reported as a defect.
- **Prohibit inventing artificial syntax symmetries:** When writing or suggesting tests for literate formats or external tool integrations (`knitr`, `quarto`), tests must exclusively assert valid, documented, and empirically verified upstream behaviors. Never suggest or write tests based on theoretical symmetry (such as ````{python, engine = "r"}````) without verifying upstream execution (e.g. via `knitr::knit()`).
