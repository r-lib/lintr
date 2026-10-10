---
name: testing-linters
description: >-
  Guidelines and conventions for writing unit tests for linters in the
  r-lib/lintr package using `testthat`. Use whenever creating or modifying test
  files under `tests/testthat/test-*.R`. Don't use for implementing linter R
  logic (see `developing-linters`) or writing XPath queries (see `xpath-style`).
---

# Testing Linters in `r-lib/lintr`

When writing or updating unit tests in `tests/testthat/test-*.R`, adhere to the
following testing conventions and production-readiness standards:

## 1. Constructing Package & File Fixtures (`withr`, `write.dcf`, `writeLines`)

- **Create and populate single-file tempfiles in one step (`withr::local_tempfile`)**:
  Use `withr::local_tempfile(fileext = ".Rmd", lines = c(...))` to create and
  populate a temporary file cleanly. When passing multiple lines to `lines = c(...)`,
  place each line element on its own line:
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
- **Use `write.dcf()` for `DESCRIPTION` files**: When creating temporary package
  structures (e.g., using `withr::local_tempdir()`), always construct
  `DESCRIPTION` files using base R's `write.dcf()` rather than manual string
  concatenation:
  ```r
  write.dcf(
    list(Package = "testpkg", Version = "1.0.0"),
    file.path(pkg_dir, "DESCRIPTION")
  )
  ```
- **Pass character vectors to `writeLines()`**: Never pass newline-delimited
  single strings (`"line1\nline2\n"`) to `writeLines()`, as `writeLines()`
  appends its own trailing newline and expects a character vector where each
  element is a line:
  ```r
  writeLines(
    c("importFrom(stats, median)", "importFrom(utils, head)"),
    file.path(pkg_dir, "NAMESPACE")
  )
  ```

## 2. Fuzz Testing & Syntax Permutations (`# nofuzz`)

- **Isolate `# nofuzz` tests in dedicated `test_that()` blocks**:
  `lintr`'s automated fuzz-testing harness (`.dev/ast_fuzz_test.R`) mutates and
  runs `expect_lint()` / `expect_no_lint()` expressions across equivalent R
  syntax (`<-` vs `=`, `function()` vs `\()`, comment injection).
  - Tests that create temporary directories (`withr::local_tempdir()`),
    multi-file package fixtures (`DESCRIPTION`, `NAMESPACE`), or test
    precedence-sensitive syntax that intentionally changes meaning under a
    fuzzer (such as chained assignment `a <- time(x) = 1` under
    `# nofuzz: assignment`) must be annotated with `# nofuzz` or
    `# nofuzz: <fuzzer>`.
  - Group assertions requiring `# nofuzz` into their own dedicated
    `test_that("...", { # nofuzz ... })` block rather than scattering `# nofuzz`
    comments across standard test blocks:
    ```r
    test_that("namespace_linter detects functions already imported in NAMESPACE", { # nofuzz
      pkg_dir <- withr::local_tempdir("testpkg")
      ...
    })
    ```
- **Trust the fuzzing suite for standard syntax permutations**: Because the
  automated fuzzing harness exercises interchangeable R syntax structures across
  tests (such as mutating `function(...)` definitions into lambda shorthand
  `\(...)`), **do not write separate positive/negative unit tests for
  interchangeable syntax variations** (`\()` vs `function()`) unless the
  individual linter implementation has custom code explicitly handling
  `OP-LAMBDA` vs `FUNCTION` differently.

## 3. Writing `expect_lint()` & `expect_no_lint()` Assertions

- **Separate distinct cases into individual `expect_no_lint()` calls**:
  Write one `expect_no_lint()` call per distinct expression or scenario. Do not
  stuff multiple unrelated expressions into a single multi-line `trim_some()`
  block inside `expect_no_lint()`.
- **Always use `trim_some()` for multi-line code snippets**:
  Never write inline `\n` escapes for multi-line R code (such as
  `"if (a ||\n  b) {\n  1\n}"`). Always format multi-line code with
  `trim_some()`.
- **Quote & raw-string ergonomics (`R"(...)"` / `R'{...}'` and `'...'`)**:
  - Use raw string literals (`R"(...)"` or `R'{...}'`) whenever the tested code
    string genuinely requires backslash escaping (`\(x)` lambdas, `\n`, `\\.`,
    `\1`, etc.) so you never write `\\(`. Do not reflexively wrap clean,
    unescaped strings (`'list.files("foo")'`) in raw strings.
  - Prefer outer single quotes `trim_some('...')` when the inner tested code
    contains double quotes `"..."` (and no single quotes) to avoid `\"`
    escaping.
- **Explicit argument names when testing files**: When asserting lints against a
  file path (`file = test_file`), explicitly name the `file`, `checks`, and
  `linters` arguments rather than relying on positional ordering (passing a file
  path positionally treats the path string itself as R code):
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
- **Avoid `rex::rex()` around plain strings to reduce visual noise**:
  `expect_lint()` natively interprets check strings as regular expressions. Do
  not wrap plain target strings in `rex::rex("...")` when no regex
  metacharacters require escaping (`()`, `[]`, `.`, `$`, etc.) or composition.
  Do use `rex::rex()` when escaping regex specials (especially repeated `\\`)
  would otherwise add visual noise.
- **Avoid repetitive assertion comments**: Group related sequential assertions
  under a single high-level summary comment (e.g.,
  `# position= inferred positionally`) instead of repeating comments above every
  `expect_lint()` or `expect_no_lint()` call.
- **Positional message parameter in `expect_lint()` check lists**: In
  `expect_lint()` check lists, pass the message regex or string positionally as
  the first element of the check list rather than naming `message =`:
  `list(rex::rex("..."), line_number = 2L, column_number = 1L, ranges = list(c(1L, 18L)))`.
  Always include `line_number` when a multi-line snippet could match on more
  than one line.
- **Prefer native pipe syntax (`|>`) in test snippets**: Prefer R 4.1+ native
  pipe syntax (`|>`) over `magrittr` pipe syntax (`%>%`) unless explicitly
  testing `%>%`-specific linter handling.
- **Pair getter and setter (`foo()` vs `foo<-`) assertions side-by-side**:
  When testing linters that inspect or distinguish replacement functions
  (`foo(x) <- 1`), pair the getter and setter assertions immediately next to
  each other for each syntax variant rather than grouping all getter tests
  followed by all setter tests. Use backticked names ``c(`time<-` = ...)`` over
  quoted `"time<-"` in R argument calls.
- **Assert specific dynamic substitutions across vectorized lint tests (`# lints vectorize`)**:
  In `test_that("lints vectorize", ...)`, ensure each check pattern in `checks`
  explicitly asserts the dynamic substitutions for that specific line
  (`list("nrow = n.*expect_equal", line_number = 2L)` vs
  `list("dim = d.*expect_identical", line_number = 3L)`), and include newly
  added syntactic variants in the vectorized block.
- **Exhaustive range and column highlighting assertions across syntax variants**:
  When testing lint highlighting, column numbers, or range bounds
  (`ranges = list(c(start, end))`), include explicit `column_number` and
  `ranges` assertions across all syntax variations supported by the linter (base
  R expressions, `nrow()`, `dplyr::n_distinct()` / `n()`, `data.table`
  `uniqueN()` / `.N`, `any(duplicated())`, etc.).
- **Exact regexes & punctuation precision**: Ensure check regexes do not contain
  accidental trailing punctuation or stray characters.
- **Deduplicate `lint_msg` helper functions**: When a custom message helper
  (`lint_msg <- function(want, got) rex::rex("Use ", want, " instead of ", got)`)
  is used across multiple `test_that()` blocks in a test file, define it once at
  the top of the file.

## 4. Testing Document Structure & Literate Formats

- **Keep literate (`.Rmd`/`.qmd`) boundary tests out of individual linter suites**:
  Tests targeting literate formats, `NA_character_` masked line extraction,
  zero-chunk, or multi-chunk boundary parsing belong in
  `tests/testthat/test-knitr_formats.R` (see
  [`literate-r-formats`](../literate-r-formats/SKILL.md)), not in individual
  `tests/testthat/test-<linter_name>.R` files, unless the linter executes custom
  format-specific logic directly dependent on those extensions.

## 5. Rules Governing `# nocov` and Coverage Patches

- **Exhaust public reachability before annotating**: Never add `# nocov` or
  `# nocov start/end` to unreached lines until you have verified through public
  interface boundaries (`lint()`, `lint_package()`, `read_settings()`) that no
  realistic code pattern, malformed file format, or configuration structure can
  execute that path.
- **Eliminate dead parameters and impossible branches**: If an unreached branch
  or internal parameter is never triggered in production, **remove or simplify
  the dead logic outright** rather than masking it under `# nocov`.
- **Convert impossible states to explicit internal errors if appropriate**: If
  defensive checks guard against violations of foundational R grammatical rules
  (such as zero-child `<expr>` AST nodes), raise an intentional internal error
  (`cli_abort_internal("Invalid state encountered...")`) rather than silently
  returning empty outputs.

## 6. All Tests Must Use the Public API

- **Test unexported helpers exclusively through public entry points**: Always
  route argument validations and error assertions across public functions
  (`lint()`, `lint_dir()`, `lint_package()`) to prevent unit tests from coupling
  directly to internal private mechanics. Do not mention internal implementation
  details when describing/commenting tests.
- **Use authentic parsed structures over mock lists**: Never generate simplified
  artificial mock lists (`list(full_parsed_content = 1L)`) to satisfy internal
  type verification. Always generate real `source_expression` objects using
  `get_source_expressions()` and real configuration files (`write.dcf()`).

## 7. Robust Filesystem Paths & Cache Hashing on Windows

- **Normalize paths before hashing or comparison**: Whenever writing unit tests
  that read, corrupt, or assert specific filesystem locations
  (`get_cache_file_path(file, path)` or temporary file checking), always pass
  file strings through `normalize_path(file)` before calculating SHA1 digests or
  checking outputs so Windows backslashes (`\`) and short 8.3 names (`~1.TMP`)
  do not cause false failures on Windows CI.

## 8. Empirical Test Case Synthesis & Modular Grouping

- **Distill real-world, empirical usage over abstract toy examples**: Exercise
  realistic flags (`full.names = TRUE`, `recursive = TRUE`,
  `ignore.case = TRUE`), piped invocations (`getwd() |> list.files(...)`),
  dynamic combinations (`paste0(...)`), exact regex anchors (`"csv$"`), and
  literal string escapes (`"_bmarks\\.csv"` vs `"_bmarks.csv"`).
- **Group assertions into modular, focused `test_that()` blocks**: Separate
  tests into clearly labeled blocks by testing concern (standard keyword
  invocations, positional parameter lookups & pipelines, option toggles, and
  `# nofuzz` edge cases).
- **Comprehensive test matrix for linter extensions**: Whenever extending a
  linter to check new functions or syntax variations (e.g., 2-argument `seq()`),
  systematically test all dimensions of the empirical matrix:
  1. **Positional arguments**: `seq(1, 10)`
  2. **Standard named arguments**: `seq(from = 1, to = 10)`
  3. **Inverted named arguments**: `seq(to = 10, from = 1)`
  4. **Mixed positional/named arguments**: `seq(1, to = 10)`, `seq(to = 10, 1)`,
     `seq(10, from = 1)`, `seq(from = 1, 10)`
  5. **Literal types**: `1L`, `10L`, `1`
  6. **Boundary / single-element calls**: `seq(1, 1)`, `seq(from = 1, to = 1)`
  7. **Decreasing sequence calls**: `seq(10, 1)`, `seq(from = 10, to = 1)`,
     `seq(to = 1, from = 10)`, `seq(n, 1)`, `seq(length(x), 1)`,
     `seq(nrow(x), 1)`
  8. **Excluded / non-lintable negative cases**: `seq(0, 10)`, `seq(0, 1)`,
     `seq(-1, 10)`, `seq(-1, 1)`, `seq(2, 10)`, `seq(from = 2, to = 10)`
  9. **Extra argument exclusions**: `seq(1, 10, by = 2)`,
     `seq(1, 10, length.out = 5)`, `seq(1, 10, along.with = x)`
  10. **Namespace-prefixed calls**: `base::seq(1, 10)`, `dplyr::n()`
  11. **Multi-line expressions with comments**: `seq(1, # comment\n 10)`
  12. **Vectorization tests**: combining new and existing syntactic variants
      inside `test_that("lints vectorize", ...)`.
- **Verify non-targeted functions remain WAI when toggling options**: When
  testing an option that disables checking for specific targets (like passing
  `check_file_listing = FALSE` to skip `list.files()`/`dir()`), explicitly
  include positive assertions proving that all other target functions monitored
  by the linter (`grepl()`, `str_detect()`, `strsplit()`) continue functioning
  as working-as-intended (WAI).
- **R version availability guards**: When testing functions or S3 generics added
  or changed in a specific R version (e.g., `%*%` becoming an S3 generic in R
  4.3.0), guard with `skip_if_not_r_version("<x.y.z>")`.

## 9. Failing Regression Tests for Reported Bugs & Review Findings

- **No failing test = no bug claim**: In all but exceptional cases, any claim of
  a bug, unhandled warning, unhandled edge case, parser crash, or regression in
  a PR or issue must be substantiated by a concrete, executable regression test
  that fails on the current code and succeeds with the proposed fix.
- **Empirical proof over theoretical hazards**: Do not claim a function or chunk
  option is vulnerable to unhandled conditions without first writing a reprex
  that triggers the failure in an R session.
- **Prohibit inventing artificial syntax symmetries**: When writing or
  suggesting tests for literate formats or external tool integrations (`knitr`,
  `quarto`), tests must exclusively assert valid, documented, and empirically
  verified upstream behaviors (e.g., verify with `knitr::knit()`).
