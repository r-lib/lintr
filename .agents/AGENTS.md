# `{lintr}` Repository Agent Guidelines

## 1. Specialized Skills & Subagents

Load the relevant domain skill from `.agents/skills/` before starting work:

- [`developing-linters`](skills/developing-linters/SKILL.md): Creating,
  extending, or refactoring linters, helper design, and roxygen `@examples`.
- [`xpath-style`](skills/xpath-style/SKILL.md): Writing, composing, optimizing,
  and formatting XPath 1.0 queries on R parse trees (`xmlparsedata`).
- [`testing-linters`](skills/testing-linters/SKILL.md): Writing `testthat` unit
  tests with `expect_lint()`, `expect_no_lint()`, `trim_some()`, and `# nofuzz`.
- [`literate-r-formats`](skills/literate-r-formats/SKILL.md): Modifying
  `R/extract.R` or handling `.Rmd`, `.qmd`, `.Rnw`, `.Rhtml`, `.Rtex`, `.Rrst`,
  and `.Rtxt` files.

### PR, Branch & Commit-Stack Reviews

- **Always delegate reviews to [`pr-reviewer`](agents/pr-reviewer.md)**:
  Whenever asked to review a PR (including a bare GitHub PR URL), branch, commit,
  or commit chain, delegate to the `pr-reviewer` subagent rather than reviewing
  inline.
- **Multi-reviewer fan-out**: When asked to launch multiple (3–5+) reviewer
  subagents in parallel on a branch or commit chain, assign each subagent a
  distinct focus angle to avoid redundant findings:
  1. **XPath & AST edge cases**: `xpath-style` compliance, operator precedence,
     `<-` vs `=`, `function()` vs `\()`, `$` vs `@`, and injected comments.
  2. **R runtime, caching & helper reuse**: `get_source_expressions()` caching,
     `get_r_string()`, `xml_find_function_calls()`, `xp_is_setter_call()`, and
     avoiding redundant XML traversals.
  3. **Test completeness & fuzzing**: `testing-linters` style, metadata
     assertions, getter/setter parity (`foo()` vs `foo<-`), `# nofuzz` isolation,
     and R version guards.
  4. **API design, docs, `NEWS.md` & stack hygiene**: Function signatures,
     roxygen docs, `NEWS.md` formatting, and commit-chain atomicity (ensuring
     opportunistic refactors are in separate commits from feature/bugfix work).

---

## 2. `{lintr}` R Code Conventions

- **Explicit logical comparisons in `if ()`**:
  Always write `if (length(x) == 0L)` or `if (nrow(x) > 0L)`. Never rely on
  implicit integer truthiness such as `if (!length(x))` or `if (length(x))`.
- **Function assignment**:
  Always use `<-` (never `=`) when assigning functions
  (`helper <- function(x) ...`), including local helpers inside another function.
- **Environment & namespace lookups**:
  - Use `get0(key, envir = ..., inherits = FALSE)` instead of
    `if (exists(...)) get(...)`.
  - Avoid `delayedAssign()` when the value is evaluated in normal multi-linter
    runs. When `delayedAssign()` is justified for a rarely accessed cache (such
    as `s4_slot_cache`), include an inline comment explaining why.
  - Push type/precondition guards (`if (!is.function(fun)) {\n  return(FALSE)\n}`)
    inside helper functions rather than repeating them at call sites.
  - When an upstream function is used several times across the package (e.g.,
    `stats::setNames`), import it via `@importFrom` in `R/lintr-package.R` and
    run `roxygen2::roxygenize()` rather than repeating `pkg::fun`.
- **Self-linting & dogfooding (`.lintr.R`)**:
  - Keep `{lintr}` 100% lint-free. Whenever a linter is added or tightened,
    dogfood it across `R/` and `tests/testthat/` and fix any newly triggered
    violations in the same change.
  - Never write contorted code or `assign(..., envir = parent.frame())` just to
    bypass `cyclocomp_linter()` or `object_usage_linter()`. Extract a named
    helper function or store state on an existing environment.
- **VCS branch / bookmark & commit naming**:
  - Choose short, descriptive bookmark names that convey the feature or fix
    (e.g., `any-duplicated-highlight-range`, `unreachable-switch`,
    `indent-method-chain`), never generic `fix-issue-NNNN`.

---

## 3. Core AST & Source Expression Utilities

Reuse `{lintr}`'s built-in extraction and caching helpers rather than writing
ad-hoc string or XML manipulation:

- **`get_r_string(node, xpath = NULL)` (`R/utils.R`)**:
  Extracts R string values or symbol names from XML nodes. It handles
  `STR_CONST` (including raw strings `R"(...)"` and escapes) as well as
  backtick-quoted `SYMBOL`, `SYMBOL_SUB`, `SYMBOL_FORMALS`,
  `SYMBOL_FUNCTION_CALL`, and `SLOT`. Never write manual `gsub("^`|`$", ...)`,
  `strip_quotes()`, or custom unquoting tables.
- **`source_expression$xml_find_function_calls(function_names, keep_names = ...)` (`R/get_source_expressions.R`)**:
  Returns pre-cached `//SYMBOL_FUNCTION_CALL/parent::expr` nodes.
  - Pass `keep_names = TRUE` when matching dynamic or user-supplied function
    names not pre-listed in `R/get_source_expressions.R`.
  - The cache automatically resolves replacement/setter calls (`foo(x) <- 1` and
    `x |> foo() <- 1` are indexed under `"foo<-"`) and unquotes backticked or
    string-literal function calls.
- **`xp_is_setter_call()` (`R/xpath_utils.R`) & `xml_find_lgl_()` (`R/xml_Wrapper.R`)**:
  Use `xp_is_setter_call()` in XPath or `xml_find_lgl_()` to test node
  predicates. Never compare `xml2` nodesets using `nodes1 %in% nodes2`.

---

## 4. `NEWS.md` Conventions

- **When a `NEWS.md` entry is needed**:
  Add an entry under `# lintr (in development)` for new linters, user-facing
  enhancements, new parameters/messages, deprecations, breaking changes, or
  user-visible bug fixes. Internal range/span highlighting tweaks or test-only
  changes do **not** require a `NEWS.md` entry.
- **Section hierarchy**:
  Nest entries under the standard level-2 (`##`) and level-3 (`###`) headings
  (e.g., `## New and improved features` -> `### New linters` or
  `### Linter improvements`; or `## Bug fixes`). Never place uncategorized
  top-level bullets directly under `# lintr (in development)`.
- **Group multiple changes to the same function**:
  When more than one bullet applies to the same linter or function (e.g.,
  `get_source_expressions():`, `lint():`, `indentation_linter():`,
  `seq_linter():`), group them under a single parent bullet
  ``* `func_name()`:`` with indented `+` sub-bullets.
- **Attribution**:
  Include the issue number `#XXXX` or PR number `#YYYY` (if no issue yet exists)
  alongside the author's public GitHub handle (e.g., `@MichaelChirico`,
  `@AshesITR`): `(#1234, #5678, @username)`.
- **Formatting names**:
  Wrap R package names in braces (`{styler}`, `{cli}`, `{xml2}`) and non-R CLI
  tools or external programs in backticks (`` `air` ``, never `{air}`).
- **Migration guidance**:
  When changing default behavior or linter scope, include a concise sentence
  showing users how to opt in/out or migrate (e.g.,
  ```Use ``undesirable_function_linter(c(foo = NA, `foo<-` = NA))`` to lint both```).

---

## 5. Verification & `.dev/` Reference Scripts

- **Default verification (required on every code change)**:
  1. Run targeted unit tests:
     `Rscript -e "pkgload::load_all(); testthat::test_file('tests/testthat/test-<name>.R')"`
  2. Lint all touched `R/` and `tests/testthat/` files:
     `Rscript -e "pkgload::load_all(); print(lint('R/<name>.R')); print(lint('tests/testthat/test-<name>.R'))"`
  3. If roxygen comments, exports, or imports changed, regenerate docs:
     `Rscript -e "roxygen2::roxygenize()"`
- **Optional `.dev/` CI validation scripts** (run via `callr::rscript(".dev/<script>.R")`):
  - `.dev/unused_helpers_test.R`: Checks that all unexported objects in `R/`
    have internal callers.
  - `.dev/roxygen_test.R`: Verifies `man/*.Rd` output is idempotent across
    locales (`C`, `en_US.utf8`, `hu_HU.utf8`, `ja_JP.utf8`).
  - `.dev/defunct_linters_test.R`: Verifies defunct linter stubs.
  - `.dev/lint_metadata_test.R`: Full-suite check ensuring every linter tests
    lint metadata (`line_number`, `column_number`).
  - `.dev/ast_fuzz_test.R`: Full-suite AST equivalence fuzzer (`<-` vs `=`,
    `function()` vs `\()`, comment injection).
