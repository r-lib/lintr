---
name: developing-linters
description: Guidelines and best practices for creating, extending, and refactoring linters in the r-lib/lintr package. Use this skill whenever implementing or modifying linter functions, XPath queries, and linter helpers.
---

# Developing Linters in `r-lib/lintr`

When implementing, extending, or refactoring linters in `lintr`, adhere to the following architectural, design, and production-readiness guidelines established by project maintainers:

## 1. Minimalist API Surface & Defaults ("On by Default")
- **Avoid unnecessary configuration knobs:** When extending a linter with a new check (e.g., adding namespace import checking to `namespace_linter`), do not reflexively add new boolean parameters (like `check_imports = TRUE`) to the public function signature unless there is a clear, compelling user need to toggle only that specific sub-check independently.
- **Bleeding-edge upstream R features (`TODO(R>=x.y.z)`):** When recommending new syntax or parameters introduced in recent or upcoming upstream R releases (`fixed = TRUE` added to `list.files()` in R 4.6.0), package authors may not wish to immediately incur that bleeding-edge dependency. In such cases, providing a toggle (`check_file_listing = TRUE`) allows authors supporting older R versions to opt out (`FALSE`). Always mark such transitionary options or workarounds with an explicit `TODO(R>=x.y.z)` comment right above the function signature (`# TODO(R>=4.6.0): Deprecate check_file_listing once R 4.6.0 is the minimum supported version.`) indicating exactly when the option should be deprecated and enforced universally.
- **Natural zero-lint fallback:** If a check naturally produces zero lints when not applicable (e.g., inspecting `namespace_imports()` on a file outside an R package or in a directory without a `NAMESPACE` file returns `empty_namespace_data()`), run the check unconditionally as part of the standard linter flow.
- **Keep signatures clean:** Preserving existing function signatures (`namespace_linter(check_exports = TRUE, check_nonexports = TRUE)`) reduces API complexity and documentation churn.

## 2. Helper Functions, Readability, & Safe Fallbacks
- **Encapsulating non-trivial logic in helper functions is encouraged:** While trivial single-use wrappers purely around basic line lookups or single `vapply(...)` steps should generally be avoided if they merely fragment linear control flow, **helper functions that encapsulate non-trivial logic (such as `build_line_metadata()` or `indent_lint_metadata()`) are highly valued, even if invoked only once inside a linter callback.** The immense readability improvement and clear domain encapsulation far outweigh their single-use nature.
- **Early returns on domain empty states:** Check for domain empty states early and exit (`if (nrow(ns_imports) == 0L) return(lints)` or `if (nrow(lint_line_df) == 0L) return(list())`) rather than executing checks over empty data structures.
- **Avoid distracting checks for 0-line files:** Do not recommend or implement defensive guards for truly empty zero-line files (`if (length(source_expression$file_lines) == 0L)`). Encountering a 0-line file in practical active extraction and parsing is virtually impossible; suggesting such checks during code reviews creates unnecessary distraction and code clutter.
- **Rely on existing safe fallbacks:** Do not write defensive wrappers around functions that already handle `NULL` cleanly. For example, `namespace_imports(NULL)` safely returns `empty_namespace_data()`, so `if (!is.null(pkg_path)) namespace_imports(pkg_path)` is redundant.
- **Use numbered format specifiers (`%1$s`, `%2$s`) for repeated variables in `sprintf()`:** When formatting dynamic lint messages where placeholders are repeated (e.g., displaying both the recommended replacement and the observed violation using the exact same function name and parameter string), use numbered format specifiers (`%1$s`, `%2$s`, `%3$s`). This cleanly avoids redundant argument repetitions across the `sprintf()` call (`sprintf("expect_shape(x, %1$s = %2$s) is better than %3$s(%1$s(x), %2$s)", shape_function, shape_arg_var, matched_function)` over repeating `shape_function, shape_arg_var` twice).

## 3. Minimal Diffs & Logical Execution Order
- **Structure execution to avoid intermediate mutations:** Structure the execution order of sub-checks within a linter to avoid mutating, subsetting, or filtering shared XML node lists and symbol vectors midway through the function.
- **Inline condition-dependent vector additions over intermediate state:** When conditionally appending items to a list or character vector, inline the check right inside `c()` (`c("strsplit", if (check_file_listing) c("dir", "list.files"))`) instead of assigning an intermediate temporary variable. Because `if (FALSE)` evaluates to `NULL` (which automatically drops out of `c()`), inlining cleanly eliminates unnecessary temporary variable assignments.
- **Append new checks cleanly:** When adding a check to an existing linter (`check_exports`, `check_nonexports`), place the new check cleanly after existing checks so that existing code blocks and variables (`packages`, `symbols`, `ns_nodes`) remain untouched. This keeps diffs small, readable, and easy to review.

## 4. Self-Linting, Repository Health, & `NEWS.md`
- **Verify zero new violations and dogfood new linters across `lintr` itself:** Whenever a linter is made more strict or a brand-new linter is created, immediately run it across `lintr`'s own repository (`R/` codebase and `tests/testthat/` suites). Dogfood the new checks and clean up any newly triggered violations across existing package code and tests before proposing the PR. For example, for `expect_shape_linter()`, some existing usages of `expect_identical(nrow(x), n)` were updated to `expect_shape(x, nrow = n)` in the same PR.
- **Keep `lintr` 100% lint-free:** Immediately clean up any newly triggered violations across the repository (e.g., changing redundant `glue::glue()` or `cli::cli_warn()` calls to `glue()` and `cli_warn()`) before proposing the PR.
- **Descriptive VCS branch / bookmark naming:** Avoid non-descriptive, issue-number-only branch names like `fix-issue-NNNN`. Always choose short, descriptive branch or bookmark names that clearly convey the functional work or feature being developed (e.g. `any-duplicated-highlight-range`, `unreachable-switch`, `vector-logic-body`).
- **`NEWS.md` entry scope:** Internal range/span highlighting fixes for existing linters (which only adjust where a lint lands without altering public functions, parameters, or lint messages) do **not** require a `NEWS.md` entry. Reserve `NEWS.md` entries strictly for new features, new linters, deprecations, breaking changes, or user-facing parameter/message additions.
- **Categorize `NEWS.md` entries under hierarchical section headings:** Whenever implementing a new user-facing linter enhancement, parameter option, brand-new linter, or significant bug fix, always append a concise, well-formatted entry right under `# lintr (in development)` in `NEWS.md`. Ensure entries are nested cleanly under level 2 (`##`) and level 3 (`###`) section headings matching standard hierarchy (`## New and improved features` -> `### New linters` or `### Linter improvements`; or `## Bug fixes`). Never insert uncategorized top-level bullet points straight beneath `# lintr (in development)`.


## 5. Respecting Contract Boundaries & Avoiding Unexported Internals (`:::`)
- **Strictly respect contract boundaries:** Take seriously that `:::` means *private* and avoid violating contract boundaries across packages. While `lintr` provides `%:::%` (`p %:::% f`) to bypass self-lint checks when calling private functions, reaching across package boundaries into unexported internals (`knitr:::parse_params`, `knitr:::file_ext`) should be avoided except in rare, justified cases.
- **Look under the hood for exported alternatives:** When tempted to invoke a private internal function from another package, look under the hood first—very often those internals are simple wrappers around `base` R functions or exported utilities from imported dependencies (such as `xfun::csv_options()`, `xfun::divide_chunk()`, or `xfun::file_ext()`).
- **Pragmatic ~95% correctness over fragile 100% correctness:** Even if private upstream helpers include extra edge-case handling or nuance, choose to ignore that nuance for `lintr`'s purposes. `lintr` checks do not have to be demandingly and globally 100% correct in every obscure scenario; achieving ~95% practical correctness using clean, stable, exported APIs is far superior. If a user ever complains about a missing edge case in the future, the implementation can be refined at that time.

## 6. Simple & Idiomatic AST and Condition Checks
- **Avoid over-engineered evaluation constructs:** When checking parsed parameter values or AST expressions (such as `eval` options from chunk headers), do not write complex, over-defensive constructs like `tryCatch(eval(..., envir = baseenv()))` to handle theoretical runtime expressions.
- **Check exact parser representations:** Inspect and match the exact R objects produced by the parser (`xfun::csv_options()` produces `logical` `FALSE` for `eval=FALSE` and symbol `quote(F)` for `eval=F`). A direct, concise check such as `if (identical(eval_value, quote(F))) return(TRUE)` followed by `isFALSE(eval_value)` is simpler, safer, and much easier to maintain.
- **Annotate symbol checks for self-linting:** When comparing against `quote(F)` or `quote(T)`, add `# nolint next: T_and_F_symbol_linter.` immediately above the line to prevent `lintr`'s self-linting checks from flagging the symbol name.

## 7. Data Engineering, Centralized State, & Readability First
- **Use cohesive data frames (`line_metadata`):** When computing many parallel line-by-line properties (current indentation, expected indentation, whether a line is inside a string constant, etc.), structure intermediate objects as a single `line_metadata` `data.frame`. This enforces parallel alignment and makes code far cleaner to inspect or pass to dedicated helpers.
- **Prioritize `with()` to eliminate repetitive visual noise:** When writing compound logical filtering conditions over multi-column metadata frames (`find_bad_lines()`), embrace `with(line_metadata, !is.na(line) & indent_level != expected_level & !in_str_const)` over repetitive `$` accesses (`line_metadata$line`, `line_metadata$indent_level`). The immense readability benefit of eliminating repetitive visual noise far outweighs negligible environment evaluation overhead.

## 8. Idiomatic R Vectorization over Loops
- **Prefer vectorized operations:** Avoid using `for` loops to iterate over lines or XML nodes if a vectorized alternative exists. Use `Map()`, `vapply()`, or logical subsetting.
- **Vectorized sequence generation:** For example, to generate a sequence of indices to mark as inside a string constant:
  ```r
  line1 <- as.integer(xml_attr_(multiline_strings, "line1"))
  line2 <- as.integer(xml_attr_(multiline_strings, "line2"))
  is_in_str <- unlist(Map(`:`, line1, line2))
  in_str_const[is_in_str] <- TRUE
  ```
  This is much cleaner and faster than a `for (string in multiline_strings)` loop.

## 9. Robust Handling of Literate R Formats & Readable Transitions
- **Expect `NA_character_` in line content:** When linting files, remember that literate programming formats (like `.Rmd`, `.qmd`) extract R code by masking non-R lines with `NA_character_` to preserve line numbers.
- **Avoid NA propagation in logical expressions:** Ensure logical expressions checking conditions over lines immediately guard against `NA` (`!is.na(line) & ...`) to keep resulting booleans clean (`FALSE & NA -> FALSE`).
- **Prioritize clear consecutive logic (`diff()`) over micro-optimizations:** Because `is_bad` evaluates to `FALSE` (not `NA`) across non-code gaps, computing consecutive state differences using simple arithmetic (`diff(is_bad) == 0L`) cleanly isolates block transitions across gaps. **Never replace clear, readable difference computations with obscure logical shifting constructs solely to avoid implicit conversions or save negligible compute.** Readability is paramount.

## 10. Direct Documentation Workflow & `roxygen2` Example Style
- **Crisp parameter descriptions:** Keep `@param` documentation for linter configuration knobs concise and precise (e.g., `Logical, default \code{TRUE}, governing whether to require \code{\link[=list.files]{list.files()}} and \code{\link[=dir]{dir()}} to use \code{fixed = TRUE} for non-regex \verb{pattern=}.`), avoiding overly lengthy background explanations.
- **Roxygen example structure (`@examples`) for new or extended linters:** When creating or extending a linter, adhere strictly to `lintr`'s standard roxygen examples layout:
  1. **Section Division (`# will produce lints` and `# okay`):** Always partition `@examples` into distinct sections beginning with `# will produce lints` at top, directly followed by `# okay`.
  2. **Explicit `lint()` calls:** Format each demonstration cleanly across lines using `lint(text = ..., linters = ...)`:
     ```r
     #' lint(
     #'   text = "list.files(pattern = 'RDS')",
     #'   linters = fixed_regex_linter()
     #' )
     ```
  3. **Multi-line or escaped code (`writeLines`):** For simple single-line invocations without complex escapes, pass `text = "..."` directly inside `lint()`. For multi-line snippets (containing `\n`) or regex patterns containing heavy backslash escaping (`"\\\\."`), assign the target code to `code_lines <- "..."`, display it first using `writeLines(code_lines)`, and then pass `text = code_lines` to `lint()`. This allows users executing help examples to view the rendered target code exactly as seen by the parser:
     ```r
     #' code_lines <- 'gsub("\\\\.", "", x)'
     #' writeLines(code_lines)
     #' lint(
     #'   text = code_lines,
     #'   linters = fixed_regex_linter()
     #' )
     ```
  4. **Mirrored 1:1 problem-to-solution pairs:** Maintain 1:1 structural correspondence between cases under `# will produce lints` and their corresponding resolutions right under `# okay` so users clearly observe both the targeted violation and exactly how to fix or configure it (e.g. passing `fixed = TRUE` or adjusting options like `check_file_listing = TRUE`).
  5. **Standard footer tags:** Always conclude linter roxygen blocks right after `@examples` with `@evalRd rd_tags("<linter_name>")`, `@seealso [linters] for a complete list of linters available in lintr.`, and `@export`.
