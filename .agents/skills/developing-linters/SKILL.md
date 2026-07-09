---
name: developing-linters
description: Guidelines and best practices for creating, extending, and refactoring linters in the r-lib/lintr package. Use this skill whenever implementing or modifying linter functions, XPath queries, and linter helpers.
---

# Developing Linters in `r-lib/lintr`

When implementing, extending, or refactoring linters in `lintr`, adhere to the following architectural, design, and production-readiness guidelines established by project maintainers:

## 1. Minimalist API Surface & Defaults ("On by Default")
- **Avoid unnecessary configuration knobs:** When extending a linter with a new check (e.g., adding namespace import checking to `namespace_linter`), do not reflexively add new boolean parameters (like `check_imports = TRUE`) to the public function signature unless there is a clear, compelling user need to toggle only that specific sub-check independently.
- **Natural zero-lint fallback:** If a check naturally produces zero lints when not applicable (e.g., inspecting `namespace_imports()` on a file outside an R package or in a directory without a `NAMESPACE` file returns `empty_namespace_data()`), run the check unconditionally as part of the standard linter flow.
- **Keep signatures clean:** Preserving existing function signatures (`namespace_linter(check_exports = TRUE, check_nonexports = TRUE)`) reduces API complexity and documentation churn.

## 2. Inlining vs. Overkill Helper Functions
- **Avoid single-use wrapper functions:** Do not create private helper functions (e.g., `is_in_imports()`, `build_ns_imports_lints()`) solely to wrap a few lines of `vapply(...)` or `xml_nodes_to_lints(...)` that are only called once inside a linter callback. Inline the logic directly to keep the control flow linear and self-contained.
- **Early returns on empty state:** Always check for empty prerequisites early and exit (`if (nrow(ns_imports) == 0L) return(lints)`) rather than allocating boolean vectors (`rep(FALSE, length(symbols))`) or executing vectorized checks over empty data frames.
- **Rely on existing safe fallbacks:** Do not write defensive wrappers around functions that already handle `NULL` cleanly. For example, `namespace_imports(NULL)` safely returns `empty_namespace_data()`, so `if (!is.null(pkg_path)) namespace_imports(pkg_path)` is redundant.

## 3. Minimal Diffs & Logical Execution Order
- **Structure execution to avoid intermediate mutations:** Structure the execution order of sub-checks within a linter to avoid mutating, subsetting, or filtering shared XML node lists and symbol vectors midway through the function.
- **Append new checks cleanly:** When adding a check to an existing linter (`check_exports`, `check_nonexports`), place the new check cleanly after existing checks so that existing code blocks and variables (`packages`, `symbols`, `ns_nodes`) remain untouched. This keeps diffs small, readable, and easy to review.

## 4. Self-Linting & Repository Health
- **Verify zero new violations in `lintr` itself:** Whenever a linter is made more strict or extended with new rules, run the modified linter across `lintr`'s own `R/` codebase (`R/condition_call_linter.R`, `R/cyclocomp_linter.R`, etc.).
- **Keep `lintr` 100% lint-free:** Immediately clean up any newly triggered violations across the repository (e.g., changing redundant `glue::glue()` or `cli::cli_warn()` calls to `glue()` and `cli_warn()`) before proposing the PR.

## 5. Upstream Exported APIs vs. Unexported Internals (`%:::%`)
- **Prefer exported utilities over `%:::%` calls:** Even though `lintr` provides the internal helper `%:::%` (`p %:::% f`) to access unexported package functions cleanly without violating namespace checks, always prefer exported functions from imported packages (`DESCRIPTION` `Imports:`) whenever available.
- **Eliminate fragile internal wrappers:** For example, when extracting or parsing `knitr` chunk options, use `xfun::csv_options(params_src)` and `xfun::divide_chunk("r", code)$options` instead of calling unexported internals like `("knitr" %:::% "parse_params")` or `("knitr" %:::% "partition_chunk")`. Relying on exported utilities (`xfun::file_ext()`, `xfun::csv_options()`) keeps code robust across dependency updates and avoids unnecessary runtime checks (`if (exists("partition_chunk", ...))`).

## 6. Simple & Idiomatic AST and Condition Checks
- **Avoid over-engineered evaluation constructs:** When checking parsed parameter values or AST expressions (such as `eval` options from chunk headers), do not write complex, over-defensive constructs like `tryCatch(eval(..., envir = baseenv()))` to handle theoretical runtime expressions.
- **Check exact parser representations:** Inspect and match the exact R objects produced by the parser (`xfun::csv_options()` produces `logical` `FALSE` for `eval=FALSE` and symbol `quote(F)` for `eval=F`). A direct, concise check such as `if (identical(eval_value, quote(F))) return(TRUE)` followed by `isFALSE(eval_value)` is simpler, safer, and much easier to maintain.
- **Annotate symbol checks for self-linting:** When comparing against `quote(F)` or `quote(T)`, add `# nolint next: T_and_F_symbol_linter.` immediately above the line to prevent `lintr`'s self-linting checks from flagging the symbol name.
