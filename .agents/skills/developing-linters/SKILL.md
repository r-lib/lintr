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
