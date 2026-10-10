---
name: developing-linters
description: >-
  Guidelines and best practices for creating, extending, and refactoring linters
  in the r-lib/lintr package. Use when implementing or modifying linter
  functions (`R/*_linter.R`), linter helpers, or roxygen `@examples`. Don't use
  for XPath query composition details (see `xpath-style`), unit test structure
  (see `testing-linters`), or literate document extraction (see
  `literate-r-formats`).
---

# Developing Linters in `r-lib/lintr`

When implementing, extending, or refactoring linters in `lintr`, follow the
repository-wide rules in [`.agents/AGENTS.md`](../../AGENTS.md) alongside these
architectural, design, and production-readiness guidelines:

## 1. Minimalist API Surface & Defaults ("On by Default")

- **Avoid unnecessary configuration knobs**: When extending a linter with a new
  check (e.g., adding namespace import checking to `namespace_linter`), do not
  reflexively add new boolean parameters (like `check_imports = TRUE`) to the
  public function signature unless there is a clear, compelling user need to
  toggle only that specific sub-check independently.
- **Bleeding-edge upstream R features (`TODO(R>=x.y.z)`)**: When recommending
  new syntax or parameters introduced in recent or upcoming upstream R releases
  (`fixed = TRUE` added to `list.files()` in R 4.6.0), package authors may not
  wish to immediately incur that bleeding-edge dependency. Providing a toggle
  (`check_file_listing = TRUE`) allows authors supporting older R versions to
  opt out (`FALSE`). Always mark such transitionary options or workarounds with
  an explicit `TODO(R>=x.y.z)` comment right above the function signature
  (`# TODO(R>=4.6.0): Deprecate check_file_listing once R 4.6.0 is the minimum supported version.`)
  indicating when the option should be deprecated and enforced universally.
- **Natural zero-lint fallback**: If a check naturally produces zero lints when
  not applicable (e.g., inspecting `namespace_imports()` on a file outside an R
  package or in a directory without a `NAMESPACE` file returns
  `empty_namespace_data()`), run the check unconditionally as part of the
  standard linter flow.
- **Keep signatures clean**: Preserving existing function signatures
  (`namespace_linter(check_exports = TRUE, check_nonexports = TRUE)`) reduces
  API complexity and documentation churn. When deprecating a linter or argument,
  use `lintr_deprecated()` (`R/deprecated.R`).

## 2. Helper Functions, Readability, & Safe Fallbacks

- **Encapsulating non-trivial logic in helper functions is encouraged**: While
  trivial single-use wrappers purely around basic line lookups or single
  `vapply(...)` steps should generally be avoided if they merely fragment linear
  control flow, **helper functions that encapsulate non-trivial logic (such as
  `get_seq_lint_message()` or `indent_lint_metadata()`) are highly valued, even
  if invoked only once inside a linter callback.** The readability improvement
  and clear domain encapsulation far outweigh their single-use nature.
- **Minimalist return types for linter helpers**: Helper functions that format
  linter messages (e.g., `get_seq_lint_message(seq_expr)`) should return a
  crisp, simple character vector of messages (`character()`), rather than
  leaking internal multi-column data structures back to the main linter
  callback. Keeping helper return types minimal keeps the linter body clean
  (`lint_message <- get_seq_lint_message(seq_expr); xml_nodes_to_lints(seq_expr, source_expression, lint_message, type = "warning")`)
  and keeps PR diffs compact.
- **Encapsulate empty-state and precondition checks inside helpers**: Place
  domain 0-length or type guards directly at the top of helper functions
  (`if (length(seq_expr) == 0L) return(character())`,
  `if (!is.function(fun)) return(FALSE)`) rather than polluting the main linter
  body with wrapper checks at every call site.
- **Namespace-aware string formatting in helpers**: When stripping parentheses
  or formatting extracted function calls (e.g., converting `length(x)` ->
  `length(...)` but preserving 0-argument calls like `n()`), always ensure
  exclusions are namespace-aware (`!grepl("(^|::)n\\(\\)", funcalls)` or
  `!funcalls %in% c("n()", "dplyr::n()")`). Checking `funcalls != "n()"` naively
  fails when passed namespace-prefixed calls like `dplyr::n()`.
- **Early returns on domain empty states**: Check for domain empty states early
  and exit (`if (nrow(ns_imports) == 0L) return(lints)` or
  `if (nrow(lint_line_df) == 0L) return(list())`) rather than executing checks
  over empty data structures.
- **Avoid distracting checks for 0-line files**: Do not recommend or implement
  defensive guards for truly empty zero-line files
  (`if (length(source_expression$file_lines) == 0L)`). Encountering a 0-line
  file in active extraction and parsing is virtually impossible; suggesting such
  checks creates unnecessary code clutter.
- **Rely on existing safe fallbacks & core utilities**:
  - Do not write defensive wrappers around functions that already handle `NULL`
    cleanly (e.g., `namespace_imports(NULL)` safely returns
    `empty_namespace_data()`).
  - Always use `get_r_string()` (`R/utils.R`) to extract string literals or
    unquoted symbol/slot names, and `xml_find_function_calls(..., keep_names = TRUE)`
    to leverage `{lintr}`'s built-in call and `"foo<-"` setter cache (see
    [`.agents/AGENTS.md`](../../AGENTS.md)).
- **Avoid redundant `trimws()` on XML node text**: Text extracted via
  `xml_find_chr_(..., "string(...)")` or `xml_text()` is already clean and
  trimmed by `xml2`. Avoid calling `trimws()` redundantly on extracted XML
  string vectors.
- **Use numbered format specifiers (`%1$s`, `%2$s`) for repeated variables in `sprintf()`**:
  When formatting dynamic lint messages where placeholders are repeated (e.g.,
  displaying both the recommended replacement and the observed violation using
  the same function name and parameter string), use numbered format specifiers
  (`sprintf("expect_shape(x, %1$s = %2$s) is better than %3$s(%1$s(x), %2$s)", shape_function, shape_arg_var, matched_function)`).
- **Lint message design & discernment**: The ultimate purpose of `lint_message`
  is to provide a helpful, actionable cue when a user receives a lint result in
  their IDE or CI. Avoid introducing distinct message classes if the information
  conveyed to the user does not meaningfully differ, as extra message complexity
  adds maintenance overhead without practical user value. Use distinct messages
  whenever doing so provides significantly clearer guidance to the user.

## 3. Minimal Diffs & Logical Execution Order

- **Structure execution to avoid intermediate mutations**: Structure the
  execution order of sub-checks within a linter to avoid mutating, subsetting,
  or filtering shared XML node lists and symbol vectors midway through the
  function.
- **Inline condition-dependent vector additions over intermediate state**: When
  conditionally appending items to a list or character vector, inline the check
  right inside `c()` (`c("strsplit", if (check_file_listing) c("dir", "list.files"))`)
  instead of assigning an intermediate temporary variable. Because `if (FALSE)`
  evaluates to `NULL` (which automatically drops out of `c()`), inlining cleanly
  eliminates temporary variable assignments.
- **Append new checks cleanly**: When adding a check to an existing linter
  (`check_exports`, `check_nonexports`), place the new check cleanly after
  existing checks so that existing code blocks and variables (`packages`,
  `symbols`, `ns_nodes`) remain untouched.

## 4. Respecting Contract Boundaries & Avoiding Unexported Internals (`:::`)

- **Strictly respect contract boundaries**: Take seriously that `:::` means
  *private* and avoid violating contract boundaries across packages. While
  `lintr` provides `%:::%` (`p %:::% f`) to bypass self-lint checks when calling
  private functions, reaching across package boundaries into unexported
  internals (`knitr:::parse_params`, `knitr:::file_ext`) should be avoided
  except in rare, justified cases.
- **Look under the hood for exported alternatives**: Very often private upstream
  internals are simple wrappers around `base` R functions or exported utilities
  from imported dependencies (such as `xfun::csv_options()`,
  `xfun::divide_chunk()`, or `xfun::file_ext()`).
- **Pragmatic ~95% correctness over fragile 100% correctness**: Even if private
  upstream helpers include extra edge-case nuance, choose to ignore that nuance
  for `lintr`'s purposes. Achieving ~95% practical correctness using clean,
  stable, exported APIs is far superior to fragile private calls.
- **Pragmatic namespace scoping & AST robustness**: For standard base functions
  (such as `is.numeric`, `is.integer`, `c`, `length`), do not add verbose AST
  predicates (`expr[1][not(expr/SYMBOL_PACKAGE) or expr/SYMBOL_PACKAGE[text() = 'base']]`)
  merely to guard against hypothetical third-party packages masking base
  functions. Reserve explicit namespace guards for cases where package masking
  is a known, realistic scenario in the R ecosystem.

## 5. Simple & Idiomatic AST and Condition Checks

- **Avoid over-engineered evaluation constructs**: When checking parsed
  parameter values or AST expressions (such as `eval` options from chunk
  headers), do not write complex, over-defensive constructs like
  `tryCatch(eval(..., envir = baseenv()))` to handle theoretical runtime
  expressions.
- **Check exact parser representations**: Inspect and match the exact R objects
  produced by the parser (`xfun::csv_options()` produces `logical` `FALSE` for
  `eval=FALSE` and symbol `quote(F)` for `eval=F`). A direct check such as
  `if (identical(eval_value, quote(F))) return(TRUE)` followed by
  `isFALSE(eval_value)` is simpler, safer, and easier to maintain.

## 6. Data Engineering, Centralized State, & Readability First

- **Use cohesive data frames (`line_metadata` / internal working state)**: When
  computing many parallel properties (such as line-by-line formatting attributes
  or multi-argument expression parsing), structure intermediate state as a
  single `data.frame` to enforce parallel alignment and enable clean matrix
  subsetting (`df[is_1arg, c("a", "b")] <- list("val", df$expr[is_1arg])`).
- **Internal `data.frame` vs minimalist return types**: Use `data.frame`
  strictly as an *internal working data structure* within helper functions when
  reading and modifying multiple parallel vectors. Avoid returning multi-column
  data frames to main callbacks when only a single output vector (like a
  character vector of lint messages) is needed.
- **Omit redundant `stringsAsFactors = FALSE`**: In R >= 4.0.0, `data.frame()`
  defaults to `stringsAsFactors = FALSE`.
- **Descriptive domain variable names for message assembly**: Use explicit
  domain-specific variable names when building linter messages:
  `preferred_usage` (the recommended replacement), `observed_usage` (the code
  pattern flagged), `is_decreasing`, `not_seq_along`.
- **Prioritize `with()` to eliminate repetitive visual noise**: When writing
  compound logical filtering conditions over multi-column metadata frames
  (`find_bad_lines()`), use
  `with(line_metadata, !is.na(line) & indent_level != expected_level & !in_str_const)`
  over repetitive `$` accesses (`line_metadata$line`,
  `line_metadata$indent_level`).

## 7. Idiomatic R Vectorization over Loops

- **Prefer vectorized operations**: Avoid using `for` loops to iterate over
  lines or XML nodes if a vectorized alternative exists (`Map()`, `vapply()`, or
  logical subsetting).
- **Vectorized sequence generation**: For example, to mark line ranges inside
  multi-line string constants:
  ```r
  line1 <- as.integer(xml_attr_(multiline_strings, "line1"))
  line2 <- as.integer(xml_attr_(multiline_strings, "line2"))
  is_in_str <- unlist(Map(`:`, line1, line2))
  in_str_const[is_in_str] <- TRUE
  ```

## 8. Handling Sequences, Ranges, Directionality, & Boundary Conditions

Linters inspecting sequence generation (such as `seq_linter` inspecting `1:n`,
`seq()`, `nrow(x):1`) must explicitly distinguish increasing, decreasing, and
boundary structures:

- **Increasing sequences**: `1:n`, `seq(1, n)`, or `seq(from = 1, to = n)` ->
  recommend `seq_len(n)` or `seq_along(x)`.
- **Decreasing sequences**: `n:1`, `seq(n, 1)`, or `seq(to = 1, from = n)` ->
  recommend `rev(seq_len(n))` or `rev(seq_along(x))`.
- **Single-element boundaries**: `seq(1, 1)` or `seq(from = 1, to = 1)` ->
  recommend `seq_len(1)` (or `seq_len(1L)`), **NEVER** `rev(seq_len(1))`. Ensure
  `is_decreasing` checks exclude `dot_expr1 %in% c("1", "1L")` or are scoped
  exclusively to reverse colon calls (`!is_seq & dot_expr2 %in% c("1", "1L")`).
- **Non-positive lower bounds**: `seq(0, 1)`, `seq(0, 10)`, `seq(-1, 1)`,
  `seq(-1, 10)`, `0:1`, `-1:10` are non-standard sequences; they must NOT be
  flagged as countdowns or converted to `seq_len()`. Guard in XPath against zero
  constants and unary minus:
  `and not(expr[NUM_CONST[text() = '0' or text() = '0L'] or OP-MINUS])`.

## 9. Robust Handling of Literate R Formats & Readable Transitions

- **Expect `NA_character_` in line content**: Literate programming formats
  (`.Rmd`, `.qmd`, `.Rnw`, `.Rtex`) extract R code by masking non-R lines with
  `NA_character_` to preserve line numbers (see [`literate-r-formats`](../literate-r-formats/SKILL.md)).
- **Avoid `NA` propagation in logical expressions**: Ensure logical expressions
  checking conditions over lines immediately guard against `NA`
  (`!is.na(line) & ...`) to keep resulting booleans clean (`FALSE & NA -> FALSE`).
- **Prioritize clear consecutive logic (`diff()`) over micro-optimizations**:
  Because `is_bad` evaluates to `FALSE` (not `NA`) across non-code gaps,
  computing consecutive state differences using simple arithmetic
  (`diff(is_bad) == 0L`) cleanly isolates block transitions across gaps. Never
  replace readable difference computations with obscure logical shifting
  constructs solely to avoid implicit conversions.

## 10. Direct Documentation Workflow & `roxygen2` Example Style

- **Crisp parameter and linter descriptions**: Keep `@description` and `@param`
  documentation concise and conceptual. If a linter checks a family of
  spiritually equivalent expressions (e.g., `1:length(...)`, `seq(1, n)`,
  `seq(10, 1)`, `nrow(x):1`), summarize the intent at a high level
  (`This linter checks for expressions like \code{1:length(...)} (and many other spiritually equivalent variations) which are a common source of bugs...`)
  rather than enumerating every syntax permutation in `@description`.
- **Roxygen example structure (`@examples`)**: Adhere strictly to `lintr`'s
  standard roxygen examples layout:
  1. **Section division (`# will produce lints` and `# okay`)**: Partition
     `@examples` into `# will produce lints` at top, directly followed by
     `# okay`.
  2. **Explicit `lint()` calls**: Format each demonstration cleanly across lines
     using `lint(text = ..., linters = ...)`:
     ```r
     #' lint(
     #'   text = "list.files(pattern = 'RDS')",
     #'   linters = fixed_regex_linter()
     #' )
     ```
  3. **Multi-line or escaped code (`writeLines`)**: For simple single-line
     invocations without complex escapes, pass `text = "..."` directly inside
     `lint()`. For multi-line snippets (containing `\n`) or regex patterns
     containing heavy backslash escaping (`"\\\\."`), assign the target code to
     `code_lines <- "..."`, display it first using `writeLines(code_lines)`, and
     then pass `text = code_lines` to `lint()`:
     ```r
     #' code_lines <- 'gsub("\\\\.", "", x)'
     #' writeLines(code_lines)
     #' lint(
     #'   text = code_lines,
     #'   linters = fixed_regex_linter()
     #' )
     ```
  4. **Mirrored 1:1 problem-to-solution pairs**: Maintain strict 1:1 structural
     correspondence between cases under `# will produce lints` and their
     resolutions under `# okay` in the same order. Do not introduce orphan or
     duplicate examples under `# okay`.
  5. **Standard footer tags**: Conclude linter roxygen blocks right after
     `@examples` with `@evalRd rd_tags("<linter_name>")`,
     `@seealso [linters] for a complete list of linters available in lintr.`,
     and `@export`.
