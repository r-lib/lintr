---
name: xpath-style
description: Guidelines and conventions for writing, composing, and formatting XPath expressions when building or refactoring linters. Use this skill whenever writing or analyzing XPath queries in linter definitions.
---

# XPath Style and Composition in {lintr}

When implementing, refactoring, or reviewing XPath expressions inside `lintr` (`R/`), follow these design, composition, and formatting guidelines established by project maintainers:

## 1. Direct Path Steps & Predicates over `self::*` Transitions

- **Avoid redundant `self::*` navigation axes:** When filtering nodes based on ancestral or parent structure, express structural requirements directly inside the target predicate rather than prepending an explicit `self::*[...] /` step before traversing to siblings or children.
- **Leverage shared parent context for siblings:** Because following or preceding siblings (`following-sibling::*`, `preceding-sibling::*`) share the exact same structural parent (`parent::*`) as the starting context node, express structural parent conditions directly inside the sibling predicates without extra bracket nesting (`parent::expr/OP-LEFT-BRACE` over `[parent::expr[OP-LEFT-BRACE]]`).
  ```r
  # Anti-pattern: Unnecessary self::* step and nested bracket evaluation
  self::*[parent::expr[OP-LEFT-BRACE]]
    /following-sibling::*[not(self::OP-RIGHT-BRACE)]

  # Preferred: Direct verification of parent structure within the sibling predicate
  following-sibling::*[
    parent::expr/OP-LEFT-BRACE
    and not(self::OP-RIGHT-BRACE)
  ][1]
  ```

## 2. Modular Composition (`glue::glue()`) & Helper Utilities

- **Decompose complex queries into modular variables:** Avoid large, monolithic XPath strings. Break distinct node criteria, structural targets, and sub-conditions into well-named variables (`expr_after_control`, `terminal_call_cond`, `unreachable_expr_cond_ws`) and synthesize the complete expression cleanly using `glue()`.
- **Parenthesize compound expressions prior to unions or depth traversing:** When combining complex branches via union (`|`) or traversing deeper via `//expr[...]` or `/following-sibling::*`, wrap individual sub-expressions inside explicit parentheses within the composite `glue()` string:
  ```r
  xpath_after_terminal_node <- glue("
    ({expr_after_control} | {terminal_fun_expr})//expr[{terminal_call_cond}]/{unreachable_expr_cond_ws}
    | ({expr_after_control} | {terminal_fun_expr})//expr[{terminal_call_cond}]/{unreachable_expr_cond_sc}
    | ({expr_after_control})//expr[NEXT or BREAK]/{unreachable_expr_cond_ws}
  ")
  ```
- **Use built-in `{lintr}` XPath helpers:** Use internal helper functions like `xp_or()`, `xp_and()`, and `xp_text_in_table()` (e.g. `xp_text_in_table(c("sprintf", "paste"))`) rather than constructing verbose repetitive conditions (`text() = 'sprintf' or text() = 'paste'`).

## 3. Multi-Line Formatting, Indentation, & Visual Hierarchy
- **Format multi-line strings clearly:** For non-trivial predicates or sequential criteria, break strings across lines with clean leading whitespace indentation that accurately reflects logical depth and bracket scope (`[` ... `]`).
- **Lead with operators and axis steps on continuation lines:** Start continuation lines cleanly with logical boolean operators (`and`, `or`, `|`) or navigation path boundaries (`/following-sibling::*`, `/parent::expr`) so visual structure and conditions can be assessed rapidly:
  ```r
  unreachable_expr_cond_ws <- "
    following-sibling::*[
      parent::expr/OP-LEFT-BRACE
      and not(self::OP-RIGHT-BRACE or self::OP-SEMICOLON or self::ELSE or preceding-sibling::ELSE)
      and (not(self::COMMENT) or @line2 > preceding-sibling::*[not(self::COMMENT)][1]/@line2)
    ][1]"
  ```

## 4. AST Structural Precision & Parent Container Awareness
- **Guard against unconstrained `//expr` searches matching inside call arguments:** Using deep (`//expr`) matches inside functions or loop arms matches expressions nested inside function call arguments (`switch(...)`, `ifelse(...)`, `tryCatch(...)`, or parenthesized expressions `(...)`). When targeting sequential statements inside procedural blocks, verify the structural block container (`parent::expr/OP-LEFT-BRACE` or `parent::exprlist/OP-SEMICOLON`) so argument separators (`OP-COMMA`) or closing delimiters (`OP-RIGHT-PAREN`) inside nested calls are not falsely matched as sequential siblings.
- **Differentiate whitespace sequences from `<exprlist>` (`OP-SEMICOLON`) boundaries:** Statements separated by semicolons (`return(x); y <- 2`) in R's AST are grouped under `<exprlist[OP-SEMICOLON]>` nodes rather than basic `<expr>` siblings.

## 5. Self-Documentation & Explicit Exclusion Comments
- **Annotate structural transitions:** Provide brief, clear comments above multi-line modular XPath definitions explaining architectural tradeoffs or distinct handling regimes (e.g., `# normal case: expression after terminal call is on the next line` vs `# robustness case: expression after terminal call is on the same line`).
- **Document precise syntax exclusions right where they are defined:** Whenever an XPath guards against edge-case node structures (`not(OP-DOLLAR or OP-AT)` or `not(self::ELSE or preceding-sibling::ELSE)`), note the practical rationale or tracking reference directly above (`# NB: use not(OP-DOLLAR) to prevent matching process$stop(), #1051`) so regressions are avoided during future structural updates.
