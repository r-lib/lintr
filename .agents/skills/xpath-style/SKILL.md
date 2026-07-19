---
name: xpath-style
description: Guidelines and conventions for writing, composing, and formatting XPath expressions when building or refactoring linters. Use this skill whenever writing or analyzing XPath queries in linter definitions.
---

# XPath Style and Composition in {lintr}

When implementing, refactoring, or reviewing XPath expressions across `lintr` (`R/`), adhere to the following composition, styling, robust AST targeting, and performance guidelines established by project maintainers:

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
- **Factor shared invariant predicates out of compound disjunctions:** When matching multiple candidate structures inside a predicate using `or`, factor shared conjuncts out to the surrounding scope instead of distributing them inside every branch. For example, use `STR_CONST and (branch_A or branch_B)` rather than `(branch_A and STR_CONST) or (branch_B and STR_CONST)`. This reduces evaluation duplication and instantly exposes what truly separates the target branches.
- **Order unnested single conditions before multi-condition parenthesized expressions:** Inside compound `or` structures (`STR_CONST and (...)`), place single-condition branches (`preceding-sibling::*[not(self::COMMENT)][2][self::SYMBOL_SUB[...]]`) ahead of multi-condition compound branches (`(position() = 2 - count(...) and not(EQ_SUB))`). Ordering unnested conditions first eliminates distracting double-bracket indentation right at the opening transition (`(( ... ) or ... )`), markedly improving visual structure and readability.


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

## 4. XPath Performance & Entry Point Anchoring

- **Avoid `//*` XPaths entirely:** Wildcard descendant scanning (`//*`) is a severe performance bottleneck across `{xml2}` and `libxml2`. Even an extensive union chain of specific node names (`//A[expr] | //B[expr]`) consistently yields substantial speed optimizations (often 3x or greater) compared to unconstrained wildcard matching.
- **Avoid `//expr` XPaths as entry points:** Because over one-third of all AST nodes in an R codebase are `<expr>`, querying `//expr` as an initial anchor only eliminates a minimal portion of the parse tree. Always anchor initial traversals on more specific syntax nodes (`//IF`, `//FOR`, `//RIGHT_ASSIGN`, `//EQ_SUB`).
- **Use `xml_find_function_calls()` over `//SYMBOL_FUNCTION_CALL` entry points:** Whenever a linter checks for calls to specific functions, never anchor directly on `//SYMBOL_FUNCTION_CALL[text() = 'foo' or text() = 'bar']`. Instead, fetch pre-cached structures via `source_expression$xml_find_function_calls(c("foo", "bar"))`, which executes significantly faster across multi-option lookups.

## 5. AST Structural Precision & Robust Node Matching
- **Guard against unconstrained `//expr` searches inside call arguments:** Using deep (`//expr`) traversals inside function bodies or loops captures expressions nested deep inside argument lists (`switch(...)`, `ifelse(...)`, `tryCatch(...)`, or parenthesized `(...)` blocks). When verifying sequential procedural statements within code blocks, explicitly verify the structural block container (`parent::expr/OP-LEFT-BRACE` or `parent::exprlist/OP-SEMICOLON`) so function argument commas (`OP-COMMA`) or closing delimiters (`OP-RIGHT-PAREN`) are not falsely identified as procedural sequence steps.
- **Anchor on `EQ_SUB` instead of `SYMBOL_SUB` when matching named arguments:** Because valid string literal parameter names (`foo("a" = 1)`) parse as `<STR_CONST>` rather than `<SYMBOL_SUB>`, queries anchored strictly on `SYMBOL_SUB` skip quoted argument names. Anchoring on `<EQ_SUB>` reliably matches all named assignment arguments.
- **Be cautious with wildcard (`*`) and bare `expr` sibling lookups:**
  - Sibling lookups like `preceding-sibling::*[1]` and `following-sibling::*[1]` frequently land on `<COMMENT>` nodes because comments can appear almost anywhere across the AST. Explicitly filter out comment nodes when tracing syntax structure (`preceding-sibling::*[not(self::COMMENT)][1]`).
  - Depending on the active R version, `=` assignment expressions (`a = 1`) may wrap in `<equal_assign>` or `<expr_or_assign_or_help>` nodes rather than standard `<expr>` nodes (unlike `a <- 1`). Therefore, rigid lookups such as `preceding-sibling::expr[1]` can silently jump over assignments.
- **Differentiate whitespace sequences from `<exprlist>` (`OP-SEMICOLON`) boundaries:** Statements separated by semicolons (`return(x); y <- 2`) in R's AST are enclosed inside `<exprlist[OP-SEMICOLON]>` nodes rather than typical `<expr>` sibling sequences.
- **Account for grammar specificities and node representations:**
  - **Logical Constants vs Shorthands:** `TRUE` and `FALSE` appear as `<NUM_CONST>` nodes (`NUM_CONST[text() = 'TRUE']`), while shorthands `T` and `F` appear as `<SYMBOL>` nodes (`SYMBOL[text() = 'T']`).
  - **Magrittr vs Native Pipes (`SPECIAL` vs `PIPE`):** Native pipe `|>` appears as `<PIPE>`, whereas magrittr `%>%` appears as `<SPECIAL>`. Since all custom infix operators (`%%`, `%in%`, `%*%`) also parse as `<SPECIAL>`, verify node text explicitly (`SPECIAL[text() = '%>%']`). Keep pipeline restructuring in mind when designing positional checking (`x |> f(arg)` makes `arg` positional argument 2 inside the underlying evaluation).
  - **`for` loop AST structure:** `for` loops differ sharply from `while()` and `if()` constructs by enclosing their header elements inside a `<forcond>` node (`<forcond>` containing `<OP-LEFT-PAREN>`, `<SYMBOL>`, `<IN>`, `<expr>`, and `<OP-RIGHT-PAREN>`).
  - **S4 slot (`@`) vs Dollar (`$`) extraction:** For `x$y`, the right-hand side directly separates property symbols (`SYMBOL`) from method calls (`SYMBOL_FUNCTION_CALL`). However, the right-hand side of `x@y` (or `x@y()`) always resolves to a `<SLOT>` node. To distinguish between property access and function invocations across `@`, check whether an `<OP-LEFT-PAREN>` (`(`) sibling follows.
  - **Sub-tree value equality (`<expr1> = <expr2>`):** Comparing two node branches with XPath `=` tests aggregate string equality across all descendant text nodes. Because child `<COMMENT>` nodes inside either branch change the aggregate string value, guard against or exclude embedded comments before checking structural equality.

## 6. Self-Documentation & Explicit Exclusion Comments
- **Annotate structural distinctions:** Provide brief, explicit comments above multi-line modular XPath definitions clarifying structural design decisions or separate handling regimes (`# normal case: expression after terminal call is on the next line` vs `# robustness case: expression after terminal call is on the same line`).
- **Document syntax exclusions right where defined:** Whenever an XPath guards against edge-case structures (`not(OP-DOLLAR or OP-AT)` or `not(self::ELSE or preceding-sibling::ELSE)`), document the practical rationale or tracking reference directly above (`# NB: use not(OP-DOLLAR) to prevent matching process$stop(), #1051`) so regressions are prevented during future structural iterations.
