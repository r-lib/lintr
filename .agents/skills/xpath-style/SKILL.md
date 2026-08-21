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
- **Enforcing allowed named-argument whitelists via De Morgan's law:** To ensure that *all* named arguments present on a function call (if any exist) belong exclusively to an allowed set of keyword names (e.g. only `from` and `to` named arguments in `seq()`), negate the existence of any disallowed named argument: `not(SYMBOL_SUB[text() != 'from' and text() != 'to'])`. Always document this with an explanatory comment: `# not(SYMBOL_SUB[...]) ensures all named arguments (if any) are exclusively 'from' or 'to'`.
- **Concise AST argument role determination in multi-argument calls:** For functions where arguments may appear positionally, named, or reversed (`seq(1, 10)`, `seq(from = 1, to = 10)`, `seq(to = 10, from = 1)`, `seq(to = 10, 1)`, `seq(10, from = 1)`, `seq(from = 1, 10)`):
  - To identify if the first argument token (`expr[2]`) represents a specific role (e.g. `to`), use concise sibling lookups:
    `./expr[2]/preceding-sibling::SYMBOL_SUB[text() = 'to'] | ./expr[2]/following-sibling::SYMBOL_SUB[text() = 'from']`
  - In XPath matching predicates, support both named permutations cleanly:
    `(expr[3]/preceding-sibling::SYMBOL_SUB[1][text() = 'from'] or expr[2]/preceding-sibling::SYMBOL_SUB[1][text() = 'to'])`.


## 3. Multi-Line Formatting, Indentation, & Visual Hierarchy
- **Format multi-line strings clearly:** For non-trivial predicates or sequential criteria, break strings across lines with clean leading whitespace indentation that accurately reflects logical depth and bracket scope (`[` ... `]`).
- **Avoid `paste()` for multi-line XPath strings:** Write clean, multi-line literal strings directly rather than piecing together fragments with `paste(...)`.
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
- **Prefer `xml_find_lgl_()` with `boolean(...)` or `not(...)` over `is.na(xml_find_first_())`:**
  - `xml_find_first()` creates intermediate R objects: an `XPtrNode` external pointer, a list with `node` and `doc` elements, names, and class attributes. When testing existence or matching, this object allocation is pure overhead.
  - `xml_find_lgl(x, "boolean(...)")` evaluates `XPATH_BOOLEAN` directly inside `libxml2` and returns a flat atomic logical vector with zero intermediate R object allocations. On realistic parse trees, `xml_find_lgl_()` is **~1.7x faster** and consumes **67–75% less memory** on large nodesets than `xml_find_first_()`.
  - **XPath boolean requirement:** In XPath 1.0, `xml_find_lgl_()` requires an expression that evaluates to a boolean (e.g. `boolean(...)`, `not(...)`, `=`, `!=`). Passing a raw node query like `xml_find_lgl_(x, "SYMBOL")` throws an error.
  - **Use positive framing with `not(...)` over double-negatives (`!xml_find_lgl_(..., "boolean(...)")`):**
    - Check presence: `has_match <- xml_find_lgl_(x, "boolean(XPATH)")`
    - Check absence: `is_missing <- xml_find_lgl_(x, "not(XPATH)")`
    - Avoid `!xml_find_lgl_(x, "boolean(XPATH)")` when `xml_find_lgl_(x, "not(XPATH)")` directly expresses the condition without forcing the reader to parse a double negative.
  - **Safely handling `xml_missing()`:**
    - On `xml2::xml_missing()` inputs (e.g. files with syntax errors or unparsed source expressions), `xml_find_lgl_(xml, ...)` returns `logical(0)`.
    - If testing `xml` in a scalar `if (...)` condition at the file level, guard against `length 0` with `isTRUE()` or `!isTRUE()` (e.g. `if (isTRUE(xml_find_lgl_(xml, xpath)))` or `if (!isTRUE(xml_find_lgl_(xml, xpath)))`), or `!any(xml_find_lgl_(...))` for nodesets.
- **Prefer `xml_name_()` or XPath `name(...)` over XPath `self::TAG` or `xml_find_first_` for node tag matching:**
  - Direct tag comparison `xml_name_(nodes) == "TAG"` on AST nodes in R is **~47x faster** than evaluating an XPath `self::TAG` query or `xml_find_first_(nodes, "self::TAG")`.
  - When extracting the tag name of child or sibling nodes without needing the node object, use `xml_find_chr_(nodes, "name(*[2])")` instead of `xml_name_(xml_find_first_(nodes, "*[2]"))`.
- **Extracting node text via `xml_find_chr_(x, "string(...)")` vs `xml_text(xml_find_first_(x, ...))`:**
  - `xml_find_chr_(x, "string(XPATH)")` extracts text directly at the C level without allocating intermediate `xml_node` objects, allocating **0 bytes** on scalar `xml_node` inputs and **~94% less memory** on nodesets.
  - **Missing node behavior (`""` vs `NA`):** Per W3C XPath 1.0 (§4.2), converting an empty nodeset to a string returns `""` (empty string), whereas `xml_text(xml_find_first_())` returns `NA_character_`.
    - `""` is safer in direct comparisons (`if (xml_find_chr_(node, "string(OP)") == "%>%")` evaluates cleanly to `FALSE` instead of throwing a `missing value where TRUE/FALSE needed` error when the operator is absent).
  - **Avoid multiple `xml_find_chr_` calls for properties of the same node:** If you need **multiple properties** from the same target node (e.g. both tag name and text, or multiple attributes like `@line1` and `@col1`), find the node **once** via `xml_find_first_()` / `xml_find_all_()` and extract the properties directly (`xml_name_()`, `xml_text()`, `xml_attr_()`). Evaluating the full XPath traversal multiple times via separate `xml_find_chr_()` calls is **~2x slower** and uses more memory than a single node lookup followed by direct C-pointer property access.
- **Prefer vectorized `xml_find_num_()` with XPath `count()` over `vapply(length(xml_find_all_))`:** To count argument or node occurrences across a nodeset, wrap the XPath query in `count(...)` and execute `as.integer(xml_find_num_(nodes, "count(...)"))` instead of iterating with `vapply(nodes, \(node) length(xml_find_all_(node, xpath)), integer(1L))`. This delegates counting to `libxml2`'s C-level XPath evaluator in a single vectorized pass across all nodes. Always cast to `as.integer()` for explicit integer typing. Never use `as.integer(xml_find_chr_(..., "string(count(...))"))`.
- **Precise preceding-sibling relative anchoring for named keyword argument values:** When excluding or matching the value `<expr>` of a specific keyword argument (such as `domain = <expr>` in `gettextf()`), do not write unindexed preceding-sibling predicates like `not(preceding-sibling::*[...][self::SYMBOL_SUB[text() = 'domain']])`. Unindexed `preceding-sibling::*` evaluates across *all* preceding siblings of subsequent positional arguments, falsely matching earlier keyword tokens and breaking argument counts. Precisely anchor to the immediate preceding non-comment sibling token instead:
  `preceding-sibling::*[not(self::COMMENT)][1][self::EQ_SUB]/preceding-sibling::*[not(self::COMMENT)][1][self::SYMBOL_SUB[text() = 'domain']]`
- **Anchor on `EQ_SUB` instead of `SYMBOL_SUB` when matching named arguments:** Because valid string literal parameter names (`foo("a" = 1)`) parse as `<STR_CONST>` rather than `<SYMBOL_SUB>`, queries anchored strictly on `SYMBOL_SUB` skip quoted argument names. Anchoring on `<EQ_SUB>` reliably matches all named assignment arguments.
- **Exact predicate matching for AST symbols:** Always match symbol text explicitly with `expr[SYMBOL[text() = '.N']]` rather than loose comparisons like `expr[SYMBOL = '.N']`.
- **Be cautious with wildcard (`*`) and bare `expr` sibling lookups:**
  - Sibling lookups like `preceding-sibling::*[1]` and `following-sibling::*[1]` frequently land on `<COMMENT>` nodes because comments can appear almost anywhere across the AST. Explicitly filter out comment nodes when tracing syntax structure (`preceding-sibling::*[not(self::COMMENT)][1]`).
  - Depending on the active R version, `=` assignment expressions (`a = 1`) may wrap in `<equal_assign>` or `<expr_or_assign_or_help>` nodes rather than standard `<expr>` nodes (unlike `a <- 1`). Therefore, rigid lookups such as `preceding-sibling::expr[1]` can silently jump over assignments.
- **Differentiate whitespace sequences from `<exprlist>` (`OP-SEMICOLON`) boundaries:** Statements separated by semicolons (`return(x); y <- 2`) in R's AST are enclosed inside `<exprlist[OP-SEMICOLON]>` nodes rather than typical `<expr>` sibling sequences.
- **Account for grammar specificities and node representations:**
  - **Unary Operators and Negative Numbers (`OP-MINUS`):** In R's parse tree, unary minus `-1` or unary plus `+1` wraps the numeric constant inside a nested `<expr>`: `<expr><OP-MINUS>-</OP-MINUS><expr><NUM_CONST>1</NUM_CONST></expr></expr>`.
    - A direct child query `expr[NUM_CONST[text() = '1']]` matches only positive `1`, NOT `-1`.
    - A descendant query `expr//NUM_CONST[text() = '1']` or `expr[.//NUM_CONST[text() = '1']]` matches BOTH `1` and `-1`.
    - When matching positive numeric boundaries (e.g. `1` or `1L`) in sequence generation, explicitly guard against non-positive numbers (e.g. `0`, `0L`, or negative numbers) with `and not(expr[NUM_CONST[text() = '0' or text() = '0L'] or OP-MINUS])`.
  - **Logical Constants vs Shorthands:** `TRUE` and `FALSE` appear as `<NUM_CONST>` nodes (`NUM_CONST[text() = 'TRUE']`), while shorthands `T` and `F` appear as `<SYMBOL>` nodes (`SYMBOL[text() = 'T']`).
  - **Magrittr vs Native Pipes (`SPECIAL` vs `PIPE`):** Native pipe `|>` appears as `<PIPE>`, whereas magrittr `%>%` appears as `<SPECIAL>`. Since all custom infix operators (`%%`, `%in%`, `%*%`) also parse as `<SPECIAL>`, verify node text explicitly (`SPECIAL[text() = '%>%']`). Keep pipeline restructuring in mind when designing positional checking (`x |> f(arg)` makes `arg` positional argument 2 inside the underlying evaluation).
  - **`for` loop AST structure:** `for` loops differ sharply from `while()` and `if()` constructs by enclosing their header elements inside a `<forcond>` node (`<forcond>` containing `<OP-LEFT-PAREN>`, `<SYMBOL>`, `<IN>`, `<expr>`, and `<OP-RIGHT-PAREN>`).
  - **S4 slot (`@`) vs Dollar (`$`) extraction:** For `x$y`, the right-hand side directly separates property symbols (`SYMBOL`) from method calls (`SYMBOL_FUNCTION_CALL`). However, the right-hand side of `x@y` (or `x@y()`) always resolves to a `<SLOT>` node. To distinguish between property access and function invocations across `@`, check whether an `<OP-LEFT-PAREN>` (`(`) sibling follows.
  - **Sub-tree value equality (`<expr1> = <expr2>`):** Comparing two node branches with XPath `=` tests aggregate string equality across all descendant text nodes. Because child `<COMMENT>` nodes inside either branch change the aggregate string value, guard against or exclude embedded comments before checking structural equality.

## 5. Self-Documentation & Explicit Exclusion Comments
- **Annotate structural distinctions:** Provide brief, explicit comments above multi-line modular XPath definitions clarifying structural design decisions or separate handling regimes (`# normal case: expression after terminal call is on the next line` vs `# robustness case: expression after terminal call is on the same line`).
- **Document syntax exclusions right where defined:** Whenever an XPath guards against edge-case structures (`not(OP-DOLLAR or OP-AT)` or `not(self::ELSE or preceding-sibling::ELSE)`), document the practical rationale or tracking reference directly above (`# NB: use not(OP-DOLLAR) to prevent matching process$stop(), #1051`) so regressions are prevented during future structural iterations.
