#' Require usage of switch() over repeated if/else blocks
#'
#' [switch()] statements in R are used to delegate behavior based
#'   on the value of some input scalar string, e.g.
#'   `switch(x, a = 1, b = 3, c = 7, d = 8)` will be one of
#'   `1`, `3`, `7`, or `8`, depending on the value of `x`.
#'
#' This can also be accomplished by repeated `if`/`else` statements like
#'   so: `if (x == "a") 1 else if (x == "b") 2 else if (x == "c") 7 else 8`
#'   (implicitly, the last `else` assumes x only takes 4 possible values),
#'   but this is more cluttered and slower (note that `switch()` takes the same
#'   time to evaluate regardless of the value of `x`, and is faster even
#'   when `x` takes the first value (here `a`), and that the `if`/`else`
#'   approach is roughly linear in the number of conditions that need to
#'   be evaluated, here up to 3 times).
#'
#' @param max_branch_lines,max_branch_expressions Integer, default 0 indicates "no maximum".
#'   If set any `if`/`else if`/.../`else` chain where any branch occupies more than
#'   this number of lines (resp. expressions) will not be linted. The conjugate
#'   applies to `switch()` statements -- if these parameters are set, any `switch()`
#'   statement with any overly-complicated branches will be linted. See examples.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "if (x == 'a') 1 else if (x == 'b') 2 else 3",
#'   linters = if_switch_linter()
#' )
#'
#' code <- paste(
#'   "if (x == 'a') {",
#'   "  1",
#'   "} else if (x == 'b') {",
#'   "  2",
#'   "} else if (x == 'c') {",
#'   "  y <- x",
#'   "  z <- sqrt(match(y, letters))",
#'   "  z",
#'   "}",
#'   sep = "\n"
#' )
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = if_switch_linter()
#' )
#'
#' code <- paste(
#'   "if (x == 'a') {",
#'   "  1",
#'   "} else if (x == 'b') {",
#'   "  2",
#'   "} else if (x == 'c') {",
#'   "  y <- x",
#'   "  z <- sqrt(",
#'   "    match(y, letters)",
#'   "  )",
#'   "  z",
#'   "}",
#'   sep = "\n"
#' )
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = if_switch_linter()
#' )
#'
#' code <- paste(
#'   "switch(x,",
#'   "  a = {",
#'   "    1",
#'   "    2",
#'   "    3",
#'   "  },",
#'   "  b = {",
#'   "    1",
#'   "    2",
#'   "  }",
#'   ")",
#'   sep = "\n"
#' )
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = if_switch_linter(max_branch_lines = 2L)
#' )
#'
#' # okay
#' lint(
#'   text = "switch(x, a = 1, b = 2, 3)",
#'   linters = if_switch_linter()
#' )
#'
#' # switch() version not as clear
#' lint(
#'   text = "if (x == 'a') 1 else if (x == 'b' & y == 2) 2 else 3",
#'   linters = if_switch_linter()
#' )
#'
#' code <- paste(
#'   "if (x == 'a') {",
#'   "  1",
#'   "} else if (x == 'b') {",
#'   "  2",
#'   "} else if (x == 'c') {",
#'   "  y <- x",
#'   "  z <- sqrt(match(y, letters))",
#'   "  z",
#'   "}",
#'   sep = "\n"
#' )
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = if_switch_linter(max_branch_lines = 2L)
#' )
#'
#' code <- paste(
#'   "if (x == 'a') {",
#'   "  1",
#'   "} else if (x == 'b') {",
#'   "  2",
#'   "} else if (x == 'c') {",
#'   "  y <- x",
#'   "  z <- sqrt(",
#'   "    match(y, letters)",
#'   "  )",
#'   "  z",
#'   "}",
#'   sep = "\n"
#' )
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = if_switch_linter(max_branch_expressions = 2L)
#' )
#'
#' code <- paste(
#'   "switch(x,",
#'   "  a = {",
#'   "    1",
#'   "    2",
#'   "    3",
#'   "  },",
#'   "  b = {",
#'   "    1",
#'   "    2",
#'   "  }",
#'   ")",
#'   sep = "\n"
#' )
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = if_switch_linter(max_branch_lines = 3L)
#' )
#'
#' @evalRd rd_tags("if_switch_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
if_switch_linter <- function(max_branch_lines = 0L, max_branch_expressions = 0L) {
  equal_str_cond <- "expr[1][EQ and expr/STR_CONST]"

  if (max_branch_lines > 0L || max_branch_expressions > 0L) {
    complexity_cond <- xp_or(c(
      if (max_branch_lines > 0L) paste("OP-RIGHT-BRACE/@line2 - OP-LEFT-BRACE/@line1 > 1 +", max_branch_lines),
      if (max_branch_expressions > 0L) paste("count(expr) >", max_branch_expressions)
    ))
    branch_expr_cond <- xp_and(c(
      xp_or(
        # if (x) { <this expr> } ...
        xp_and("preceding-sibling::IF", "position() = 2"),
        # if (x) { ... } else { <this expr> }
        xp_and("preceding-sibling::ELSE", "not(IF)")
      ),
      complexity_cond
    ))
    max_lines_cond <- glue(".//expr[{branch_expr_cond}]")

    switch_xpath <- glue("
    parent::expr[expr[
      position() > 2
      and {complexity_cond}
    ]]")
  } else {
    max_lines_cond <- "false"

    switch_xpath <- NULL
  }

  # NB: IF AND {...} AND ELSE/... implies >= 3 equality conditions are present
  # .//expr/IF/...: the expr in `==` that's _not_ the STR_CONST
  # not(preceding::IF): prevent nested matches which might be incorrect globally
  if_xpath <- glue("
  //IF
    /parent::expr[
      not(preceding-sibling::IF)
      and {equal_str_cond}
      and ELSE/following-sibling::expr[
        IF
        and {equal_str_cond}
        and ELSE/following-sibling::expr[IF and {equal_str_cond}]
      ]
      and not({ max_lines_cond })
    ]
  ")

  # not(. != .): don't match if there are _any_ expr which _don't_ match the top expr
  equality_test_cond <- glue("self::*[
    .//expr/IF/following-sibling::{equal_str_cond}/expr[not(STR_CONST)]
      != expr[1][EQ]/expr[not(STR_CONST)]
  ]")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, if_xpath)
    expr_all_equal <- is.na(xml_find_first(
      strip_comments_from_subtree(bad_expr),
      equality_test_cond
    ))

    lints <- xml_nodes_to_lints(
      bad_expr[expr_all_equal],
      source_expression = source_expression,
      lint_message = paste(
        "Prefer switch() statements over repeated if/else equality tests,",
        "e.g., switch(x, a = 1, b = 2) over",
        'if (x == "a") 1 else if (x == "b") 2.'
      ),
      type = "warning"
    )

    if (!is.null(switch_xpath)) {
      xml_calls <- source_expression$xml_find_function_calls("switch")
      switch_expr <- xml_find_all(xml_calls, switch_xpath)

      lints <- c(lints, xml_nodes_to_lints(
        switch_expr,
        source_expression = source_expression,
        lint_message = "Prefer repeated if/else statements over overly-complicated switch() statements.",
        type = "warning"
      ))
    }

    lints
  })
}
