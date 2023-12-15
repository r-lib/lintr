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
#' @examples
#' # will produce lints
#' lint(
#'   text = "if (x == 'a') 1 else if (x == 'b') 2 else 3",
#'   linters = if_switch_linter()
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
#' @evalRd rd_tags("if_switch_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
if_switch_linter <- function() {
  equal_str_cond <- "expr[1][EQ and expr[STR_CONST]]"

  # NB: IF AND {...} AND ELSE/... implies >= 3 equality conditions are present
  # .//expr/IF/...: the expr in `==` that's _not_ the STR_CONST
  # not(preceding::IF): prevent nested matches which might be incorrect globally
  # not(. != .): don't match if there are _any_ expr which _don't_ match the top
  #   expr
  xpath <- glue("
  //IF
    /parent::expr[
      not(preceding-sibling::IF)
      and {equal_str_cond}
      and ELSE/following-sibling::expr[
        IF
        and {equal_str_cond}
        and ELSE/following-sibling::expr[IF and {equal_str_cond}]
      ]
      and not(
        .//expr/IF/following-sibling::{equal_str_cond}/expr[not(STR_CONST)]
          != expr[1][EQ]/expr[not(STR_CONST)]
      )
    ]
  ")

  Linter(linter_level = "expression", supports_exprlist = TRUE, function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = paste(
        "Prefer switch() statements over repeated if/else equality tests,",
        "e.g., switch(x, a = 1, b = 2) over",
        'if (x == "a") 1 else if (x == "b") 2.'
      ),
      type = "warning"
    )
  })
}
