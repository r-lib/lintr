#' Enforce usage of scalar logical operators in conditional statements
#'
#' Usage of `&` in conditional statements is error-prone and inefficient.
#'   `condition` in `if (condition) expr` must always be of length 1, in which
#'   case `&&` is to be preferred. Ditto for `|` vs. `||`.
#'
#' This linter covers inputs to `if()` and `while()` conditions and to
#'   [testthat::expect_true()] and [testthat::expect_false()].
#'
#' Note that because `&` and `|` are generics, it is possible that
#'   `&&` / `||` are not perfect substitutes because `&` is doing
#'   method dispatch in an incompatible way.
#'
#' Moreover, be wary of code that may have side effects, most commonly
#'   assignments. Consider `if ((a <- foo(x)) | (b <- bar(y))) { ... }`
#'   vs. `if ((a <- foo(x)) || (b <- bar(y))) { ... }`. Because `||` exits
#'   early, if `a` is `TRUE`,  the second condition will never be evaluated
#'   and `b` will not be assigned. Such usage is not allowed by the Tidyverse
#'   style guide, and the code can easily be refactored by pulling the
#'   assignment outside the condition, so using `||` is still preferable.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "if (TRUE & FALSE) 1",
#'   linters = vector_logic_linter()
#' )
#'
#' lint(
#'   text = "if (TRUE && (TRUE | FALSE)) 4",
#'   linters = vector_logic_linter()
#' )
#'
#' lint(
#'   text = "filter(x, A && B)",
#'   linters = vector_logic_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "if (TRUE && FALSE) 1",
#'   linters = vector_logic_linter()
#' )
#'
#' lint(
#'   text = "if (TRUE && (TRUE || FALSE)) 4",
#'   linters = vector_logic_linter()
#' )
#'
#' lint(
#'   text = "filter(x, A & B)",
#'   linters = vector_logic_linter()
#' )
#'
#' @evalRd rd_tags("vector_logic_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#if-statements>
#' @export
vector_logic_linter <- function() {
  # ensures the expr is in the cond part of `if/while (cond) expr` --
  #   if on the XML parse tree is structured like
  #   <expr>
  #     <IF> | <expr><SYMBOL_FUNCTION_CALL>
  #     <OP-LEFT-PAREN>
  #     <expr> ... </expr> # <- loop condition
  #     <OP-RIGHT-PAREN>
  #     <expr> ... </expr> # <- evaluation; includes BRACEs if present
  #     <ELSE>             # (here & below is optional)
  #     <expr> ... </expr>
  #  </expr>
  #  we _don't_ want to match anything on the second expr, hence this
  condition_xpath <- "
  (//AND | //OR)[
    ancestor::expr[
      not(preceding-sibling::OP-RIGHT-PAREN)
      and preceding-sibling::*[
        self::IF
        or self::WHILE
        or self::expr/SYMBOL_FUNCTION_CALL[text() = 'expect_true' or text() = 'expect_false']
      ]
    ]
    and not(ancestor::expr[
      preceding-sibling::expr[last()][SYMBOL_FUNCTION_CALL[not(text() = 'expect_true' or text() = 'expect_false')]]
      or preceding-sibling::OP-LEFT-BRACKET
    ])
    and not(parent::expr/expr[
      STR_CONST
      or expr/SYMBOL_FUNCTION_CALL[text() = 'as.raw' or text() = 'as.octmode' or text() = 'as.hexmode']
    ])
  ]
  "

  subset_xpath <- "
  self::*[not(SYMBOL_PACKAGE[text() = 'stats'])]
    /parent::expr
    //expr[
      (AND2 or OR2)
      and not(preceding-sibling::expr[last()]/SYMBOL_FUNCTION_CALL[not(text() = 'subset' or text() = 'filter')])
      and not(preceding-sibling::OP-LEFT-BRACKET)
      and not(preceding-sibling::*[not(self::COMMENT)][2][self::SYMBOL_SUB and text() = 'circular'])
    ]
    /*[not(self::COMMENT)][2]
  "

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    xml_call <- source_expression$xml_find_function_calls(c("subset", "filter"))

    condition_expr <- xml_find_all(xml, condition_xpath)
    condition_op <- xml_text(condition_expr)
    condition_lints <- xml_nodes_to_lints(
      condition_expr,
      source_expression = source_expression,
      lint_message = sprintf("Use `%s` in conditional expressions.", strrep(condition_op, 2L)),
      type = "warning"
    )

    subset_expr <- xml_find_all(xml_call, subset_xpath)
    subset_op <- xml_text(subset_expr)
    subset_lints <- xml_nodes_to_lints(
      subset_expr,
      source_expression = source_expression,
      lint_message = sprintf("Use `%s` in subsetting expressions.", substr(subset_op, 1L, 1L)),
      type = "warning"
    )

    c(condition_lints, subset_lints)
  })
}
