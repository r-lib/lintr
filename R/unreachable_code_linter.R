#' Block unreachable code and comments following return statements
#'
#' Code after e.g. a [return()] or [stop()]
#'   or in deterministically false conditional loops like `if (FALSE)` can't be reached;
#'   typically this is vestigial code left after refactoring or sandboxing code, which
#'   is fine for exploration, but shouldn't ultimately be checked in. Comments
#'   meant for posterity should be placed *before* the final `return()`.
#'
#' @examples
#' # will produce lints
#' code_lines <- "f <- function() {\n  return(1 + 1)\n  2 + 2\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = unreachable_code_linter()
#' )
#'
#' code_lines <- "f <- if (FALSE) {\n 2 + 2\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = unreachable_code_linter()
#' )
#'
#' code_lines <- "f <- while (FALSE) {\n 2 + 2\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = unreachable_code_linter()
#' )
#'
#' # okay
#' code_lines <- "f <- function() {\n  return(1 + 1)\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = unreachable_code_linter()
#' )
#'
#' code_lines <- "f <- if (foo) {\n 2 + 2\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = unreachable_code_linter()
#' )
#'
#' code_lines <- "f <- while (foo) {\n 2 + 2\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = unreachable_code_linter()
#' )
#'
#' @evalRd rd_tags("unreachable_code_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
unreachable_code_linter <- function() {
  expr_after_control <- "
    (//REPEAT | //ELSE | //FOR)/following-sibling::expr[1]
    | (//IF | //WHILE)/following-sibling::expr[2]
  "
  # NB: use not(OP-DOLLAR) to prevent matching process$stop(), #1051
  xpath_return_stop <- glue("
  (
    {expr_after_control}
    | (//FUNCTION | //OP-LAMBDA)[following-sibling::expr[1]/*[1][self::OP-LEFT-BRACE]]/following-sibling::expr[1]
  )
    /expr[expr[1][
      not(OP-DOLLAR or OP-AT)
      and SYMBOL_FUNCTION_CALL[text() = 'return' or text() = 'stop']
    ]]
    /following-sibling::*[
      not(self::OP-RIGHT-BRACE or self::OP-SEMICOLON)
      and (not(self::COMMENT) or @line2 > preceding-sibling::*[1]/@line2)
    ][1]
  ")
  xpath_next_break <- glue("
  ({expr_after_control})
    /expr[NEXT or BREAK]
    /following-sibling::*[
      not(self::OP-RIGHT-BRACE or self::OP-SEMICOLON)
      and (not(self::COMMENT) or @line2 > preceding-sibling::*[1]/@line2)
    ][1]
  ")

  xpath_if_while <- "
  (//WHILE | //IF)
    /following-sibling::expr[1][NUM_CONST[text() = 'FALSE']]
    /following-sibling::expr[1]
  "

  xpath_else <- "
  //IF[following-sibling::expr[1]/NUM_CONST[text() = 'TRUE']]
    /following-sibling::ELSE/following-sibling::expr[1]
  "

  handle_inline_conditions <- function(expr) {
    expr <- lapply(
      expr,
      function(x) {
        if (xml_name(xml2::xml_child(x)) == "OP-LEFT-BRACE") {
          xml_find_first(x, "expr")
        } else {
          x
        }
      }
    )
    expr[vapply(expr, xml2::xml_length, integer(1L)) != 0L]
  }

  # exclude comments that start with a nolint directive
  drop_nolint_end_comment <- function(expr) {
    is_nolint_end_comment <- xml2::xml_name(expr) == "COMMENT" &
      re_matches(xml_text(expr), settings$exclude_end)
    expr[!is_nolint_end_comment]
  }

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    expr_return_stop <- xml_find_all(xml, xpath_return_stop)

    lints_return_stop <- xml_nodes_to_lints(
      drop_nolint_end_comment(expr_return_stop),
      source_expression = source_expression,
      lint_message = "Code and comments coming after a return() or stop() should be removed.",
      type = "warning"
    )

    expr_next_break <- xml_find_all(xml, xpath_next_break)

    lints_next_break <- xml_nodes_to_lints(
      drop_nolint_end_comment(expr_next_break),
      source_expression = source_expression,
      lint_message = "Code and comments coming after a `next` or `break` should be removed.",
      type = "warning"
    )

    expr_if_while <- handle_inline_conditions(xml_find_all(xml, xpath_if_while))

    lints_if_while <- xml_nodes_to_lints(
      expr_if_while,
      source_expression = source_expression,
      lint_message = "Code inside a conditional loop with a deterministically false condition should be removed.",
      type = "warning"
    )

    expr_else <- handle_inline_conditions(xml_find_all(xml, xpath_else))

    lints_else <- xml_nodes_to_lints(
      expr_else,
      source_expression = source_expression,
      lint_message = "Code inside an else block after a deterministically true if condition should be removed.",
      type = "warning"
    )

    c(lints_return_stop, lints_next_break, lints_if_while, lints_else)
  })
}
