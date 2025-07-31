#' Block unreachable code and comments following return statements
#'
#' Code after e.g. a [return()] or [stop()]
#'   or in deterministically false conditional loops like `if (FALSE)` can't be reached;
#'   typically this is vestigial code left after refactoring or sandboxing code, which
#'   is fine for exploration, but shouldn't ultimately be checked in. Comments
#'   meant for posterity should be placed *before* the final `return()`.
#'
#' @param allow_comment_regex Character vector of regular expressions which identify
#'   comments to exclude when finding unreachable terminal comments. By default, this
#'   includes the default "skip region" end marker for `{covr}`
#'   (option "covr.exclude_end", or `"# nocov end"` if unset).
#'   The end marker for `{lintr}` (`settings$exclude_end`) is always included.
#'   Note that the regexes should include the initial comment character `#`.
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
#' code_lines <- "if (FALSE) {\n 2 + 2\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = unreachable_code_linter()
#' )
#'
#' code_lines <- "while (FALSE) {\n 2 + 2\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = unreachable_code_linter()
#' )
#'
#' code_lines <- "f <- function() {\n  return(1)\n  # end skip\n}"
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
#' code_lines <- "if (foo) {\n 2 + 2\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = unreachable_code_linter()
#' )
#'
#' code_lines <- "while (foo) {\n 2 + 2\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = unreachable_code_linter()
#' )
#'
#' code_lines <- "f <- function() {\n  return(1)\n  # end skip\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = unreachable_code_linter(allow_comment_regex = "# end skip")
#' )
#'
#' @evalRd rd_tags("unreachable_code_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
unreachable_code_linter <- function(allow_comment_regex = getOption("covr.exclude_end", "# nocov end")) {
  # nolint next: object_usage_linter. Used in glue() in statically-difficult fashion to detect.
  expr_after_control <- "
    (//REPEAT | //ELSE | //FOR)/following-sibling::expr[1]
    | (//IF | //WHILE)/following-sibling::expr[2]
  "

  unreachable_expr_cond_ws <- "
  following-sibling::*[
    not(self::OP-RIGHT-BRACE or self::OP-SEMICOLON or self::ELSE or preceding-sibling::ELSE)
    and (not(self::COMMENT) or @line2 > preceding-sibling::*[not(self::COMMENT)][1]/@line2)
  ][1]"
  # when a semicolon is present, the condition is a bit different due to <exprlist> nodes
  unreachable_expr_cond_sc <- "
  parent::exprlist[OP-SEMICOLON]
    /following-sibling::*[
      not(self::OP-RIGHT-BRACE)
      and (not(self::COMMENT) or @line1 > preceding-sibling::exprlist/expr/@line2)
    ][1]
  "

  # NB: use not(OP-DOLLAR) to prevent matching process$stop(), #1051
  xpath_return_stop_fmt <- "
  (
    {expr_after_control}
    |
    (//FUNCTION | //OP-LAMBDA)
      /following-sibling::expr[OP-LEFT-BRACE][last()]
  )
    //expr[expr[1][
      not(OP-DOLLAR or OP-AT)
      and SYMBOL_FUNCTION_CALL[text() = 'return' or text() = 'stop']
    ]]
    /{unreachable_expr_cond}
  "
  xpath_return_stop <- paste(
    glue(xpath_return_stop_fmt, unreachable_expr_cond = unreachable_expr_cond_ws),
    glue(xpath_return_stop_fmt, unreachable_expr_cond = unreachable_expr_cond_sc),
    sep = " | "
  )
  xpath_next_break_fmt <- "
  ({expr_after_control})
    //expr[NEXT or BREAK]
    /{unreachable_expr_cond}
  "
  xpath_next_break <- paste(
    glue(xpath_next_break_fmt, unreachable_expr_cond = unreachable_expr_cond_ws),
    glue(xpath_next_break_fmt, unreachable_expr_cond = unreachable_expr_cond_sc),
    sep = " | "
  )

  xpath_if_while <- "
    (//WHILE | //IF)[following-sibling::expr[1]/NUM_CONST[text() = 'FALSE']]
      /parent::expr
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

  drop_valid_comments <- function(expr, valid_comment_re) {
    is_valid_comment <- xml2::xml_name(expr) == "COMMENT" &
      re_matches_logical(xml_text(expr), valid_comment_re)
    expr[!is_valid_comment]
  }

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    # run here because 'settings$exclude_end' may not be set correctly at "compile time".
    # also build with '|', not rex::rex(or(.)), the latter which will double-escape the regex.
    allow_comment_regex <- paste(union(allow_comment_regex, settings$exclude_end), collapse = "|")

    expr_return_stop <- xml_find_all(xml, xpath_return_stop)

    lints_return_stop <- xml_nodes_to_lints(
      drop_valid_comments(expr_return_stop, allow_comment_regex),
      source_expression = source_expression,
      lint_message = "Remove code and comments coming after return() or stop().",
      type = "warning"
    )

    expr_next_break <- xml_find_all(xml, xpath_next_break)

    lints_next_break <- xml_nodes_to_lints(
      drop_valid_comments(expr_next_break, allow_comment_regex),
      source_expression = source_expression,
      lint_message = "Remove code and comments coming after `next` or `break`.",
      type = "warning"
    )

    expr_if_while <- handle_inline_conditions(xml_find_all(xml, xpath_if_while))

    lints_if_while <- xml_nodes_to_lints(
      expr_if_while,
      source_expression = source_expression,
      lint_message = "Remove code inside a conditional loop with a deterministically false condition.",
      type = "warning"
    )

    expr_else <- handle_inline_conditions(xml_find_all(xml, xpath_else))

    lints_else <- xml_nodes_to_lints(
      expr_else,
      source_expression = source_expression,
      lint_message = "Remove code inside an else block after a deterministically true condition.",
      type = "warning"
    )

    c(lints_return_stop, lints_next_break, lints_if_while, lints_else)
  })
}
