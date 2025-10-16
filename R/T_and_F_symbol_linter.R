#' `T` and `F` symbol linter
#'
#' Although they can be synonyms, avoid the symbols `T` and `F`, and use `TRUE` and `FALSE`, respectively, instead.
#' `T` and `F` are not reserved keywords and can be assigned to any other values.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x <- T; y <- F",
#'   linters = T_and_F_symbol_linter()
#' )
#'
#' lint(
#'   text = "T = 1.2; F = 2.4",
#'   linters = T_and_F_symbol_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x <- c(TRUE, FALSE)",
#'   linters = T_and_F_symbol_linter()
#' )
#'
#' lint(
#'   text = "t = 1.2; f = 2.4",
#'   linters = T_and_F_symbol_linter()
#' )
#'
#' @evalRd rd_tags("T_and_F_symbol_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#logical-vectors>
#' @export
T_and_F_symbol_linter <- function() { # nolint: object_name.
  symbol_xpath <- "//SYMBOL[
    (text() = 'T' or text() = 'F')
    and not(parent::expr[OP-DOLLAR or OP-AT])
    and not(parent::expr/following-sibling::OP-LEFT-BRACKET
            or parent::expr/following-sibling::LBB)
    and (
      not(ancestor::expr[OP-TILDE])
      or parent::expr/preceding-sibling::*[not(self::COMMENT)][1][self::EQ_SUB]
    )
  ]"
  assignment_xpath <-
    "parent::expr[following-sibling::LEFT_ASSIGN or preceding-sibling::RIGHT_ASSIGN or following-sibling::EQ_ASSIGN]"

  usage_xpath <- glue("{symbol_xpath}[not({assignment_xpath})]")
  assignment_xpath <- glue("{symbol_xpath}[{assignment_xpath}]")

  replacement_map <- c(T = "TRUE", F = "FALSE")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_usage <- xml_find_all(xml, usage_xpath)
    bad_assignment <- xml_find_all(xml, assignment_xpath)

    make_lints <- function(expr, fmt) {
      symbol <- xml_text(expr)
      lint_message <- sprintf(fmt, replacement_map[symbol], symbol)
      xml_nodes_to_lints(
        xml = expr,
        source_expression = source_expression,
        lint_message = lint_message,
        type = "style",
        column_number_xpath = "number(./@col2 + 1)", # mark at end
        range_end_xpath = "number(./@col2 + 1)" # end after T/F for easy fixing
      )
    }

    c(
      make_lints(bad_usage, "Use %s instead of the symbol %s."),
      make_lints(bad_assignment, "Don't use %2$s as a variable name, as it can break code relying on %2$s being %1$s.")
    )
  })
}
