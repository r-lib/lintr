#' `T` and `F` symbol linter
#'
#' Avoid the symbols `T` and `F` (for `TRUE` and `FALSE`).
#'
#' @evalRd rd_tags("T_and_F_symbol_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#logical-vectors>
#' @export
T_and_F_symbol_linter <- function() { # nolint: object_name.
  xpath <- paste0(
    "//SYMBOL[",
    "(text() = 'T' or text() = 'F')", # T or F symbol
    " and not(preceding-sibling::OP-DOLLAR)", # not part of a $-subset expression
    " and not(parent::expr[",
    "  following-sibling::LEFT_ASSIGN", # not target of left assignment
    "  or preceding-sibling::RIGHT_ASSIGN", # not target of right assignment
    "  or following-sibling::EQ_ASSIGN", # not target of equals assignment
    "])",
    "]"
  )

  xpath_assignment <- paste0(
    "//SYMBOL[",
    "(text() = 'T' or text() = 'F')", # T or F symbol
    " and not(preceding-sibling::OP-DOLLAR)", # not part of a $-subset expression
    " and parent::expr[", #, but ...
    "  following-sibling::LEFT_ASSIGN", # target of left assignment
    "  or preceding-sibling::RIGHT_ASSIGN", # target of right assignment
    "  or following-sibling::EQ_ASSIGN", # target of equals assignment
    " ]",
    "]"
  )

  replacement_map <- c(T = "TRUE", F = "FALSE")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    bad_exprs <- xml2::xml_find_all(source_expression$xml_parsed_content, xpath)
    bad_assigns <- xml2::xml_find_all(source_expression$xml_parsed_content, xpath_assignment)

    make_lints <- function(expr, fmt) {
      symbol <- xml2::xml_text(expr)
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
      make_lints(bad_exprs, "Use %s instead of the symbol %s."),
      make_lints(bad_assigns, "Don't use %2$s as a variable name, as it can break code relying on %2$s being %1$s.")
    )
  })
}
