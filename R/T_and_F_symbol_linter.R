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
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression", require_xml = TRUE)) {
      return(list())
    }

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

    bad_exprs <- xml2::xml_find_all(source_expression$xml_parsed_content, xpath)
    bad_assigns <- xml2::xml_find_all(source_expression$xml_parsed_content, xpath_assignment)

    replacement_map <- c(T = "TRUE", F = "FALSE")
    make_lint <- function(expr, fmt) {
      xml_nodes_to_lints(
        xml = expr,
        source_expression = source_expression,
        lint_message = function(expr) {
          symbol <- xml2::xml_text(expr)
          sprintf(fmt, replacement_map[[symbol]], symbol)
        },
        type = "style",
        offset = 1L
      )
    }

    c(
      make_lint(bad_exprs, fmt = "Use %s instead of the symbol %s."),
      make_lint(bad_assigns, fmt = "Don't use %2$s as a variable name, as it can break code relying on %2$s being %1$s.")
    )
  })
}
