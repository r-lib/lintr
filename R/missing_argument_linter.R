#' Missing argument linter
#'
#' Check for missing arguments in function calls.
#' @param except a character vector of function names as exceptions.
#' @param allow_trailing always allow trailing empty arguments?
#' @evalRd rd_tags("missing_argument_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
missing_argument_linter <- function(except = c("switch", "alist"), allow_trailing = FALSE) {

  conds <- c(
    "self::OP-COMMA[preceding-sibling::*[not(self::COMMENT)][1][self::OP-LEFT-PAREN or self::OP-COMMA]]",
    "self::EQ_SUB[following-sibling::*[not(self::COMMENT)][1][self::OP-RIGHT-PAREN or self::OP-COMMA]]"
  )
  if (!allow_trailing) {
    conds <- c(conds, "self::OP-COMMA[following-sibling::*[not(self::COMMENT)][1][self::OP-RIGHT-PAREN]]")
  }

  xpath <- glue::glue("//SYMBOL_FUNCTION_CALL/parent::expr/parent::expr/*[{xp_or(conds)}]")
  to_function_xpath <- "string(./preceding-sibling::expr[last()]/SYMBOL_FUNCTION_CALL)"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    missing_args <- xml2::xml_find_all(xml, xpath)
    function_call_name <- get_r_string(xml2::xml_find_chr(missing_args, to_function_xpath))

    xml_nodes_to_lints(
      missing_args[!function_call_name %in% except],
      source_expression = source_expression,
      lint_message = "Missing argument in function call."
    )
  })
}
