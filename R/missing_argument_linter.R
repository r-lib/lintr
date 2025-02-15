#' Missing argument linter
#'
#' Check for missing arguments in function calls (e.g. `stats::median(1:10, )`).
#'
#' @param except a character vector of function names as exceptions.
#' @param allow_trailing always allow trailing empty arguments?
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = 'tibble(x = "a", )',
#'   linters = missing_argument_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'tibble(x = "a")',
#'   linters = missing_argument_linter()
#' )
#'
#' lint(
#'   text = 'tibble(x = "a", )',
#'   linters = missing_argument_linter(except = "tibble")
#' )
#'
#' lint(
#'   text = 'tibble(x = "a", )',
#'   linters = missing_argument_linter(allow_trailing = TRUE)
#' )
#'
#' @evalRd rd_tags("missing_argument_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
missing_argument_linter <- function(except = c("alist", "quote", "switch"), allow_trailing = FALSE) {
  conds <- c(
    "self::OP-COMMA[preceding-sibling::*[not(self::COMMENT)][1][self::OP-LEFT-PAREN or self::OP-COMMA]]",
    "self::EQ_SUB[following-sibling::*[not(self::COMMENT)][1][self::OP-RIGHT-PAREN or self::OP-COMMA]]"
  )
  if (!allow_trailing) {
    conds <- c(conds,
      "self::OP-RIGHT-PAREN[preceding-sibling::*[not(self::COMMENT)][1][self::OP-LEFT-PAREN or self::OP-COMMA]]"
    )
  }

  # require >3 children to exclude foo(), which is <expr><OP-LEFT-PAREN><OP-RIGHT-PAREN>
  xpath <- glue("
  parent::expr[count(*) > 3]
    /*[{xp_or(conds)}]
  ")

  Linter(linter_level = "file", function(source_expression) {
    xml_targets <- source_expression$xml_find_function_calls(NULL, keep_names = TRUE)
    xml_targets <- xml_targets[!names(xml_targets) %in% except]

    missing_args <- xml_find_all(xml_targets, xpath)

    named_idx <- xml_name(missing_args) == "EQ_SUB"
    arg_id <- character(length(missing_args))
    arg_id[named_idx] <- sQuote(xml_find_chr(missing_args[named_idx], "string(preceding-sibling::SYMBOL_SUB[1])"), "'")
    # TODO(#2452): use xml_find_int() instead
    arg_id[!named_idx] <- xml_find_num(missing_args[!named_idx], "count(preceding-sibling::OP-COMMA)") + 1.0

    xml_nodes_to_lints(
      missing_args,
      source_expression = source_expression,
      lint_message = sprintf("Missing argument %s in function call.", arg_id)
    )
  })
}
