#' Create a linter from an XPath
#'
#' @inheritParams xml_nodes_to_lints
#' @inheritParams is_lint_level
#' @param xpath Character string, an XPath identifying R code to lint.
#'   For `make_linter_from_function_xpath()`, the XPath is relative to the `parent::expr` of the
#'   `SYMBOL_FUNCTION_CALL` nodes of the selected functions.
#'   See [xmlparsedata::xml_parse_data()] and [get_source_expressions()].
#'
#' @examples
#' number_linter <- make_linter_from_xpath("//NUM_CONST", "This is a number.")
#' lint(text = "1 + 2", linters = number_linter())
#' @export
make_linter_from_xpath <- function(xpath,
                                   lint_message,
                                   type = c("warning", "style", "error"),
                                   level = c("expression", "file")) {
  type <- match.arg(type)
  level <- match.arg(level)

  if (!is.character(xpath) || length(xpath) != 1L || is.na(xpath)) {
    cli_abort("{.arg xpath} should be a character string.")
  }
  if (missing(lint_message)) {
    cli_abort("{.arg lint_message} is required.")
  }

  xml_key <- if (level == "expression") "xml_parsed_content" else "full_xml_parsed_content"

  function() {
    Linter(linter_level = level, function(source_expression) {
      xml <- source_expression[[xml_key]]


      expr <- xml_find_all(xml, xpath)

      xml_nodes_to_lints(
        expr,
        source_expression = source_expression,
        lint_message = lint_message,
        type = type
      )
    })
  }
}

#' @rdname make_linter_from_xpath
#' @param function_names Character vector, names of functions whose calls to examine..
#' @export
# nolint next: object_length.
make_linter_from_function_xpath <- function(function_names,
                                            xpath,
                                            lint_message,
                                            type = c("warning", "style", "error"),
                                            level = c("expression", "file")) {
  type <- match.arg(type)
  level <- match.arg(level)

  if (!is.character(function_names) || length(function_names) == 0L) {
    cli_abort("{.arg function_names} should be a character vector.")
  }
  if (!is.character(xpath) || length(xpath) != 1L || is.na(xpath)) {
    cli_abort("{.arg xpath} should be a character string.")
  }
  if (missing(lint_message)) {
    cli_abort("{.arg lint_message} is required.")
  }

  function() {
    Linter(linter_level = level, function(source_expression) {
      call_xml <- source_expression$xml_find_function_calls(function_names)

      expr <- xml_find_all(call_xml, xpath)

      xml_nodes_to_lints(
        expr,
        source_expression = source_expression,
        lint_message = lint_message,
        type = type
      )
    })
  }
}
