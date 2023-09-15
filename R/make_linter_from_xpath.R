#' Create a linter from an XPath
#'
#' @inheritParams xml_nodes_to_lints
#' @inheritParams is_lint_level
#' @param xpath Character string, an XPath identifying R code to lint.
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

  stopifnot(
    "xpath should be a character string" = is.character(xpath) && length(xpath) == 1L && !is.na(xpath)
  )

  xml_key <- if (level == "expression") "xml_parsed_content" else "full_xml_parsed_content"

  function() {
    Linter(function(source_expression) {
      if (!is_lint_level(source_expression, level)) {
        return(list())
      }

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
