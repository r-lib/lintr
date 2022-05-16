#' Block comparison of class with ==
#'
#' Usage like `class(x) == "character"` is prone to error since class in R
#'   is in general a vector. The correct version for S3 classes is [inherits()]:
#'   `inherits(x, "character")`. Often, class `k` will have an `is.` equivalent,
#'   for example [is.character()] or [is.data.frame()].
#'
#' Similar reasoning applies for `class(x) %in% "character"`
#'
#' @evalRd rd_tags("class_equals_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
class_equals_linter <- function() {
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression", require_xml = TRUE)) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    xpath <- "//expr[
      not(preceding-sibling::OP-LEFT-BRACKET)
      and expr[expr[SYMBOL_FUNCTION_CALL[text() = 'class']]]
      and (EQ or NE or SPECIAL[text() = '%in%'])
    ]"

    bad_expr <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_expression = source_expression,
      lint_message = function(expr) {
        op <- xml2::xml_text(xml2::xml_find_first(expr, "*[2]"))
        message <- sprintf("Instead of comparing class(x) with %s,", op)
        paste(message, "use inherits(x, 'class-name') or is.<class> or is(x, 'class')")
      },
      type = "warning"
    ))
  })
}
