#' Redirect `is.numeric(x) || is.integer(x)` to just use `is.numeric(x)`
#'
#' [is.numeric()] returns `TRUE` when `typeof(x)` is `double` or `integer` --
#'   testing `is.numeric(x) || is.integer(x)` is thus redundant.
#'
#' @evalRd rd_tags("is_numeric_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
is_numeric_linter <- function() {
  # TODO(michaelchirico): this should also cover is.double(x) || is.integer(x), ditto below
  is_numeric_expr <- "expr[1][SYMBOL_FUNCTION_CALL[text() = 'is.numeric']]"
  is_integer_expr <- "expr[1][SYMBOL_FUNCTION_CALL[text() = 'is.integer']]"

  # testing things like is.numeric(x) || is.integer(x)
  or_xpath <- glue::glue("
  //OR2
  /parent::expr[
    expr/{is_numeric_expr}
    and expr/{is_integer_expr}
    and
      expr/{is_numeric_expr}/following-sibling::expr[1]
      = expr/{is_integer_expr}/following-sibling::expr[1]
  ]
  ")

  # testing class(x) %in% c("numeric", "integer")
  # TODO(michaelchirico): use get_r_string here
  # TODO(michaelchirico): include typeof(x) %in% c("integer", "double")
  class_xpath <- "
  //SPECIAL[
    text() = '%in%'
    and following-sibling::expr[
      expr/SYMBOL_FUNCTION_CALL[text() = 'c']
      and expr/STR_CONST[substring(text(), 2, 7) = 'integer']
      and expr/STR_CONST[substring(text(), 2, 7) = 'numeric']
      and count(expr) = 3
    ]
  ]
  /preceding-sibling::expr[expr/SYMBOL_FUNCTION_CALL[text() = 'class']]
  /parent::expr
  "

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    or_expr <- xml2::xml_find_all(xml, or_xpath)
    or_lints <- xml_nodes_to_lints(
      or_expr,
      source_expression = source_expression,
      lint_message = paste(
        "is.numeric(x) is the same as is.numeric(x) || is.integer(x).",
        "Use is.double(x) to test for objects stored as 64-bit floating point."
      ),
      type = "warning"
    )

    class_expr <- xml2::xml_find_all(xml, class_xpath)
    class_lints <- xml_nodes_to_lints(
      class_expr,
      source_expression = source_expression,
      lint_message = paste(
        'is.numeric(x) is the same as class(x) %in% c("integer", "numeric").',
        "Use is.double(x) to test for objects stored as 64-bit floating point."
      ),
      type = "warning"
    )

    c(or_lints, class_lints)
  })
}
