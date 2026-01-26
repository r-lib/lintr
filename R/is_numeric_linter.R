#' Redirect `is.numeric(x) || is.integer(x)` to just use `is.numeric(x)`
#'
#' [is.numeric()] returns `TRUE` when `typeof(x)` is `double` or `integer` --
#'   testing `is.numeric(x) || is.integer(x)` is thus redundant.
#'
#' NB: This linter plays well with [class_equals_linter()], which can help
#'   avoid further `is.numeric()` equivalents like
#'   `any(class(x) == c("numeric", "integer"))`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "is.numeric(y) || is.integer(y)",
#'   linters = is_numeric_linter()
#' )
#'
#' lint(
#'   text = 'class(z) %in% c("numeric", "integer")',
#'   linters = is_numeric_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "is.numeric(y) || is.factor(y)",
#'   linters = is_numeric_linter()
#' )
#'
#' lint(
#'   text = 'class(z) %in% c("numeric", "integer", "factor")',
#'   linters = is_numeric_linter()
#' )
#'
#' @evalRd rd_tags("is_numeric_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
is_numeric_linter <- function() {
  # TODO(#2469): This should also cover is.double(x) || is.integer(x).
  # TODO(#1636): is.numeric(x) || is.integer(x) || is.factor(x) is also redundant.
  # TODO(#2470): Consider usages with class(), typeof(), or inherits().
  is_numeric_expr <- "expr[1][SYMBOL_FUNCTION_CALL[text() = 'is.numeric']]"
  is_integer_expr <- "expr[1][SYMBOL_FUNCTION_CALL[text() = 'is.integer']]"

  # testing things like is.numeric(x) || is.integer(x)
  or_xpath <- glue("
  self::*[
    expr/{is_numeric_expr}
    and expr/{is_integer_expr}
    and
      expr/{is_numeric_expr}/following-sibling::expr[1]
      = expr/{is_integer_expr}/following-sibling::expr[1]
  ]")

  # testing class(x) %in% c("numeric", "integer")
  class_xpath <- "
  //SPECIAL[
    text() = '%in%'
    and following-sibling::expr[
      expr/SYMBOL_FUNCTION_CALL[text() = 'c']
      and count(expr/STR_CONST) = 2
      and count(expr) = 3
    ]
    and preceding-sibling::expr/expr/SYMBOL_FUNCTION_CALL[text() = 'class']
  ]
    /parent::expr
  "

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    or_expr <- xml_find_all(xml, "//OR2/parent::*")
    or_expr <- or_expr |>
      strip_comments_from_subtree() |>
      xml_find_all(or_xpath)
    or_lints <- xml_nodes_to_lints(
      or_expr,
      source_expression = source_expression,
      lint_message = paste(
        "Use `is.numeric(x)` instead of the equivalent `is.numeric(x) || is.integer(x)`.",
        "Use is.double(x) to test for objects stored as 64-bit floating point."
      ),
      type = "warning"
    )

    class_expr <- xml_find_all(xml, class_xpath)
    if (length(class_expr) > 0L) {
      class_strings <- c(
        get_r_string(class_expr, "expr[2]/expr[2]/STR_CONST"),
        get_r_string(class_expr, "expr[2]/expr[3]/STR_CONST")
      )
      is_lintable <- "integer" %in% class_strings && "numeric" %in% class_strings
      class_expr <- class_expr[is_lintable]
    }
    class_lints <- xml_nodes_to_lints(
      class_expr,
      source_expression = source_expression,
      lint_message = paste(
        'Use is.numeric(x) instead of class(x) %in% c("integer", "numeric").',
        "Use is.double(x) to test for objects stored as 64-bit floating point."
      ),
      type = "warning"
    )

    c(or_lints, class_lints)
  })
}
