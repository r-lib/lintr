#' Block usage like x %in% "a"
#'
#' `vector %in% set` is appropriate for matching a vector to a set, but if
#'   that set has size 1, `==` is more appropriate. However, if `vector` has
#'   also size 1 and can be `NA`, the use of `==` should be accompanied by extra
#'   protection for the missing case (for example, `isTRUE(NA == "arg")` or
#'   `!is.na(x) && x == "arg"`).
#'
#' `scalar %in% vector` is OK, because the alternative (`any(vector == scalar)`)
#'   is more circuitous & potentially less clear.
#'
#' @param in_operators Character vector of additional infix operators that behave like the `%in%` operator,
#'   e.g. `{data.table}`'s `%chin%` operator.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x %in% 1L",
#'   linters = scalar_in_linter()
#' )
#'
#' lint(
#'   text = "x %chin% 'a'",
#'   linters = scalar_in_linter(in_operators = "%chin%")
#' )
#'
#' # okay
#' lint(
#'   text = "x %in% 1:10",
#'   linters = scalar_in_linter()
#' )
#'
#' @evalRd rd_tags("scalar_in_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
scalar_in_linter <- function(in_operators = NULL) {
  # TODO(#2085): Extend to include other cases where the RHS is clearly a scalar
  # NB: all of logical, integer, double, hex, complex are parsed as NUM_CONST
  xpath <- glue("
  //SPECIAL[{xp_text_in_table(c('%in%', {in_operators}))}]
    /following-sibling::expr[NUM_CONST[not(starts-with(text(), 'NA'))] or STR_CONST]
    /parent::expr
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)
    in_op <- xml_find_chr(bad_expr, "string(SPECIAL)")
    lint_msg <- paste0(
      "Use comparison operators (e.g. ==, !=, etc.) to match length-1 scalars instead of ", in_op, ". ",
      "Note that if x can be NA, x == 'arg' is NA whereas x ", in_op, " 'arg' is FALSE, ",
      "so consider extra protection for the missing case in your code."
    )

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = lint_msg,
      type = "warning"
    )
  })
}
