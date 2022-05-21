#' Block usage of ifelse where pmin or pmax is more appropriate
#'
#' `ifelse(x > M, M, x)` is the same as `pmin(x, M)`, but harder
#'   to read and requires several passes over the vector.
#'
#' The same goes for other similar ways to censor a vector, e.g.
#'   `ifelse(x <= M, x, M)` is `pmin(x, M)`,
#'   `ifelse(x < m, m, x)` is `pmax(x, m)`, and
#'   `ifelse(x >= m, x, m)` is `pmax(x, m)`.
#'
#' @evalRd rd_tags("ifelse_censor_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
ifelse_censor_linter <- function() {
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    xpath <- glue::glue("//expr[
      expr[SYMBOL_FUNCTION_CALL[ {xp_text_in_table(ifelse_funs)} ]]
      and expr[2][
        (LT or GT or LE or GE)
        and expr[1] = following-sibling::expr
        and expr[2] = following-sibling::expr
      ]
    ]")
    bad_expr <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = function(expr) {
        matched_call <- xml2::xml_text(xml2::xml_find_first(expr, "expr/SYMBOL_FUNCTION_CALL"))
        op <- xml2::xml_text(xml2::xml_find_first(expr, "expr[2]/*[2]"))
        match_first <- !is.na(xml2::xml_find_first(expr, "expr[2][expr[1] = following-sibling::expr[1]]"))
        if (op %in% c("<", "<=")) {
          if (match_first) {
            sprintf("pmin(x, y) is preferable to %s(x %s y, x, y).", matched_call, op)
          } else {
            sprintf("pmax(x, y) is preferable to %s(x %s y, y, x).", matched_call, op)
          }
        } else {
          if (match_first) {
            sprintf("pmax(x, y) is preferable to %s(x %s y, x, y).", matched_call, op)
          } else {
            sprintf("pmin(x, y) is preferable to %s(x %s y, y, x).", matched_call, op)
          }
        }
      },
      type = "warning"
    )
  })
}
