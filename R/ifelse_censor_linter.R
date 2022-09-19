#' Block usage of `ifelse()` where `pmin()` or `pmax()` is more appropriate
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
  xpath <- glue::glue("//expr[
    expr[1][SYMBOL_FUNCTION_CALL[ {xp_text_in_table(ifelse_funs)} ]]
    and expr[2][
      (LT or GT or LE or GE)
      and expr[1] = following-sibling::expr
      and expr[2] = following-sibling::expr
    ]
  ]")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    matched_call <- xp_call_name(bad_expr)
    operator <- xml2::xml_find_chr(bad_expr, "string(expr[2]/*[2])")
    match_first <- !is.na(xml2::xml_find_first(bad_expr, "expr[2][expr[1] = following-sibling::expr[1]]"))
    optimizer <- ifelse((operator %in% c("<", "<=")) == match_first, "pmin", "pmax")
    first_var <- rep_len("x", length(match_first))
    second_var <- rep_len("y", length(match_first))
    first_var[!match_first] <- "y"
    second_var[!match_first] <- "x"

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = sprintf(
        "%s(x, y) is preferable to %s(x %s y, %s, %s).",
        optimizer, matched_call, operator, first_var, second_var
      ),
      type = "warning"
    )
  })
}
