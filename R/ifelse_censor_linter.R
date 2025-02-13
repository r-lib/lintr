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
#' @examples
#' # will produce lints
#' lint(
#'   text = "ifelse(5:1 < pi, 5:1, pi)",
#'   linters = ifelse_censor_linter()
#' )
#'
#' lint(
#'   text = "ifelse(x > 0, x, 0)",
#'   linters = ifelse_censor_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "pmin(5:1, pi)",
#'   linters = ifelse_censor_linter()
#' )
#'
#' lint(
#'   text = "pmax(x, 0)",
#'   linters = ifelse_censor_linter()
#' )
#'
#' @evalRd rd_tags("ifelse_censor_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
ifelse_censor_linter <- function() {
  xpath <- glue("
  following-sibling::expr[
    (LT or GT or LE or GE)
    and expr[1] = following-sibling::expr
    and expr[2] = following-sibling::expr
  ]
    /parent::expr
  ")

  Linter(linter_level = "expression", function(source_expression) {
    ifelse_calls <- source_expression$xml_find_function_calls(ifelse_funs)
    bad_expr <- xml_find_all(ifelse_calls, xpath)

    matched_call <- xp_call_name(bad_expr)
    operator <- xml_find_chr(bad_expr, "string(expr[2]/*[2])")
    match_first <- !is.na(xml_find_first(bad_expr, "expr[2][expr[1] = following-sibling::expr[1]]"))
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
