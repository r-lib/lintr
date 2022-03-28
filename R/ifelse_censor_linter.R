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
  Linter(function(source_file) {
    if (length(source_file$xml_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    xpath <- glue::glue("//expr[
      expr[SYMBOL_FUNCTION_CALL[ {xp_text_in_table(ifelse_funs)} ]]
      and expr[2][
        (LT or GT or LE or GE)
        and expr[1] = following-sibling::expr
        and expr[2] = following-sibling::expr
      ]
    ]")
    bad_expr <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = paste(
        "pmin(x, y) is preferable to ifelse(x < y, x, y);",
        "pmax(x, y) is preferable to ifelse(x > y, x, y).",
        "This reasoning also extends to using ifelse to censor a vector",
        "(from above or below)."
      ),
      type = "warning"
    ))
  })
}
