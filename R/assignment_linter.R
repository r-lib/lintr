#' Assignment linter
#'
#' Check that `<-` is always used for assignment.
#'
#' @param allow_cascading_assign Logical, default `TRUE`.
#'   If `FALSE`, [`<<-`][base::assignOps] and `->>` are not allowed.
#' @param allow_right_assign Logical, default `FALSE`. If `TRUE`, `->` and `->>` are allowed.
#' @evalRd rd_tags("assignment_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#assignment-1>
#' @export
assignment_linter <- function(allow_cascading_assign = TRUE, allow_right_assign = FALSE) {
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    xpath <- paste(collapse = " | ", c(
      # always block = (NB: the parser differentiates EQ_ASSIGN, EQ_SUB, and EQ_FORMALS)
      "//EQ_ASSIGN",
      # -> and ->> are both 'RIGHT_ASSIGN'
      if (!allow_right_assign) "//RIGHT_ASSIGN" else if (!allow_cascading_assign) "//RIGHT_ASSIGN[text() = '->>']",
      # <-, :=, and <<- are all 'LEFT_ASSIGN'; check the text if blocking <<-.
      # NB: := is not linted because of (1) its common usage in rlang/data.table and
      #   (2) it's extremely uncommon as a normal assignment operator
      if (!allow_cascading_assign) "//LEFT_ASSIGN[text() = '<<-']"
    ))

    bad_expr <- xml2::xml_find_all(xml, xpath)
    xml_nodes_to_lints(
      bad_expr,
      source_expression,
      function(expr) {
        operator <- xml2::xml_text(expr)
        if (operator %in% c("<<-", "->>")) {
          paste(
            operator,
            "can have hard-to-predict behavior;",
            "prefer assigning to a specific environment instead (with assign() or <-)."
          )
        } else {
          paste0("Use <-, not ", operator, ", for assignment.")
        }
      },
      type = "style"
    )
  })
}
