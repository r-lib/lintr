#' Assignment linter
#'
#' Check that `<-` is always used for assignment.
#'
#' @param allow_cascading_assign Logical, default `TRUE`.
#'   If `FALSE`, [`<<-`][base::assignOps] and `->>` are not allowed.
#' @param allow_right_assign Logical, default `FALSE`. If `TRUE`, `->` and `->>` are allowed.
#' @param allow_trailing Logical, default `TRUE`. If `FALSE` then assignments aren't allowed at end of lines.
#' @evalRd rd_tags("assignment_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#assignment-1>
#' @export
assignment_linter <- function(allow_cascading_assign = TRUE, allow_right_assign = FALSE, allow_trailing = TRUE) {
  trailing_assign_xpath <- paste(
    collapse = " | ",
    c(
      paste0("//LEFT_ASSIGN", if (allow_cascading_assign) "" else "[text() = '<-']"),
      if (allow_right_assign) paste0("//RIGHT_ASSIGN", if (allow_cascading_assign) "" else "[text() = '->']"),
      "//EQ_SUB",
      "//EQ_FORMALS"
    ),
    "[@line1 < following-sibling::*[1]/@line1 or //COMMENT]"
  )

  xpath <- paste(collapse = " | ", c(
    # always block = (NB: the parser differentiates EQ_ASSIGN, EQ_SUB, and EQ_FORMALS)
    "//EQ_ASSIGN",
    # -> and ->> are both 'RIGHT_ASSIGN'
    if (!allow_right_assign) "//RIGHT_ASSIGN" else if (!allow_cascading_assign) "//RIGHT_ASSIGN[text() = '->>']",
    # <-, :=, and <<- are all 'LEFT_ASSIGN'; check the text if blocking <<-.
    # NB: := is not linted because of (1) its common usage in rlang/data.table and
    #   (2) it's extremely uncommon as a normal assignment operator
    if (!allow_cascading_assign) "//LEFT_ASSIGN[text() = '<<-']",
    if (!allow_trailing) trailing_assign_xpath
  ))

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)
    if (length(bad_expr) == 0L) {
      return(list())
    }

    operator <- xml2::xml_text(bad_expr)
    lint_message_fmt <- ifelse(
      operator %in% c("<<-", "->>"),
      "%s can have hard-to-predict behavior; prefer assigning to a specific environment instead (with assign() or <-).",
      "Use <-, not %s, for assignment."
    )

    if (!allow_trailing) {
      bad_trailing_expr <- xml2::xml_find_all(xml, trailing_assign_xpath)
      trailing_assignments <- xml2::xml_attrs(bad_expr) %in% xml2::xml_attrs(bad_trailing_expr)
      lint_message_fmt[trailing_assignments] <- "Assignment %s should not be trailing at end of line"
    }

    lint_message <- sprintf(lint_message_fmt, operator)
    xml_nodes_to_lints(bad_expr, source_expression, lint_message, type = "style")
  })
}
