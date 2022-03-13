#' Assignment linter
#'
#' Check that `<-` is always used for assignment.
#'
#' @param allow_cascading_assign Logical, default `TRUE`.
#'   If `FALSE`, [`<<-`][base::assignOps] and `->>` are not allowed.
#' @param allow_right_assign Logical, default `FALSE`. If `TRUE`, `->` and `->>` are allowed.
#' @evalRd rd_tags("assignment_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
assignment_linter <- function(allow_cascading_assign = TRUE, allow_right_assign = FALSE) {
  Linter(function(source_file) {
    if (is.null(source_file$xml_parsed_content)) return(list())

    xml <- source_file$xml_parsed_content

    # TODO: is there any performance consideration for //*[self::a or self::b] vs. //a|//b ?
    xpath <- paste(collapse = " | ", c(
      # always block = (NB: the parser differentiates EQ_ASSIGN, EQ_SUB, and EQ_FORMALS)
      "//EQ_ASSIGN",
      # -> and ->> are both 'RIGHT_ASSIGN'
      if (!allow_right_assign) "//RIGHT_ASSIGN" else if (!allow_cascading_assign) "//RIGHT_ASSIGN[text() = '->>']",
      # <-, :=, and <<- are all 'LEFT_ASSIGN'; check the text if blocking <<-
      if (!allow_cascading_assign) "//LEFT_ASSIGN[text() = '<<-']"
    ))

    bad_expr <- xml2::xml_find_all(xml, xpath)
    lapply(bad_expr, gen_assignment_lint, source_file)
  })
}

gen_assignment_lint <- function(expr, source_file) {
  operator <- xml2::xml_text(expr)
  if (operator %in% c("<<-", "->>")) {
    message <- sprintf(
      "%s can have hard-to-predict behavior; prefer assigning to a specific environment instead (with assign() or <-).",
      operator
    )
  } else {
    message <- sprintf("Use <-, not %s, for assignment.", operator)
  }
  xml_nodes_to_lint(expr, source_file, message, type = "style")
}
