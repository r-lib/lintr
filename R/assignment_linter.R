#' Assignment linter
#'
#' Check that `<-` is always used for assignment.
#'
#' @param block_double_assign Logical, `FALSE` by default. If `TRUE`, usage
#'   of [`<<-`][base::assignOps] and [`->>`][base::assignOps] are also blocked.
#' @evalRd rd_tags("assignment_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
assignment_linter <- function(block_double_assign = FALSE) {
  Linter(function(source_file) {
    if (is.null(source_file$xml_parsed_content)) return(list())

    xml <- source_file$xml_parsed_content

    node_conditions <- paste0("self::", c(
      # always block = (NB: the parser differentiates EQ_ASSIGN and EQ_SUB)
      "EQ_ASSIGN",
      # -> and ->> are both 'RIGHT_ASSIGN'; check the text if not blocking ->>
      paste0("RIGHT_ASSIGN", if (!block_double_assign) "[text() = '->']"),
      # <-, :=, and <<- are all 'LEFT_ASSIGN'; check the text if blocking <<-
      if (block_double_assign) "LEFT_ASSIGN[text() = '<<-']"
    ))
    # TODO: is there any performance consideration for //*[self::a or self::b] vs. //a|//b ?
    xpath <- sprintf("//*[%s]", do.call(xp_or, as.list(node_conditions)))

    bad_expr <- xml2::xml_find_all(xml, xpath)
    lapply(bad_expr, gen_assignment_lint, source_file)
  })
}

gen_assignment_lint <- function(expr, source_file) {
  operator <- xml2::xml_text(expr)
  message <- sprintf("Use <-, not %s, for assignment.", operator)
  if (operator %in% c("<<-", "->>")) message <- paste(message, "Assign to specific environments instead.")
  xml_nodes_to_lint(expr, source_file, message, type = "style")
}
