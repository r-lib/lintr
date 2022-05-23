#' Undesirable operator linter
#'
#' Report the use of undesirable operators, e.g. [`:::`][base::ns-dblcolon] or
#' [`<<-`][base::assignOps] and suggest an alternative.
#'
#' @param op Named character vector, where the names are the names of the undesirable operators, and the values are
#'   the text for the alternative operator to use (or `NA`).
#' @evalRd rd_tags("undesirable_operator_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
undesirable_operator_linter <- function(op = default_undesirable_operators) {
  undesirable_operator_metadata <- merge(
    # must be handled individually below
    infix_metadata[infix_metadata$string_value != "%%", ],
    infix_overload,
    by = "xml_tag", all.x = TRUE
  )

  included_operators <- undesirable_operator_metadata$string_value %in% names(op) |
    undesirable_operator_metadata$exact_string_value %in% names(op)
  operator_nodes <- undesirable_operator_metadata$xml_tag[included_operators]
  needs_exact_string <- !is.na(undesirable_operator_metadata$exact_string_value[included_operators])
  operator_nodes[needs_exact_string] <- sprintf(
    "%s[text() = '%s']",
    operator_nodes[needs_exact_string],
    undesirable_operator_metadata$exact_string_value[included_operators][needs_exact_string]
  )

  is_infix <- startsWith(names(op), "%")
  if (any(is_infix)) {
    operator_nodes <- c(operator_nodes, sprintf("SPECIAL[text() = '%s']", names(op)[is_infix]))
  }

  if (length(operator_nodes) == 0L) {
    stop("Did not recognize any valid operators in request for: ", toString(names(op)))
  }

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    xpath <- glue::glue("//*[ { paste0('self::', operator_nodes, collapse = ' or ') } ]")
    bad_op <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_op,
      source_expression = source_expression,
      lint_message = function(expr) {
        op_name <- xml2::xml_text(expr)
        msg <- sprintf("Operator `%s` is undesirable.", op_name)
        alt_op <- op[[op_name]]
        if (!is.na(alt_op)) {
          msg <- paste(msg, alt_op)
        }
        msg
      },
      type = "warning"
    )
  })
}
