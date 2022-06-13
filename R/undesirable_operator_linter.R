#' Undesirable operator linter
#'
#' Report the use of undesirable operators, e.g. \code{\link[base:ns-dblcolon]{:::}} or
#' [`<<-`][base::assignOps] and suggest an alternative.
#'
#' @param op Named character vector. `names(op)` correspond to undesirable operators,
#'   while the values give a description of why the operator is undesirable.
#'   If `NA`, no additional information is given in the lint message. Defaults to
#'   [default_undesirable_operators]. To make small customizations to this list,
#'   use [modify_defaults()].
#' @evalRd rd_tags("undesirable_operator_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
undesirable_operator_linter <- function(op = default_undesirable_operators) {
  undesirable_operator_metadata <- merge(
    # infix must be handled individually below; non-assignment `=` are always OK
    infix_metadata[
      infix_metadata$string_value != "%%" & !infix_metadata$xml_tag %in% c("EQ_SUB", "EQ_FORMALS"),
    ],
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

  xpath <- paste(paste0("//", operator_nodes), collapse = " | ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_op <- xml2::xml_find_all(xml, xpath)

    operator <- xml2::xml_text(bad_op)
    lint_message <- sprintf("Operator `%s` is undesirable.", operator)
    alternative <- op[operator]
    has_alternative <- !is.na(alternative)
    lint_message[has_alternative] <- paste(lint_message[has_alternative], alternative[has_alternative])

    xml_nodes_to_lints(bad_op, source_expression, lint_message, type = "warning")
  })
}
