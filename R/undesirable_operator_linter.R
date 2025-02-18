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
#' @param call_is_undesirable Logical, default `TRUE`. Should function-style usage of
#'   the provided operators also generate a lint?
#'
#' @examples
#' # defaults for which functions are considered undesirable
#' names(default_undesirable_operators)
#'
#' # will produce lints
#' lint(
#'   text = "a <<- log(10)",
#'   linters = undesirable_operator_linter()
#' )
#'
#' lint(
#'   text = "mtcars$wt",
#'   linters = undesirable_operator_linter(op = c("$" = "As an alternative, use the `[[` accessor."))
#' )
#'
#' lint(
#'   text = "`:::`(utils, hasName)",
#'   linters = undesirable_operator_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "a <- log(10)",
#'   linters = undesirable_operator_linter()
#' )
#' lint(
#'   text = 'mtcars[["wt"]]',
#'   linters = undesirable_operator_linter(op = c("$" = NA))
#' )
#'
#' lint(
#'   text = 'mtcars[["wt"]]',
#'   linters = undesirable_operator_linter(op = c("$" = "As an alternative, use the `[[` accessor."))
#' )
#'
#' lint(
#'   text = "`:::`(utils, hasName)",
#'   linters = undesirable_operator_linter(call_is_undesirable = FALSE)
#' )
#'
#' @evalRd rd_tags("undesirable_operator_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
undesirable_operator_linter <- function(op = default_undesirable_operators,
                                        call_is_undesirable = TRUE) {
  if (is.null(names(op)) || !all(nzchar(names(op))) || length(op) == 0L) {
    cli_abort(c(
      x = "{.arg op} should be a non-empty named character vector.",
      i = "Use missing elements to indicate default messages."
    ))
  }
  # infix must be handled individually below; non-assignment `=` are always OK
  operator_nodes <- infix_metadata$xml_tag_exact[
    infix_metadata$string_value %in% setdiff(names(op), "%%") &
      !infix_metadata$xml_tag %in% c("EQ_SUB", "EQ_FORMALS")
  ]

  is_infix <- startsWith(names(op), "%")
  if (any(is_infix)) {
    operator_nodes <- c(operator_nodes, sprintf("SPECIAL[text() = '%s']", names(op)[is_infix]))
  }

  if (length(operator_nodes) == 0L) {
    cli_abort("Did not recognize any valid operators in request for: {.str {names(op)}}")
  }

  infix_xpath <- paste(paste0("//", operator_nodes), collapse = " | ")

  quoted_op <- paste0("`", names(op), "`")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    op_calls <- source_expression$xml_find_function_calls(quoted_op)

    infix_expr <- xml_find_all(xml, infix_xpath)

    operator <- c(xml_text(infix_expr), gsub("^`|`$", "", xml_text(op_calls)))
    lint_message <- sprintf("Avoid undesirable operator `%s`.", operator)
    alternative <- op[operator]
    has_alternative <- !is.na(alternative)
    lint_message[has_alternative] <- paste(lint_message[has_alternative], alternative[has_alternative])

    xml_nodes_to_lints(
      combine_nodesets(infix_expr, op_calls),
      source_expression,
      lint_message,
      type = "warning"
    )
  })
}
