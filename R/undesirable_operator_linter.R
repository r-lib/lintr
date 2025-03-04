#' Undesirable operator linter
#'
#' Report the use of undesirable operators, e.g. \code{\link[base:ns-dblcolon]{:::}} or
#' [`<<-`][base::assignOps] and suggest an alternative.
#'
#' @param op Character vector of undesirable operators. Input can be any of three types:
#'   - Unnamed entries must be a character string specifying an undesirable operator.
#'   - For named entries, the name specifies the undesirable operator.
#'     + If the entry is a character string, it is used as a description of
#'       why a given operator is undesirable
#'     + Otherwise, entries should be missing (`NA`)
#'   A generic message that the named operator is undesirable is used if no
#'     specific description is provided.
#'   Input can also be a list of character strings for convenience.
#'
#'   Defaults to [default_undesirable_operators]. To make small customizations to this list,
#'   use [modify_defaults()].
#' @param call_is_undesirable Logical, default `TRUE`. Should lints also be produced
#'   for prefix-style usage of the operators provided in `op`?
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
#' lint(
#'   text = "mtcars$wt",
#'   linters = undesirable_operator_linter("$")
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
#' lint(
#'   text = 'mtcars[["wt"]]',
#'   linters = undesirable_operator_linter("$")
#' )
#'
#' @evalRd rd_tags("undesirable_operator_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
undesirable_operator_linter <- function(op = default_undesirable_operators,
                                        call_is_undesirable = TRUE) {
  if (is.list(op)) op <- unlist(op)
  stopifnot(
    is.logical(call_is_undesirable),
    # allow (uncoerced->implicitly logical) 'NA'
    `\`op\` should be a non-empty character vector` =
      length(op) > 0L && (is.character(op) || all(is.na(op)))
  )

  nm <- names2(op)
  implicit_idx <- !nzchar(nm)
  if (any(implicit_idx)) {
    names(op)[implicit_idx] <- op[implicit_idx]
    is.na(op) <- implicit_idx
  }
  if (anyNA(names(op))) {
    missing_idx <- which(is.na(names(op))) # nolint: object_usage_linter. False positive.
    cli_abort(paste(
      "Unnamed elements of {.arg op} must not be missing,",
      "but {.val {missing_idx}} {qty(length(missing_idx))} {?is/are}."
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
