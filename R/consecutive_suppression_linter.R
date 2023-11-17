#' Force consecutive calls suppress* calls into just one when possible
#'
#' Suppressing the output of consecutive calls to [library()] with
#'   [suppressMessages()] or [suppressPackageStartupMessages()] need only be
#'   done once, by wrapping the sequence of library calls into
#'   one `{}` expression, as opposed to repeatedly
#'
#' @examples
#' # will produce lints
#' code <- "suppressMessages(library(dplyr))\nsuppressMessages(library(tidyr))"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = consecutive_suppression_linter()
#' )
#'
#' # okay
#' code <- "suppressMessages({\n  library(dplyr)\n  library(tidyr)\n})"
#' writeLines(code)
#' lint(
#'   text = code
#'   linters = consecutive_suppression_linter()
#' )
#'
#' @evalRd rd_tags("consecutive_suppression_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
consecutive_suppression_linter <- function() {
  suppress_calls <- c("suppressMessages", "suppressPackageStartupMessages")
  library_calls <- c("library", "require")
  library_cond <- glue(
    "expr[expr[SYMBOL_FUNCTION_CALL[{xp_text_in_table(library_calls)}]]]"
  )
  # Use `calls` in the first condition, not in the second, to prevent, e.g.,
  #   the first call matching calls[1] but the second matching calls[2].
  #   That is, ensure that calls[i] only matches a following call to calls[i].
  # match on the expr, not the SYMBOL_FUNCTION_CALL, to ensure
  #   namespace-qualified calls only match if the namespaces do.
  xpath <- glue("
  //SYMBOL_FUNCTION_CALL[{ xp_text_in_table(suppress_calls) }]
    /parent::expr
    /parent::expr[
      expr[SYMBOL_FUNCTION_CALL[{ xp_text_in_table(suppress_calls) }]]
        = following-sibling::expr[1][{library_cond}]/expr
      and {library_cond}
    ]
  ")

  Linter(function(source_expression) {
    # need the full file to also catch usages at the top level
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)
    call_text <- xp_call_name(bad_expr)
    lint_message <- glue(
      "Unify consecutive calls to {call_text}(). ",
      "You can do so by writing all of the calls in one braced expression ",
      "like {call_text}({{...}})."
    )
    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )
  })
}
