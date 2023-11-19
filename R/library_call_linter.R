#' Library call linter
#'
#' Force library calls to all be at the top of the script.
#'
#' @param allow_preamble Logical, default `TRUE`. If `FALSE`,
#'   no code is allowed to precede the first `library()` call,
#'   otherwise some setup code is allowed, but all `library()`
#'   calls must follow consecutively after the first one.
#' @examples
#' # will produce lints
#' lint(
#'   text = "
#'     library(dplyr)
#'     print('test')
#'     library(tidyr)
#'   ",
#'   linters = library_call_linter()
#' )
#'
#' lint(
#'   text = "
#'     library(dplyr)
#'     print('test')
#'     library(tidyr)
#'     library(purrr)
#'   ",
#'   linters = library_call_linter()
#' )
#'
#' code <- "suppressMessages(library(dplyr))\nsuppressMessages(library(tidyr))"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = consecutive_suppression_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "
#'     library(dplyr)
#'     print('test')
#'   ",
#'   linters = library_call_linter()
#' )
#'
#' lint(
#'   text = "
#'     # comment
#'     library(dplyr)
#'   ",
#'   linters = library_call_linter()
#' )
#'
#' code <- "suppressMessages({\n  library(dplyr)\n  library(tidyr)\n})"
#' writeLines(code)
#' lint(
#'   text = code
#'   linters = consecutive_suppression_linter()
#' )
#'
#' @evalRd rd_tags("library_call_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
library_call_linter <- function(allow_preamble = TRUE) {
  attach_call_cond <- xp_text_in_table(c("library", "require"))
  suppress_call_cond <- xp_text_in_table(c("suppressMessages", "suppressPackageStartupMessages"))

  unsuppressed_call_cond <- glue("not( {xp_or(attach_call_cond, suppress_call_cond)} )")
  if (allow_preamble) {
    unsuppressed_call_cond <- xp_and(
      unsuppressed_call_cond,
      glue("@line1 > //SYMBOL_FUNCTION_CALL[{ attach_call_cond }][1]/@line1")
    )
  }
  late_attach_xpath <- glue("
    //SYMBOL_FUNCTION_CALL[{ attach_call_cond }][last()]
      /preceding::expr
      /SYMBOL_FUNCTION_CALL[{ unsuppressed_call_cond }][last()]
      /following::expr[SYMBOL_FUNCTION_CALL[{ attach_call_cond }]]
      /parent::expr
  ")

  attach_expr_cond <- glue("expr[expr[SYMBOL_FUNCTION_CALL[{attach_call_cond}]]]")

  # Use `calls` in the first condition, not in the second, to prevent, e.g.,
  #   the first call matching calls[1] but the second matching calls[2].
  #   That is, ensure that calls[i] only matches a following call to calls[i].
  # match on the expr, not the SYMBOL_FUNCTION_CALL, to ensure
  #   namespace-qualified calls only match if the namespaces do.
  consecutive_suppress_xpath <- glue("
  //SYMBOL_FUNCTION_CALL[{ suppress_call_cond }]
    /parent::expr
    /parent::expr[
      expr[SYMBOL_FUNCTION_CALL[{ suppress_call_cond }]] =
        following-sibling::expr[1][{attach_expr_cond}]/expr
      and {attach_expr_cond}
    ]
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    late_attach_expr <- xml_find_all(xml, late_attach_xpath)

    late_attach_call_name <- xp_call_name(late_attach_expr)

    late_attach_lints <- xml_nodes_to_lints(
      late_attach_expr,
      source_expression = source_expression,
      lint_message = sprintf("Move all %s calls to the top of the script.", late_attach_call_name),
      type = "warning"
    )

    consecutive_suppress_expr <- xml_find_all(xml, consecutive_suppress_xpath)
    consecutive_suppress_call_text <- xp_call_name(consecutive_suppress_expr)
    lint_message <- glue(
      "Unify consecutive calls to {consecutive_suppress_call_text}(). ",
      "You can do so by writing all of the calls in one braced expression ",
      "like {consecutive_suppress_call_text}({{...}})."
    )
    consecutive_suppress_lints <- xml_nodes_to_lints(
      consecutive_suppress_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )

    c(late_attach_lints, consecutive_suppress_lints)
  })
}
