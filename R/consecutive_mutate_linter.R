#' Force consecutive calls to mutate() into just one when possible
#'
#' `dplyr::mutate()` accepts any number of columns, so sequences like
#'   `DF %>% dplyr::mutate(..1) %>% dplyr::mutate(..2)` are redundant --
#'   they can always be expressed with a single call to `dplyr::mutate()`.
#'
#' An exception is for some SQL back-ends, where the translation logic may not be
#'   as sophisticated as that in the default `dplyr`, for example in
#'   `DF %>% mutate(a = a + 1) %>% mutate(b = a - 2)`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x %>% mutate(a = 1) %>% mutate(b = 2)",
#'   linters = consecutive_mutate_linter()
#' )
#'
#' # okay
#' code <- "library(dplyr)\nx %>% mutate(a = 1) %>% mutate(a = a + 1)"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = consecutive_mutate_linter()
#' )
#'
#' @evalRd rd_tags("consecutive_mutate_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
consecutive_mutate_linter <- function(invalid_backends = "dbplyr") {
  blocked_library_xpath <- glue("
  //SYMBOL_FUNCTION_CALL[text() = 'library']
    /parent::expr
    /following-sibling::expr[SYMBOL[{ xp_text_in_table(invalid_backends) }]]
  ")

  # match on the expr, not the SYMBOL_FUNCTION_CALL, to ensure
  #   namespace-qualified calls only match if the namespaces do.
  # expr[2] needed in expr[1][expr[2]] to skip matches on pipelines
  #   starting like mutate(DF, ...) %>% foo() %>% mutate().
  # similarly, expr[1][expr[call='mutate']] covers pipelines
  #   starting like mutate(DF, ...) %>% mutate(...)
  xpath <- glue("
  (//PIPE | //SPECIAL[{ xp_text_in_table(magrittr_pipes) }])
    /preceding-sibling::expr[
      expr[2][expr/SYMBOL_FUNCTION_CALL[text() = 'mutate']]
      or expr/SYMBOL_FUNCTION_CALL[text() = 'mutate']
    ]
    /following-sibling::expr[
      expr/SYMBOL_FUNCTION_CALL[text() = 'mutate']
      and not(SYMBOL_SUB[text() = '.keep' or text() = '.by'])
    ]
  ")

  Linter(function(source_expression) {
    # need the full file to also catch usages at the top level
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    blocked_expr <- xml_find_first(xml, blocked_library_xpath)
    if (!is.na(blocked_expr)) {
      return(list())
    }

    bad_expr <- xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = "Unify consecutive calls to mutate().",
      type = "warning"
    )
  })
}
