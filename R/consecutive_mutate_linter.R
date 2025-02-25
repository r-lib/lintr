#' Require consecutive calls to mutate() to be combined when possible
#'
#' `dplyr::mutate()` accepts any number of columns, so sequences like
#'   `DF %>% dplyr::mutate(..1) %>% dplyr::mutate(..2)` are redundant --
#'   they can always be expressed with a single call to `dplyr::mutate()`.
#'
#' An exception is for some SQL back-ends, where the translation logic may not be
#'   as sophisticated as that in the default `dplyr`, for example in
#'   `DF %>% mutate(a = a + 1) %>% mutate(b = a - 2)`.
#'
#' @param invalid_backends Character vector of packages providing dplyr backends
#'   which may not be compatible with combining `mutate()` calls in all cases.
#'   Defaults to `"dbplyr"` since not all SQL backends can handle re-using
#'   a variable defined in the same `mutate()` expression.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x %>% mutate(a = 1) %>% mutate(b = 2)",
#'   linters = consecutive_mutate_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x %>% mutate(a = 1, b = 2)",
#'   linters = consecutive_mutate_linter()
#' )
#'
#' code <- "library(dbplyr)\nx %>% mutate(a = 1) %>% mutate(a = a + 1)"
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
  attach_pkg_xpath <- "
  following-sibling::expr
    /*[self::SYMBOL or self::STR_CONST]
  "

  namespace_xpath <- glue("
  //SYMBOL_PACKAGE[{ xp_text_in_table(invalid_backends) }]
  |
  //COMMENT[
    contains(text(), '@import')
    and (
      {xp_or(sprintf(\"contains(text(), '%s')\", invalid_backends))}
    )
  ]
  ")

  # match on the expr, not the SYMBOL_FUNCTION_CALL, to ensure
  #   namespace-qualified calls only match if the namespaces do.
  # expr[2] needed in expr[1][expr[2]] to skip matches on pipelines
  #   starting like mutate(DF, ...) %>% foo() %>% mutate().
  # similarly, expr[1][expr[call='mutate']] covers pipelines
  #   starting like mutate(DF, ...) %>% mutate(...)
  mutate_cond <- xp_and(
    "expr/SYMBOL_FUNCTION_CALL[text() = 'mutate']",
    "not(SYMBOL_SUB[text() = '.keep' or text() = '.by'])"
  )
  xpath <- glue("
  (//PIPE | //SPECIAL[{ xp_text_in_table(magrittr_pipes) }])
    /preceding-sibling::expr[expr[2][{ mutate_cond }] or ({ mutate_cond })]
    /following-sibling::expr[{ mutate_cond }]
  ")

  Linter(linter_level = "file", function(source_expression) {
    # need the full file to also catch usages at the top level
    xml <- source_expression$full_xml_parsed_content

    attach_str <- get_r_string(xml_find_all(
      source_expression$xml_find_function_calls(c("library", "require")),
      attach_pkg_xpath
    ))
    if (any(invalid_backends %in% attach_str)) {
      return(list())
    }

    namespace_expr <- xml_find_first(xml, namespace_xpath)
    if (!is.na(namespace_expr)) {
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
