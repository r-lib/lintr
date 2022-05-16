#' Raise lints for several common poor usages of paste()
#'
#' The following issues are linted by default by this linter
#'   (and each can be turned off optionally):
#'
#'  1. Block usage of [paste()] with `sep = ""`. [paste0()] is a faster, more concise alternative.
#'  2. Block usage of `paste()` or `paste0()` with `collapse = ", "`. [toString()] is a direct
#'     wrapper for this, and alternatives like [glue::glue_collapse()] might give better messages for humans.
#'  3. Block usage of `paste0()` that supplies `sep=` -- this is not a formal argument to `paste0`, and
#'     is likely to be a mistake.
#'
#' @evalRd rd_tags("paste_linter")
#' @param allow_empty_sep Logical, default `FALSE`. If `TRUE`, usage of
#'   `paste()` with `sep = ""` is not linted.
#' @param allow_to_string Logical, default `FALSE`. If `TRUE`, usage of
#'   `paste()` and `paste0()` with `collapse = ", "` is not linted.
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
paste_linter <- function(allow_empty_sep = FALSE, allow_to_string = FALSE) {
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression", require_xml = TRUE)) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    if (allow_empty_sep) {
      empty_sep_lints <- NULL
    } else {
      sep_xpath <- "//expr[
        expr[SYMBOL_FUNCTION_CALL[text() = 'paste']]
        and SYMBOL_SUB[text() = 'sep']/following-sibling::expr[1][STR_CONST]
      ]"

      empty_sep_expr <- xml2::xml_find_all(xml, sep_xpath)
      sep_value <- get_r_string(xml2::xml_find_first(
        empty_sep_expr,
        "./SYMBOL_SUB[text() = 'sep']/following-sibling::expr[1]"
      ))

      empty_sep_lints <- lapply(
        empty_sep_expr[!nzchar(sep_value)],
        xml_nodes_to_lint,
        source_expression = source_expression,
        lint_message = 'paste0(...) is better than paste(..., sep = "").',
        type = "warning"
      )
    }

    if (allow_to_string) {
      to_string_lints <- NULL
    } else {
      # 3 expr: the function call, the argument, and collapse=
      to_string_xpath <- glue::glue("//expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'paste' or text() = 'paste0']]
        and count(expr) = 3
        and SYMBOL_SUB[text() = 'collapse']/following-sibling::expr[1][STR_CONST]
      ]")
      to_string_expr <- xml2::xml_find_all(xml, to_string_xpath)
      collapse_value <- get_r_string(xml2::xml_find_first(
        to_string_expr,
        "./SYMBOL_SUB[text() = 'collapse']/following-sibling::expr[1]"
      ))

      to_string_lints <- lapply(
        to_string_expr[collapse_value == ", "],
        xml_nodes_to_lint,
        source_expression = source_expression,
        lint_message = paste(
          'toString(.) is more expressive than paste(., collapse = ", ").',
          "Note also glue::glue_collapse() and and::and()",
          "for constructing human-readable / translation-friendly lists"
        ),
        type = "warning"
      )
    }

    paste0_sep_xpath <- "//expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'paste0']]
      and SYMBOL_SUB[text() = 'sep']
    ]"
    paste0_sep_expr <- xml2::xml_find_all(xml, paste0_sep_xpath)
    paste0_sep_lints <- lapply(
      paste0_sep_expr,
      xml_nodes_to_lint,
      source_expression = source_expression,
      lint_message = "sep= is not a formal argument to paste0(); did you mean to use paste(), or collapse=?",
      type = "warning"
    )

    c(empty_sep_lints, to_string_lints, paste0_sep_lints)
  })
}
