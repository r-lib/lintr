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
  Linter(function(source_file) {
    if (length(source_file$xml_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    if (allow_empty_sep) {
      empty_sep_lints <- NULL
    } else {
      # NB: string-length()=2 means '' or "" (*not* 'ab' or 'cd' which have length 4)
      # TODO: adapt this for R>4.0 raw strings
      empty_sep_xpath <- "//expr[
        expr[SYMBOL_FUNCTION_CALL[text() = 'paste']]
        and SYMBOL_SUB[text() = 'sep']/following-sibling::expr[1][STR_CONST[string-length(text()) = 2]]
      ]"

      empty_sep_expr <- xml2::xml_find_all(xml, empty_sep_xpath)

      empty_sep_lints <- lapply(
        empty_sep_expr,
        xml_nodes_to_lint,
        source_file = source_file,
        lint_message = 'paste0(...) is better than paste(..., sep = "").',
        type = "warning"
      )
    }

    if (allow_to_string) {
      to_string_lints <- NULL
    } else {
      # 3 expr: the function call, the argument, and collapse=
      # TODO(michaelchirico): catch raw-string equivalents
      seps <- c("', '", '", "')
      to_string_xpath <- glue::glue("//expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'paste' or text() = 'paste0']]
        and count(expr) = 3
        and SYMBOL_SUB[text() = 'collapse']/following-sibling::expr[1][STR_CONST[ {xp_text_in_table(seps)} ]]
      ]")
      to_string_expr <- xml2::xml_find_all(xml, to_string_xpath)

      to_string_lints <- lapply(
        to_string_expr,
        xml_nodes_to_lint,
        source_file = source_file,
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
      source_file = source_file,
      lint_message = "sep= is not a formal argument to paste0(); did you mean to use paste(), or collapse=?",
      type = "warning"
    )

    c(empty_sep_lints, to_string_lints, paste0_sep_lints)
  })
}
