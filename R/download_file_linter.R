#' Recommend usage of a portable `mode` value in [download.file()]
#'
#' `mode = "w"` or `mode = "a"` in `download.file()` can generate broken files
#' on Windows. Instead, `?download.file()` recommends the usage of `mode = "wb"`
#' and `mode = "ab"`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "download.file(x = my_url)",
#'   linters = download_file_linter()
#' )
#'
#' lint(
#'   text = "download.file(x = my_url, mode = 'w')",
#'   linters = download_file_linter()
#' )
#'
#' lint(
#'   text = "download.file(x = my_url, , , , 'w')",
#'   linters = download_file_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "download.file(x = my_url, mode = 'wb')",
#'   linters = download_file_linter()
#' )
#'
#' lint(
#'   text = "download.file(x = my_url, , , , 'wb')",
#'   linters = download_file_linter()
#' )
#'
#' @evalRd rd_tags("download_file_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
download_file_linter <- function() {

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls("download.file")

    # 3 options:
    # - no explicit value for mode
    # - mode = "a" or mode = "w"
    # - similar as previous but specified by position
    xml_calls_no_mode <- xml_find_all(
      xml_calls,
      "parent::expr[1][
        not(SYMBOL_SUB[text() = 'mode'])
        and count(OP-COMMA) < 4
      ]"
    )

    xml_calls_bad_mode_by_name <- xml_find_all(
      xml_calls,
      glue(
        "parent::expr[1][
          SYMBOL_SUB[text() = 'mode'
          and following-sibling::expr[1]/STR_CONST[not(contains(text(), 'b'))]]
        ]"
      )
    )

    xml_calls_bad_mode_by_position <- xml_find_all(
      xml_calls,
      "parent::expr[1][
        count(SYMBOL_SUB[text() = 'mode']) = 0
        and OP-COMMA[4]/following-sibling::expr[1]/STR_CONST[not(contains(text(), 'b'))]
      ]"
    )

    xml_nodes_to_lints(
      combine_nodesets(xml_calls_no_mode, xml_calls_bad_mode_by_name, xml_calls_bad_mode_by_position),
      source_expression = source_expression,
      lint_message = paste(
        "download.file() should use mode = 'wb' (or 'ab') rather than mode = 'w' (or 'a') for portability on Windows."
      ),
      type = "warning"
    )
  })

}
