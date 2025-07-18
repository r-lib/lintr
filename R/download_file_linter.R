#' Recommend usage of a portable `mode` value for downloading files
#'
#' `mode = "w"` (the default) or `mode = "a"` in `download.file()` can generate broken files
#' on Windows. Instead, [utils::download.file()] recommends the usage of `mode = "wb"`
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
#' # okay
#' lint(
#'   text = "download.file(x = my_url, mode = 'wb')",
#'   linters = download_file_linter()
#' )
#'
#' @evalRd rd_tags("download_file_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
download_file_linter <- function() {

  Linter(linter_level = "expression", function(source_expression) {
    download_file_calls <- source_expression$xml_find_function_calls("download.file")

    download_file_mode <- get_r_string(
      download_file_calls,
      "parent::expr[1]/
        SYMBOL_SUB[text() = 'mode']/
        following-sibling::expr[1]/STR_CONST
      "
    )

    implicit_mode <- is.na(download_file_mode)

    implicit_mode_lint <- xml_nodes_to_lints(
      download_file_calls[implicit_mode],
      source_expression = source_expression,
      lint_message = "download.file() should use mode = 'wb' or ('ab') rather than relying on the default mode = 'w'.",
      type = "warning"
    )

    wrong_mode <- !implicit_mode & download_file_mode %in% c("a", "w")

    explicit_wrong_mode_lint <- xml_nodes_to_lints(
      download_file_calls[wrong_mode],
      source_expression = source_expression,
      lint_message = sprintf(
        "download.file() should use mode = '%1$sb' rather than mode = '%1$s' for portability on Windows.",
        download_file_mode[wrong_mode]
      ),
      type = "warning"
    )

    c(implicit_mode_lint, explicit_wrong_mode_lint)
  })

}
