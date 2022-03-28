#' Block usage of file.path() with system.file()
#'
#' [system.file()] has a `...` argument which, internally, is passed to
#'   [file.path()], so including it in user code is repetitive.
#'
#' @evalRd rd_tags("system_file_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
system_file_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$xml_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    xpath <- "//expr[
      (
        expr/SYMBOL_FUNCTION_CALL[text() = 'system.file']
        and expr/expr/SYMBOL_FUNCTION_CALL[text() = 'file.path']
      ) or (
        expr/SYMBOL_FUNCTION_CALL[text() = 'file.path']
        and expr/expr/SYMBOL_FUNCTION_CALL[text() = 'system.file']
      )
    ]"
    bad_expr <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = function(expr) {
        outer_call <- xml2::xml_text(xml2::xml_find_first(expr, "expr/SYMBOL_FUNCTION_CALL"))
        if (outer_call == "system.file") {
          bad_usage <- 'system.file(file.path("data", "model.csv"), package = "myrf")'
        } else {
          bad_usage <- 'file.path(system.file(package = "myrf"), "data", "model.csv")'
        }
        paste(
          "Use the `...` argument of system.file() to expand paths,",
          'e.g. system.file("data", "model.csv", package = "myrf") instead of',
          bad_usage
        )
      },
      type = "warning"
    ))
  })
}
