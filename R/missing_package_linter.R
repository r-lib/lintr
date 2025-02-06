#' Missing package linter
#'
#' Check for missing packages in `library()`, `require()`, `loadNamespace()`, and `requireNamespace()` calls.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "library(xyzxyz)",
#'   linters = missing_package_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "library(stats)",
#'   linters = missing_package_linter()
#' )
#'
#' @evalRd rd_tags("missing_package_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
missing_package_linter <- function() {
  library_require_xpath <- "
  parent::expr[
    expr[2][STR_CONST]
    or (
      expr[2][SYMBOL]
      and not(
        SYMBOL_SUB[text() = 'character.only']
        /following-sibling::expr[1]
        /NUM_CONST[text() = 'TRUE' or text() = 'T']
      )
    )
  ]"
  load_require_namespace_xpath <- "
  following-sibling::expr[1][STR_CONST]
    /parent::expr
  "

  Linter(linter_level = "file", function(source_expression) {
    library_require_calls <- source_expression$xml_find_function_calls(c("library", "require"))
    load_require_namespace_calls <- source_expression$xml_find_function_calls(c("loadNamespace", "requireNamespace"))
    pkg_calls <- combine_nodesets(
      xml_find_all(library_require_calls, library_require_xpath),
      xml_find_all(load_require_namespace_calls, load_require_namespace_xpath)
    )
    pkg_names <- get_r_string(xml_find_all(
      pkg_calls,
      "OP-LEFT-PAREN[1]/following-sibling::expr[1][SYMBOL | STR_CONST]"
    ))

    # run here, not in the factory, to allow for run- vs. "compile"-time differences in available packages
    installed_packges <- .packages(all.available = TRUE)
    missing_pkgs <- !(pkg_names %in% installed_packges)

    xml_nodes_to_lints(
      pkg_calls[missing_pkgs],
      source_expression = source_expression,
      lint_message = sprintf("Package '%s' is not installed.", pkg_names[missing_pkgs]),
      type = "warning"
    )
  })
}
