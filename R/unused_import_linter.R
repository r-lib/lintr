#' Check that imported packages are actually used
#'
#' @param except_packages Character vector of packages that are ignored.
#' These are usually attached for their side effects.
#'
#' @evalRd rd_tags("unused_import_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
unused_import_linter <- function(except_packages = c("bit64", "data.table", "tidyverse")) {
  Linter(function(source_file) {
    if (is.null(source_file$full_xml_parsed_content)) return(list())

    import_exprs <- xml2::xml_find_all(
      source_file$full_xml_parsed_content,
      paste(
        "//expr[",
        "expr[SYMBOL_FUNCTION_CALL[text() = 'library' or text() = 'require']]",
        "and",
        "not(SYMBOL_SUB[text() = 'character.only'])",
        "]"
      )
    )
    if (length(import_exprs) == 0) {
      return()
    }
    # strip_names() needed to turn "'pkg'" -> "pkg" for STR_CONST calls.
    imported_pkgs <- strip_names(xml2::xml_text(xml2::xml_find_first(import_exprs, "expr[STR_CONST|SYMBOL]")))

    used_symbols <- xml2::xml_text(xml2::xml_find_all(
      source_file$full_xml_parsed_content,
      "//SYMBOL_FUNCTION_CALL[not(preceding-sibling::NS_GET)]/text() | //SYMBOL/text() | //SPECIAL/text()"
    ))

    is_unused <- vapply(
      imported_pkgs,
      function(pkg) {
        # Skip excepted packages and packages that are not installed
        if (pkg %in% except_packages || !requireNamespace(pkg, quietly = TRUE)) {
          return(FALSE)
        }

        package_exports <- getNamespaceExports(pkg)
        !any(package_exports %in% used_symbols)
      },
      logical(1L)
    )

    lapply(
      import_exprs[is_unused],
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = function(import_expr) {
        pkg <- strip_names(xml2::xml_text(xml2::xml_find_first(import_expr, "expr[STR_CONST|SYMBOL]")))
        paste0("package '", pkg, "' is attached but never used.")
      },
      type = "warning",
      global = TRUE
    )
  })
}
