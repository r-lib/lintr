#' Check that imported packages are actually used
#'
#' @param allow_ns_usage Suppress lints for packages only used via namespace.
#' This is `FALSE` by default because `pkg::fun()` doesn't require `library(pkg)`.
#' You can use [requireNamespace("pkg")][requireNamespace()] to ensure a package is installed without loading it.
#' @param except_packages Character vector of packages that are ignored.
#' These are usually attached for their side effects.
#'
#' @evalRd rd_tags("unused_import_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
unused_import_linter <- function(allow_ns_usage = FALSE, except_packages = c("bit64", "data.table", "tidyverse")) {
  Linter(function(source_expression) {
    if (is.null(source_expression$full_xml_parsed_content)) return(list())

    import_exprs <- xml2::xml_find_all(
      source_expression$full_xml_parsed_content,
      paste(
        "//expr[",
        "expr[SYMBOL_FUNCTION_CALL[text() = 'library' or text() = 'require']]",
        "and",
        "(not(SYMBOL_SUB[text() = 'character.only']) or *[1][self::STR_CONST])",
        "]"
      )
    )
    if (length(import_exprs) == 0) {
      return(list())
    }
    imported_pkgs <- xml2::xml_text(xml2::xml_find_first(import_exprs, "expr[STR_CONST|SYMBOL]"))
    # as.character(parse(...)) returns one entry per expression
    imported_pkgs <- as.character(parse(text = imported_pkgs, keep.source = FALSE))

    xp_used_symbols <- paste(
      if (isTRUE(allow_ns_usage)) {
        "//SYMBOL_FUNCTION_CALL/text()"
      } else {
        "//SYMBOL_FUNCTION_CALL[not(preceding-sibling::NS_GET)]/text()"
      },
      "//SYMBOL/text()",
      "//SPECIAL/text()",
      sep = " | "
    )

    used_symbols <- xml2::xml_text(xml2::xml_find_all(source_expression$full_xml_parsed_content, xp_used_symbols))

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
      source_file = source_expression,
      lint_message = function(import_expr) {
        pkg <- get_r_string(xml2::xml_text(xml2::xml_find_first(import_expr, "expr[STR_CONST|SYMBOL]")))
        paste0("package '", pkg, "' is attached but never used.")
      },
      type = "warning",
      global = TRUE
    )
  })
}
