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
  import_xpath <- "//expr[
      expr[1][SYMBOL_FUNCTION_CALL[text() = 'library' or text() = 'require']]
      and
      (
        not(SYMBOL_SUB[
          text() = 'character.only' and
          following-sibling::expr[1][NUM_CONST[text() = 'TRUE'] or SYMBOL[text() = 'T']]
        ]) or
        expr[2][STR_CONST]
      )
    ]"

  xp_used_symbols <- paste(
    "//SYMBOL_FUNCTION_CALL[not(preceding-sibling::NS_GET)]/text()",
    "//SYMBOL[not(
      parent::expr/preceding-sibling::expr/SYMBOL_FUNCTION_CALL[text() = 'library' or text() = 'require']
    )]/text()",
    "//SPECIAL/text()",
    sep = " | "
  )

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    import_exprs <- xml2::xml_find_all(xml, import_xpath)
    if (length(import_exprs) == 0L) {
      return(list())
    }
    imported_pkgs <- xml2::xml_find_chr(import_exprs, "string(expr[STR_CONST|SYMBOL])")
    # as.character(parse(...)) returns one entry per expression
    imported_pkgs <- as.character(parse(text = imported_pkgs, keep.source = FALSE))

    used_symbols <- xml2::xml_text(xml2::xml_find_all(xml, xp_used_symbols))

    is_used <- vapply(
      imported_pkgs,
      function(pkg) {
        # Skip excepted packages and packages that are not installed
        if (pkg %in% except_packages || !requireNamespace(pkg, quietly = TRUE)) {
          return(TRUE)
        }

        package_exports <- getNamespaceExports(pkg)
        any(package_exports %in% used_symbols)
      },
      logical(1L)
    )

    # TODO(michaelchirico): instead of vectorizing over packages,
    #   xml_find_all SYMBOL_PACKAGE namespaces and check imported_pkgs %in%
    is_ns_used <- vapply(
      imported_pkgs,
      function(pkg) {
        ns_usage <- xml2::xml_find_first(xml, paste0("//SYMBOL_PACKAGE[text() = '", pkg, "']"))
        !identical(ns_usage, xml2::xml_missing())
      },
      logical(1L)
    )

    is_unused <- !is_used
    if (allow_ns_usage) {
      is_unused[is_ns_used] <- FALSE
    }

    import_exprs <- import_exprs[is_unused]

    unused_packages <- get_r_string(import_exprs, xpath = "expr[STR_CONST | SYMBOL]")
    lint_message <- ifelse(
      is_ns_used[is_unused][unused_packages],
      paste0(
        "Package '", unused_packages, "' is only used by namespace. ",
        "Check that it is installed using loadNamespace() instead."
      ),
      paste0("Package '", unused_packages, "' is attached but never used.")
    )
    xml_nodes_to_lints(import_exprs, source_expression, lint_message, type = "warning")
  })
}
