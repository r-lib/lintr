#' Check that imported packages are actually used
#'
#' @inheritParams object_usage_linter
#' @param allow_ns_usage Suppress lints for packages only used via namespace.
#' This is `FALSE` by default because `pkg::fun()` doesn't require `library(pkg)`.
#' You can use [requireNamespace("pkg")][requireNamespace()] to ensure a package is
#' installed without attaching it.
#' @param except_packages Character vector of packages that are ignored.
#' These are usually attached for their side effects.
#'
#' @examples
#' # will produce lints
#' code_lines <- "library(dplyr)\n1 + 1"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = unused_import_linter()
#' )
#'
#' code_lines <- "library(dplyr)\ndplyr::tibble(a = 1)"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = unused_import_linter()
#' )
#'
#' # okay
#' code_lines <- "library(dplyr)\ntibble(a = 1)"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = unused_import_linter()
#' )
#'
#' code_lines <- "library(dplyr)\ndplyr::tibble(a = 1)"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = unused_import_linter(allow_ns_usage = TRUE)
#' )
#'
#' @evalRd rd_tags("unused_import_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
unused_import_linter <- function(allow_ns_usage = FALSE,
                                 except_packages = c("bit64", "data.table", "tidyverse"),
                                 interpret_glue = TRUE) {
  # Get dataset names lazy-loaded by imported packages
  get_datasets <- function(pkg) {
    results <- utils::data(package = pkg)$results
    items <- results[, "Item"]
    # e.g. 'state.abb (state)' in 'datasets'
    gsub("\\s*\\([^)]*\\)$", "", items)
  }

  import_xpath <- "
  parent::expr[
    expr[2][STR_CONST]
    or not(SYMBOL_SUB[
      text() = 'character.only' and
      following-sibling::expr[1][NUM_CONST[text() = 'TRUE'] or SYMBOL[text() = 'T']]
    ])
  ]"

  xp_used_functions <- "SYMBOL_FUNCTION_CALL[not(preceding-sibling::NS_GET)]"
  xp_used_symbols <- paste(
    "//SYMBOL[not(
      parent::expr/preceding-sibling::expr[last()]/SYMBOL_FUNCTION_CALL[text() = 'library' or text() = 'require']
    )]",
    "//SPECIAL",
    sep = " | "
  )

  Linter(linter_level = "file", function(source_expression) {
    xml <- source_expression$full_xml_parsed_content
    library_calls <- source_expression$xml_find_function_calls(c("library", "require"))
    all_calls <- source_expression$xml_find_function_calls(NULL)

    import_exprs <- xml_find_all(library_calls, import_xpath)

    if (length(import_exprs) == 0L) {
      return(list())
    }
    imported_pkgs <- xml_find_chr(import_exprs, "string(expr[STR_CONST|SYMBOL])")
    # as.character(parse(...)) returns one entry per expression
    imported_pkgs <- as.character(parse(text = imported_pkgs, keep.source = FALSE))

    used_symbols <- unique(c(
      xml_text(xml_find_all(all_calls, xp_used_functions)),
      xml_text(xml_find_all(xml, xp_used_symbols)),
      extract_glued_symbols(xml, interpret_glue = interpret_glue)
    ))

    is_used <- vapply(
      imported_pkgs,
      function(pkg) {
        # Skip excepted packages and packages that are not installed
        if (pkg %in% except_packages || !requireNamespace(pkg, quietly = TRUE)) {
          return(TRUE)
        }

        package_exports <- getNamespaceExports(pkg) # functions
        dataset_exports <- get_datasets(pkg) # datasets

        any(package_exports %in% used_symbols) || any(dataset_exports %in% used_symbols)
      },
      logical(1L)
    )

    # TODO(#2480): Only call //SYMBOL_PACKAGE once.
    is_ns_used <- vapply(
      imported_pkgs,
      function(pkg) {
        ns_usage <- xml_find_first(xml, paste0("//SYMBOL_PACKAGE[text() = '", pkg, "']"))
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
        "Don't attach package '", unused_packages, "', which is only used by namespace. ",
        "Check that it is installed using loadNamespace() instead."
      ),
      paste0("Package '", unused_packages, "' is attached but never used.")
    )
    xml_nodes_to_lints(import_exprs, source_expression, lint_message, type = "warning")
  })
}
