#' Namespace linter
#'
#' Check for missing packages and symbols in namespace calls.
#' Note that using `check_exports=TRUE` or `check_nonexports=TRUE` will load packages used in user code so it could
#' potentially change the global state.
#'
#' @param check_exports Check if `symbol` is exported from `namespace` in `namespace::symbol` calls.
#' @param check_nonexports Check if `symbol` exists in `namespace` in `namespace:::symbol` calls.
#' @evalRd rd_tags("namespace_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
namespace_linter <- function(check_exports = TRUE, check_nonexports = TRUE) {
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    ns_nodes <- xml2::xml_find_all(xml, "//expr/*[self::NS_GET or self::NS_GET_INT]")

    get_node_text <- function(nodes) {
      text <- xml2::xml_text(nodes)
      expr <- parse(text = text, keep.source = FALSE)
      vapply(expr, as.character, character(1L))
    }

    pkg_nodes <- xml2::xml_find_all(ns_nodes, "preceding-sibling::*[1]")
    sym_nodes <- xml2::xml_find_all(ns_nodes, "following-sibling::*[1]")
    pkgs <- get_node_text(pkg_nodes)
    ops <- xml2::xml_text(ns_nodes)
    syms <- get_node_text(sym_nodes)

    installed_packages <- .packages(all.available = TRUE)

    results <- lapply(
      seq_along(ns_nodes),
      function(i) {
        build_namespace_lint(pkgs[[i]], syms[[i]], pkg_nodes[[i]], sym_nodes[[i]], check_exports, check_nonexports)
      }
    )

    results[!vapply(results, is.null, logical(1L))]
  })
}

build_namespace_lint <- function(package, symbol, package_node, symbol_node, check_exports, check_nonexports) {
  if (!package %in% installed_packages) {
    return(build_uninstalled_lint(package, package_node, source_expression))
  }
  if (!check_exports && !check_nonexports) {
    return(NULL)
  }

  ns <- tryCatch(getNamespace(package), error = identity)

  if (!isNamespace(ns)) {
    return(build_non_namespace_lint(package_node, conditionMessage(ns), source_expression)) # nocov
  }
  if (check_exports && ops[[i]] == "::" && !is_exported_or_lazydata(symbol, ns)) {
    return(build_unexported_lint(symbol, symbol_node, package, source_expression))
  }

  if (!check_nonexports && ops[[i]] == ":::") {
    return(NULL)
  }

  if (exists(symbol, ns, inherits = FALSE)) {
    if (symbol %in% getNamespaceExports(ns)) {
      return(build_unnecessary_private_lint(symbol, symbol_node, package, source_expression))
    }
  } else {
    return(build_unfound_symbol_lint(symbol, symbol_node, package, source_expression))
  }
}

is_exported_or_lazydata <- function(symbol, namespace) {
  exports <- getNamespaceExports(namespace)
  lazydata <- names(.getNamespaceInfo(namespace, "lazydata"))
  symbol %in% c(exports, lazydata)
}

build_unexported_lint <- function(symbol, symbol_node, package, source_expression) {
  line1 <- as.integer(xml2::xml_attr(symbol_node, "line1"))
  col1 <- as.integer(xml2::xml_attr(symbol_node, "col1"))
  col2 <- as.integer(xml2::xml_attr(symbol_node, "col2"))
  Lint(
    filename = source_expression$filename,
    line_number = line1,
    column_number = col1,
    type = "warning",
    message = sprintf("'%s' is not exported from {%s}.", symbol, package),
    line = source_expression$file_lines[[line1]],
    ranges = list(c(col1, col2))
  )
}

build_unnecessary_private_lint <- function(symbol, symbol_node, package, source_expression) {
  line1 <- as.integer(xml2::xml_attr(symbol_node, "line1"))
  col1 <- as.integer(xml2::xml_attr(symbol_node, "col1"))
  col2 <- as.integer(xml2::xml_attr(symbol_node, "col2"))
  Lint(
    filename = source_expression$filename,
    line_number = line1,
    column_number = col1,
    type = "style",
    message = sprintf("'%s' is exported from {%s}. Use %s::%s instead.", symbol, package, package, symbol),
    line = source_expression$file_lines[[line1]],
    ranges = list(c(col1, col2))
  )
}

build_unfound_symbol_lint <- function(symbol, symbol_node, package, source_expression) {
  line1 <- as.integer(xml2::xml_attr(symbol_node, "line1"))
  col1 <- as.integer(xml2::xml_attr(symbol_node, "col1"))
  col2 <- as.integer(xml2::xml_attr(symbol_node, "col2"))
  Lint(
    filename = source_expression$filename,
    line_number = line1,
    column_number = col1,
    type = "warning",
    message = sprintf("'%s' does not exist in {%s}.", symbol, package),
    line = source_expression$file_lines[[line1]],
    ranges = list(c(col1, col2))
  )
}

build_non_namespace_lint <- function(package_node, message, source_expression) {
  line1 <- as.integer(xml2::xml_attr(package_node, "line1"))
  col1 <- as.integer(xml2::xml_attr(package_node, "col1"))
  col2 <- as.integer(xml2::xml_attr(package_node, "col2"))
  Lint(
    filename = source_expression$filename,
    line_number = line1,
    column_number = col1,
    type = "warning",
    message = message,
    line = source_expression$file_lines[[line1]],
    ranges = list(c(col1, col2))
  )
}

build_uninstalled_lint <- function(package, package_node, source_expression) {
  line1 <- as.integer(xml2::xml_attr(package_node, "line1"))
  col1 <- as.integer(xml2::xml_attr(package_node, "col1"))
  col2 <- as.integer(xml2::xml_attr(package_node, "col2"))
  Lint(
    filename = source_expression$filename,
    line_number = line1,
    column_number = col1,
    type = "warning",
    message = sprintf("Package '%s' is not installed.", package),
    line = source_expression$file_lines[[line1]],
    ranges = list(c(col1, col2))
  )
}
