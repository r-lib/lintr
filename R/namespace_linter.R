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

    ns_nodes <- xml2::xml_find_all(xml, "//NS_GET | //NS_GET_INT")

    if (length(ns_nodes) == 0L) {
      return(list())
    }

    package_nodes <- xml2::xml_find_all(ns_nodes, "preceding-sibling::*[1]")
    packages <- get_r_string(package_nodes)

    lints <- list()

    installed_packages <- .packages(all.available = TRUE)
    installed <- packages %in% installed_packages

    if (!all(installed)) {
      lints <- c(lints, build_uninstalled_lints(
        packages[!installed],
        package_nodes[!installed],
        source_expression
      ))

      ns_nodes <- ns_nodes[installed]
      packages <- packages[installed]
      package_nodes <- package_nodes[installed]
    }

    if (!check_exports && !check_nonexports) {
      return(uninstalled_lints)
    }

    namespaces <- lapply(packages, function(package) tryCatch(getNamespace(package), error = identity))
    failed_namespace <- vapply(namespaces, inherits, "condition", FUN.VALUE = logical(1L))

    if (any(failed_namespace)) {
      lints <- c(lints, build_failed_namespace_lints(
        package_nodes[failed_namespace],
        vapply(namespaces[failed_namespace], conditionMessage, character(1L)),
        source_expression
      ))

      ns_nodes <- ns_nodes[!failed_namespace]
      packages <- packages[!failed_namespace]
      namespaces <- namespaces[!failed_namespace]
    }

    ns_get <- xml2::xml_text(ns_nodes) == "::"
    symbol_nodes <- xml2::xml_find_all(ns_nodes, "following-sibling::*[1]")
    symbols <- get_r_string(symbol_nodes)

    if (check_nonexports) {
      lints <- c(lints, build_ns_get_int_lints(
        packages[!ns_get],
        symbols[!ns_get],
        symbol_nodes[!ns_get],
        namespaces[!ns_get],
        source_expression
      ))
    }

    if (check_exports) {
      lints <- c(lints, build_ns_get_lints(
        packages[ns_get],
        symbols[ns_get],
        symbol_nodes[ns_get],
        namespaces[ns_get],
        source_expression
      ))
    }

    lints
  })
}

build_ns_get_int_lints <- function(packages, symbols, symbol_nodes, namespaces, source_expression) {
  lints <- list()

  non_symbols <- !vapply(
    seq_along(symbols),
    function(ii) symbols[[ii]] %in% ls(namespaces[[ii]], all = TRUE),
    logical(1L)
  )
  if (any(non_symbols)) {
    lints <- c(lints, build_unfound_symbol_lints(
      packages[non_symbols],
      symbols[non_symbols],
      symbol_nodes[non_symbols],
      source_expression
    ))

    packages <- packages[!non_symbols]
    symbols <- symbols[!non_symbols]
    symbol_nodes <- symbol_nodes[!non_symbols]
  }

  exported <- vapply(
    seq_along(symbols),
    function(ii) symbols[[ii]] %in% get_all_exports(namespaces[[ii]]),
    logical(1L)
  )
  if (any(exported)) {
    lints <- c(lints, build_unnecessary_private_lints(
      packages[exported],
      symbols[exported],
      symbol_nodes[exported],
      source_expression
    ))
  }

  lints
}

build_ns_get_lints <- function(packages, symbols, symbol_nodes, namespaces, source_expression) {
  exports <- lapply(namespaces, get_all_exports)

  lints <- list()
  unexported <- !vapply(
    seq_along(symbols),
    function(ii) symbols[[ii]] %in% exports[[ii]],
    logical(1L)
  )
  if (any(unexported)) {
    lints <- c(lints, build_unexported_lints(
      packages[unexported],
      symbols[unexported],
      symbol_nodes[unexported],
      source_expression
    ))
  }
}

get_all_exports <- function(namespace) {
  c(getNamespaceExports(namespace), names(.getNamespaceInfo(namespace, "lazydata")))
}

build_unexported_lints <- function(packages, symbols, symbol_nodes, source_expression) {
  line1 <- as.integer(xml2::xml_attr(symbol_nodes, "line1"))
  col1 <- as.integer(xml2::xml_attr(symbol_nodes, "col1"))
  col2 <- as.integer(xml2::xml_attr(symbol_nodes, "col2"))

  messages <- sprintf("'%s' is not exported from {%s}.", symbols, packages)
  lapply(
    seq_along(symbol_nodes),
    function(ii) {
      line1 <- line1[[ii]]
      col1 <- col1[[ii]]
      Lint(
        filename = source_expression$filename,
        line_number = line1,
        column_number = col1,
        type = "warning",
        message = messages[[ii]],
        line = source_expression$file_lines[[line1]],
        ranges = list(c(col1, col2[[ii]]))
      )
    }
  )
}

build_unnecessary_private_lints <- function(packages, symbols, symbol_nodes, source_expression) {
  line1 <- as.integer(xml2::xml_attr(symbol_nodes, "line1"))
  col1 <- as.integer(xml2::xml_attr(symbol_nodes, "col1"))
  col2 <- as.integer(xml2::xml_attr(symbol_nodes, "col2"))

  messages <- sprintf("'%1$s' is exported from {%2$s}. Use %2$s::%1$s instead.", symbols, packages)
  lapply(
    seq_along(symbol_nodes),
    function(ii) {
      line1 <- line1[[ii]]
      col1 <- col1[[ii]]
      Lint(
        filename = source_expression$filename,
        line_number = line1,
        column_number = col1,
        type = "warning",
        message = messages[[ii]],
        line = source_expression$file_lines[[line1]],
        ranges = list(c(col1, col2[[ii]]))
      )
    }
  )
}

build_unfound_symbol_lints <- function(packages, symbols, symbol_nodes, source_expression) {
  line1 <- as.integer(xml2::xml_attr(symbol_nodes, "line1"))
  col1 <- as.integer(xml2::xml_attr(symbol_nodes, "col1"))
  col2 <- as.integer(xml2::xml_attr(symbol_nodes, "col2"))

  messages <- sprintf("'%s' does not exist in {%s}.", symbols, packages)
  lapply(
    seq_along(symbol_nodes),
    function(ii) {
      line1 <- line1[[ii]]
      col1 <- col1[[ii]]
      Lint(
        filename = source_expression$filename,
        line_number = line1,
        column_number = col1,
        type = "warning",
        message = messages[[ii]],
        line = source_expression$file_lines[[line1]],
        ranges = list(c(col1, col2[[ii]]))
      )
    }
  )
}

build_failed_namespace_lints <- function(package_nodes, messages, source_expression) {
  line1 <- as.integer(xml2::xml_attr(package_nodes, "line1"))
  col1 <- as.integer(xml2::xml_attr(package_nodes, "col1"))
  col2 <- as.integer(xml2::xml_attr(package_nodes, "col2"))

  lapply(
    seq_along(package_nodes),
    function(ii) {
      line1 <- line1[[ii]]
      col1 <- col1[[ii]]
      Lint(
        filename = source_expression$filename,
        line_number = line1,
        column_number = col1,
        type = "warning",
        message = messages[[ii]],
        line = source_expression$file_lines[[line1]],
        ranges = list(c(col1, col2[[ii]]))
      )
    }
  )
}

build_uninstalled_lints <- function(packages, package_nodes, source_expression) {
  line1 <- as.integer(xml2::xml_attr(package_nodes, "line1"))
  col1 <- as.integer(xml2::xml_attr(package_nodes, "col1"))
  col2 <- as.integer(xml2::xml_attr(package_nodes, "col2"))

  messages <- sprintf("Package '%s' is not installed.", packages)
  lapply(
    seq_along(packages),
    function(ii) {
      line1 <- line1[[ii]]
      col1 <- col1[[ii]]
      Lint(
        filename = source_expression$filename,
        line_number = line1,
        column_number = col1,
        type = "warning",
        message = messages[[ii]],
        line = source_expression$file_lines[[line1]],
        ranges = list(c(col1, col2[[ii]]))
      )
    }
  )
}
