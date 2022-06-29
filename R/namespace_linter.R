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

    ## Case 1: pkg is uninstalled in pkg::foo

    package_nodes <- xml2::xml_find_all(ns_nodes, "preceding-sibling::*[1]")
    packages <- get_r_string(package_nodes)

    lints <- list()

    # run here, not in the factory, to allow for run- vs. "compile"-time differences in available packages
    installed_packages <- .packages(all.available = TRUE)
    installed <- packages %in% installed_packages

    if (!all(installed)) {
      lints <- c(lints, xml_nodes_to_lints(
        package_nodes[!installed],
        source_expression = source_expression,
        lint_message = sprintf("Package '%s' is not installed.", packages[!installed]),
        type = "warning"
      ))

      ns_nodes <- ns_nodes[installed]
      packages <- packages[installed]
      package_nodes <- package_nodes[installed]
    }

    if (!check_exports && !check_nonexports) {
      return(lints)
    }

    ## Case 2 (rare?): pkg namespace is broken in pkg::foo

    # run here, not in the factory, to allow for run- vs. "compile"-time differences in package structure
    namespaces <- lapply(packages, function(package) tryCatch(getNamespace(package), error = identity))
    failed_namespace <- vapply(namespaces, inherits, "condition", FUN.VALUE = logical(1L))

    if (any(failed_namespace)) {
      lints <- c(lints, xml_nodes_to_lints(
        package_nodes[failed_namespace],
        source_expression = source_expression,
        lint_message = vapply(namespaces[failed_namespace], conditionMessage, character(1L)),
        type = "warning"
      ))

      ns_nodes <- ns_nodes[!failed_namespace]
      packages <- packages[!failed_namespace]
      namespaces <- namespaces[!failed_namespace]
    }

    ## Case 3/4/5: problems with foo in pkg::foo / pkg:::foo

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

get_all_exports <- function(namespace) {
  c(getNamespaceExports(namespace), names(.getNamespaceInfo(namespace, "lazydata")))
}

build_ns_get_int_lints <- function(packages, symbols, symbol_nodes, namespaces, source_expression) {
  lints <- list()

  ## Case 3: foo does not exist in pkg:::foo

  non_symbols <- !vapply(
    seq_along(symbols),
    function(ii) symbols[[ii]] %in% ls(namespaces[[ii]], all.names = TRUE),
    logical(1L)
  )
  if (any(non_symbols)) {
    lints <- c(lints, xml_nodes_to_lints(
      symbol_nodes[non_symbols],
      source_expression = source_expression,
      lint_message = sprintf("'%s' does not exist in {%s}.", symbols[non_symbols], packages[non_symbols]),
      type = "warning"
    ))

    packages <- packages[!non_symbols]
    symbols <- symbols[!non_symbols]
    symbol_nodes <- symbol_nodes[!non_symbols]
  }

  ## Case 4: foo is already exported pkg:::foo

  exported <- vapply(
    seq_along(symbols),
    function(ii) symbols[[ii]] %in% get_all_exports(namespaces[[ii]]),
    logical(1L)
  )
  if (any(exported)) {
    lints <- c(lints, xml_nodes_to_lints(
      symbol_nodes[exported],
      source_expression = source_expression,
      lint_message =
        sprintf("'%1$s' is exported from {%2$s}. Use %2$s::%1$s instead.", symbols[exported], packages[exported]),
      type = "warning"
    ))
  }

  lints
}

build_ns_get_lints <- function(packages, symbols, symbol_nodes, namespaces, source_expression) {
  lints <- list()

  ## Case 5: foo is not an export in pkg::foo

  unexported <- !vapply(
    seq_along(symbols),
    function(ii) symbols[[ii]] %in% get_all_exports(namespaces[[ii]]),
    logical(1L)
  )
  if (any(unexported)) {
    lints <- c(lints, xml_nodes_to_lints(
      symbol_nodes[unexported],
      source_expression = source_expression,
      lint_message = sprintf("'%s' is not exported from {%s}.", symbols[unexported], packages[unexported]),
      type = "warning"
    ))
  }
  lints
}
