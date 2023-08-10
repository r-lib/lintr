#' Namespace linter
#'
#' Check for missing packages and symbols in namespace calls.
#' Note that using `check_exports=TRUE` or `check_nonexports=TRUE` will load packages used in user code so it could
#' potentially change the global state.
#'
#' @param check_exports Check if `symbol` is exported from `namespace` in `namespace::symbol` calls.
#' @param check_nonexports Check if `symbol` exists in `namespace` in `namespace:::symbol` calls.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "xyzxyz::sd(c(1, 2, 3))",
#'   linters = namespace_linter()
#' )
#'
#' lint(
#'   text = "stats::ssd(c(1, 2, 3))",
#'   linters = namespace_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "stats::sd(c(1, 2, 3))",
#'   linters = namespace_linter()
#' )
#'
#' lint(
#'   text = "stats::ssd(c(1, 2, 3))",
#'   linters = namespace_linter(check_exports = FALSE)
#' )
#'
#' lint(
#'   text = "stats:::ssd(c(1, 2, 3))",
#'   linters = namespace_linter(check_nonexports = FALSE)
#' )
#'
#' @evalRd rd_tags("namespace_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
namespace_linter <- function(check_exports = TRUE, check_nonexports = TRUE) {
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    ns_nodes <- xml_find_all(xml, "//NS_GET | //NS_GET_INT")

    if (length(ns_nodes) == 0L) {
      return(list())
    }

    ## Case 1: pkg is uninstalled in pkg::foo

    package_nodes <- xml_find_all(ns_nodes, "preceding-sibling::*[1]")
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

    ## Case 2/3/4: problems with foo in pkg::foo / pkg:::foo

    # run here, not in the factory, to allow for run- vs. "compile"-time differences in package structure
    namespaces <- lapply(packages, function(package) tryCatch(getNamespace(package), error = identity))
    failed_namespace <- vapply(namespaces, inherits, "condition", FUN.VALUE = logical(1L))

    # nocov start
    if (any(failed_namespace)) {
      stop(
        "Failed to retrieve namespaces for one or more of the packages used with `::` or `:::`. ",
        "Please report the issue at https://github.com/r-lib/lintr/issues."
      )
    }
    # nocov end

    ns_get <- xml_text(ns_nodes) == "::"
    symbol_nodes <- xml_find_all(ns_nodes, "following-sibling::*[1]")
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

namespace_symbols <- function(ns, exported = TRUE) {
  if (exported) {
    c(getNamespaceExports(ns), names(.getNamespaceInfo(ns, "lazydata")))
  } else {
    ls(ns, all.names = TRUE)
  }
}
is_in_pkg <- function(symbols, namespaces, exported = TRUE) {
  vapply(
    seq_along(symbols),
    function(ii) symbols[[ii]] %in% namespace_symbols(namespaces[[ii]], exported = exported),
    logical(1L)
  )
}

build_ns_get_int_lints <- function(packages, symbols, symbol_nodes, namespaces, source_expression) {
  ## Case 2: foo does not exist in pkg:::foo
  non_symbols <- !is_in_pkg(symbols, namespaces, exported = FALSE)
  non_symbols_lints <- xml_nodes_to_lints(
    symbol_nodes[non_symbols],
    source_expression = source_expression,
    lint_message = sprintf("'%s' does not exist in {%s}.", symbols[non_symbols], packages[non_symbols]),
    type = "warning"
  )

  packages <- packages[!non_symbols]
  symbols <- symbols[!non_symbols]
  symbol_nodes <- symbol_nodes[!non_symbols]
  namespaces <- namespaces[!non_symbols]

  ## Case 3: foo is already exported pkg:::foo
  exported <- is_in_pkg(symbols, namespaces)
  exported_lints <- xml_nodes_to_lints(
    symbol_nodes[exported],
    source_expression = source_expression,
    lint_message =
      sprintf("'%1$s' is exported from {%2$s}. Use %2$s::%1$s instead.", symbols[exported], packages[exported]),
    type = "warning"
  )

  c(non_symbols_lints, exported_lints)
}

build_ns_get_lints <- function(packages, symbols, symbol_nodes, namespaces, source_expression) {
  # strip backticked symbols; `%>%` is the same as %>% (#1752).
  symbols <- gsub("^`(.*)`$", "\\1", symbols)

  ## Case 4: foo is not an export in pkg::foo
  unexported <- !is_in_pkg(symbols, namespaces)
  xml_nodes_to_lints(
    symbol_nodes[unexported],
    source_expression = source_expression,
    lint_message = sprintf("'%s' is not exported from {%s}.", symbols[unexported], packages[unexported]),
    type = "warning"
  )
}
