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
  Linter(linter_level = "file", function(source_expression) {
    xml <- source_expression$full_xml_parsed_content

    ns_nodes <- xml_find_all_(xml, "//NS_GET | //NS_GET_INT")

    if (length(ns_nodes) == 0L) {
      return(list())
    }

    ## Case 1: pkg is uninstalled in pkg::foo

    package_nodes <- xml_find_all_(ns_nodes, "preceding-sibling::*[1]")
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

    if (length(packages) == 0L) {
      return(lints)
    }

    ## Case 2/3/4/5: problems with foo in pkg::foo / pkg:::foo

    # run here, not in the factory, to allow for run- vs. "compile"-time differences in package structure
    namespaces <- lapply(packages, \(package) tryCatch(getNamespace(package), packageNotFoundError = identity))
    failed_namespace <- vapply(namespaces, inherits, "packageNotFoundError", FUN.VALUE = logical(1L))

    if (any(failed_namespace)) {
      failed_pkgs <- unique(packages[failed_namespace]) # nolint: object_usage_linter. Used in cli message.
      cli_abort_internal("Failed to retrieve namespaces for {.pkg {failed_pkgs}} used with {.code ::} or {.code :::}.")
    }

    ns_get <- xml_text(ns_nodes) == "::"
    symbol_nodes <- xml_find_all_(ns_nodes, "following-sibling::*[1]")
    symbols <- get_r_string(symbol_nodes)
    is_setter <- xp_is_setter_call(ns_nodes, depth = 1L)
    symbols[is_setter] <- paste0(symbols[is_setter], "<-")

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

    pkg_path <- find_package(source_expression$filename)
    ns_imports <- namespace_imports(pkg_path)
    if (nrow(ns_imports) == 0L) {
      return(lints)
    }

    is_imported <- vapply(
      seq_along(symbols),
      \(ii) any(ns_imports$pkg == packages[ii] & ns_imports$fun == symbols[ii]),
      logical(1L)
    )
    if (any(is_imported)) {
      lints <- c(lints, xml_nodes_to_lints(
        symbol_nodes[is_imported],
        source_expression = source_expression,
        lint_message = sprintf(
          "Don't use `%s` to access %s, which is already imported from %s.",
          ifelse(ns_get[is_imported], "::", ":::"), symbols[is_imported], packages[is_imported]
        ),
        type = "warning"
      ))
    }

    lints
  })
}

# Discover OS-specific symbols in base R and package NAMESPACE files (#3075)
tools_compat_symbols <- function() {
  with_namespace_cache("tools_compat_symbols", {
    tools_ns <- asNamespace("tools")
    union(
      names(get0(".unix_only_proto_objects", envir = tools_ns, inherits = FALSE)),
      names(get0(".windows_only_proto_objects", envir = tools_ns, inherits = FALSE))
    )
  })
}

unwrap_ns_if <- function(e) {
  if (!is.call(e)) {
    return(e)
  }
  if (identical(e[[1L]], quote(`if`))) {
    b1 <- unwrap_ns_if(e[[3L]])
    return(if (length(e) == 4L) call("{", b1, unwrap_ns_if(e[[4L]])) else b1)
  }
  as.call(lapply(e, unwrap_ns_if))
}

parse_all_ns_branches <- function(ns_path) {
  if (!file.exists(ns_path) || !any(grepl("\\bif\\b", readLines(ns_path, warn = FALSE)))) {
    return(list(exports = character(), nonexports = character()))
  }
  exprs <- lapply(parse(ns_path, keep.source = FALSE, srcfile = NULL), unwrap_ns_if)
  tmp_dir <- tempfile()
  on.exit(unlink(tmp_dir, recursive = TRUE))
  pkg_dir <- file.path(tmp_dir, "pkg")
  dir.create(pkg_dir, recursive = TRUE)
  writeLines(vapply(exprs, deparse1, character(1L), collapse = "\n"), file.path(pkg_dir, "NAMESPACE"))
  ns_data <- parseNamespaceFile("pkg", tmp_dir)
  s3 <- ns_data$S3methods
  s3 <- s3[is.na(s3[, 4L]), , drop = FALSE]
  s3_names <- ifelse(is.na(s3[, 3L]), paste0(s3[, 1L], ".", s3[, 2L]), s3[, 3L])
  list(
    exports = unique(c(ns_data$exports, ns_data$exportMethods, ns_data$exportClasses)),
    nonexports = unique(s3_names)
  )
}

conditional_namespace_symbols <- function(ns, exported = TRUE) {
  pkg <- getNamespaceName(ns)
  syms <- with_namespace_cache(paste0("cond_ns_syms@", pkg), {
    if (pkg == "base") {
      gr_syms <- conditional_namespace_symbols(asNamespace("grDevices"), exported = FALSE)
      ut_syms <- conditional_namespace_symbols(asNamespace("utils"), exported = FALSE)
      list(exports = setdiff(tools_compat_symbols(), c(gr_syms, ut_syms)), nonexports = character())
    } else {
      ns_path <- file.path(getNamespaceInfo(ns, "path"), "NAMESPACE")
      res <- parse_all_ns_branches(ns_path)
      if (pkg == "utils") {
        ut_refs <- c(ls(ns, all.names = TRUE), all.names(body(ns$.onLoad)), all.names(body(ns$install.packages)))
        res$nonexports <- unique(c(res$nonexports, intersect(tools_compat_symbols(), setdiff(ut_refs, res$exports))))
      }
      res
    }
  })
  if (exported) syms$exports else c(syms$exports, syms$nonexports)
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
    function(ii) {
      sym <- symbols[[ii]]
      ns <- namespaces[[ii]]
      sym %in% namespace_symbols(ns, exported = exported) ||
        sym %in% conditional_namespace_symbols(ns, exported = exported)
    },
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
      sprintf("Don't use `:::` to access %s, which is exported from %s.", symbols[exported], packages[exported]),
    type = "warning"
  )

  c(non_symbols_lints, exported_lints)
}

build_ns_get_lints <- function(packages, symbols, symbol_nodes, namespaces, source_expression) {
  ## Case 4: foo is not an export in pkg::foo
  unexported <- !is_in_pkg(symbols, namespaces)
  xml_nodes_to_lints(
    symbol_nodes[unexported],
    source_expression = source_expression,
    lint_message = sprintf("'%s' is not exported from {%s}.", symbols[unexported], packages[unexported]),
    type = "warning"
  )
}
