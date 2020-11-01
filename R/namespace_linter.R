#' @describeIn linter that checks for missing packages and symbols in namespace calls.
#'   Note that using \code{check_exports=TRUE} or \code{check_nonexports=TRUE} will
#'   load packages used in user code so it could potentially change the global state.
#' @export
namespace_linter <- function(check_exports = TRUE, check_nonexports = TRUE) {
  function(source_file) {
    if (!length(source_file$parsed_content)) return(list())

    xml <- source_file$xml_parsed_content

    ns_nodes <- xml2::xml_find_all(xml, "//expr/*[self::NS_GET or self::NS_GET_INT]")

    get_node_text <- function(nodes) {
      text <- xml2::xml_text(nodes)
      expr <- parse(text = text, keep.source = FALSE)
      vapply(expr, format, character(1L))
    }

    pkg_nodes <- xml2::xml_find_all(ns_nodes, "preceding-sibling::*[1]")
    sym_nodes <- xml2::xml_find_all(ns_nodes, "following-sibling::*[1]")
    pkgs <- get_node_text(pkg_nodes)
    ops <- xml2::xml_text(ns_nodes)
    syms <- get_node_text(sym_nodes)

    results <- lapply(seq_along(ns_nodes), function(i) {
      ns <- tryCatch(getNamespace(pkgs[[i]]), error = function(e) NULL)
      if (isNamespace(ns)) {
        if (check_exports && ops[[i]] == "::") {
          exports <- getNamespaceExports(ns)
          lazydata <- names(.getNamespaceInfo(ns, "lazydata"))
          if (!(syms[[i]] %in% c(exports, lazydata))) {
            line1 <- as.integer(xml2::xml_attr(sym_nodes[[i]], "line1"))
            col1 <- as.integer(xml2::xml_attr(sym_nodes[[i]], "col1"))
            col2 <- as.integer(xml2::xml_attr(sym_nodes[[i]], "col2"))
            return(Lint(
              filename = source_file$filename,
              line_number = line1,
              column_number = col1,
              type = "warning",
              message = sprintf("'%s' is not exported from {%s}.", syms[[i]], pkgs[[i]]),
              line = source_file$lines[line1],
              ranges = list(c(col1, col2)),
              linter = "namespace_linter"
            ))
          }
        }

        if (check_nonexports && ops[[i]] == ":::") {
          if (!exists(syms[[i]], ns, inherits = FALSE)) {
            line1 <- as.integer(xml2::xml_attr(sym_nodes[[i]], "line1"))
            col1 <- as.integer(xml2::xml_attr(sym_nodes[[i]], "col1"))
            col2 <- as.integer(xml2::xml_attr(sym_nodes[[i]], "col2"))
            return(Lint(
              filename = source_file$filename,
              line_number = line1,
              column_number = col1,
              type = "warning",
              message = sprintf("'%s' does not exist in {%s}.", syms[[i]], pkgs[[i]]),
              line = source_file$lines[line1],
              ranges = list(c(col1, col2)),
              linter = "namespace_linter"
            ))
          }
        }
      } else {
        line1 <- as.integer(xml2::xml_attr(pkg_nodes[[i]], "line1"))
        col1 <- as.integer(xml2::xml_attr(pkg_nodes[[i]], "col1"))
        col2 <- as.integer(xml2::xml_attr(pkg_nodes[[i]], "col2"))
        return(Lint(
          filename = source_file$filename,
          line_number = line1,
          column_number = col1,
          type = "warning",
          message = sprintf("Package '%s' is not installed.", pkgs[[i]]),
          line = source_file$lines[line1],
          ranges = list(c(col1, col2)),
          linter = "namespace_linter"
        ))
      }
    })

    results[!vapply(results, is.null, logical(1L))]
  }
}
